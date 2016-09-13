{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Simplified Chinese <-> English dictionary with pinyin phonetics.
module Data.Chinese.CCDict
  ( initiate
  , Entry(..)
  , ppEntry
  , entryVariants
  , entryOriginal
  , entrySimplified
  , entryTraditional
  , entryWordFrequency
  , entryPinyin
  , Variant(..)
  , lookupMatch
  , lookupMatches
  ) where

import           Data.Char
import           Data.Maybe
import           Data.Ord
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Paths_cndict
import           Prelude                hiding (lookup)
import           System.IO.Unsafe       (unsafePerformIO)


import qualified Data.Text.Internal as T
import qualified Data.Text.Array as T
import qualified Data.Array.Unboxed as U
import qualified Data.Text.Read        as T
import Control.Exception (evaluate)

-- | Load DB into memory. Otherwise it happens when the DB
--   is first used.
-- 初始化整个Dict
initiate :: IO ()
initiate = do
  evaluate ccDict
  return ()

data CCDict = CCDict !T.Array !Int (U.UArray Int Int)
-- Text 是一个整块的内存区域
-- 通过内部的buffer，长度和偏移量
mkCCDict :: Text -> CCDict
mkCCDict text@(T.Text arr _ len) =
    CCDict arr len
      (U.listArray (0,n-1) offsets)
  where
    -- 将文件分行
    -- 按新行进行分割
    ls = T.lines text
    -- 得到相应的offset
    offsets =
      [ offset
      | T.Text _ offset _length <- ls ]
    -- 行数
    n = length ls

ccDict :: CCDict
ccDict = mkCCDict utfData
  where
    -- 加载数据
    utfData = unsafePerformIO $ do
      -- 通过cabal的data-files生成的函数，找到相应的文件路径
      path  <- getDataFileName "data/dict.sorted"
      -- 一次性的读入到内存中
      T.readFile path

ccDictNth :: Int -> CCDict -> Text
ccDictNth n (CCDict arr totalLen offsets) =
    T.text arr offset len
  where
    -- 取offsets的索引号的上界
    lastIdx = snd (U.bounds offsets)
    -- 取出第n个offset
    offset = offsets U.! n
    -- 计算dict中第n个词语的长度
    -- 如果n是最后一个词，那么它的长度应该为totalLen - offset - 1
    -- 如果n不是最后一个词，那么的长度应该是n+1个词的位移 - 自己的位移 - 1
    len
      | lastIdx == n = totalLen - offset - 1
      | otherwise    = offsets U.! (n+1) - offset - 1

bounds :: CCDict -> (Int, Int)
bounds (CCDict _ _ offsets) = U.bounds offsets
-- 前缀查找
findPrefix :: CCDict -> Int -> Int -> Int -> Text -> Maybe (Int,Int)
findPrefix dict maxUpper lower upper key
  -- 下界比上界还打
  | lower > upper = Nothing
  | otherwise =
    case compare (T.take len key) (T.take len val) of
      -- 比字典中的值小
      LT -> findPrefix dict (middle-1) lower (middle-1) key
      -- 比字典中的值大
      GT -> findPrefix dict maxUpper (middle+1) upper key
      -- 相等了
      EQ ->
        case compare (T.length key) (T.length val) of
          -- 长度比字典中的大
          GT -> findPrefix dict maxUpper (middle+1) upper key
          -- 剩下的情况，继续向下找
          -- fromMaybe 接收一个默认值和一个返回值
          -- 如果返回值是Nothing则返回默认值
          _ -> Just $ fromMaybe (middle, maxUpper) $
                  findPrefix dict maxUpper lower (middle-1) key
  where
    -- 二分查找
    middle = (upper - lower) `div` 2 + lower
    -- 从字典的中间拿出一个值
    val = T.takeWhile (/='\t') $ ccDictNth middle dict
    -- 得到最小的比较长度
    len = min (T.length val) (T.length key)

lookupMatches :: Text -> Maybe [Entry]
lookupMatches key
  | T.null key = Nothing
lookupMatches key =
    if null entries
      then Nothing
      else Just entries
  where
    -- 增长序列
    -- 得到所有keys
    keys = tail $ T.inits key
    entries = worker (bounds ccDict) keys
    worker _ [] = []
    worker (lower, upper) (k:ks) =
      case findPrefix ccDict upper lower upper k of
        Nothing -> []
        Just (first, newUpper) ->
          maybe id (:) (scrapeEntry ccDict first k) $
          worker (first, newUpper) ks

lookupMatch :: Text -> Maybe Entry
lookupMatch key
  | T.null key = Nothing
  | otherwise =
    case findPrefix ccDict upper lower upper key of
      Nothing -> Nothing
      Just (first, _newUpper) ->
        scrapeEntry ccDict first key
    where
      -- 得到字典的下界和上界
      (lower, upper) = bounds ccDict

allVariants :: [Variant]
allVariants = worker 0
  where
    worker nth | nth > snd (bounds ccDict) = []
    worker nth = parseVariant (ccDictNth nth ccDict) : worker (nth+1)

scrapeEntry :: CCDict -> Int -> Text -> Maybe Entry
scrapeEntry dict nth key =
    case variants of
      [] -> Nothing
      (v:vs) -> Just (Entry key v vs)
  where
    variants = scrapeVariants dict nth key

-- 得到所有可能的变形    
-- 如果 n > 字典的上界那么应该返回空列表
-- 如果 字典中第n个词语去除掉tab键 
scrapeVariants :: CCDict -> Int -> Text -> [Variant]
scrapeVariants dict nth key
  | nth > snd (bounds dict) = []
  | T.takeWhile (/='\t') raw == key =
      -- 拼接n+1的单词
      parseVariant raw : scrapeVariants dict (nth+1) key
    -- 此处会跳出，不用担心遍历了整个词典    
  | otherwise = []
    where
      raw = ccDictNth nth dict

parseVariant :: Text -> Variant
parseVariant line =
  --  按照tab键进行分割
  case T.splitOn "\t" line of
    -- 中文，词频，拼音，英文
    [chinese, count, pinyin, english] ->
      mkVariant chinese chinese count pinyin english
    -- 有简体和繁体  
    [traditional, simplified, "T", count, pinyin, english] ->
      mkVariant traditional simplified count pinyin english
    -- 同样是由简体和繁体
    -- 简体主导
    [simplified, traditional, "S", count, pinyin, english] ->
      mkVariant traditional simplified count pinyin english
    _ -> error $ "invalid variant: " ++ T.unpack line
  where
    mkVariant traditional simplified countStr pinyin english = Variant
      { variantTraditional = traditional
      , variantSimplified = simplified
      , variantWordFrequency = count
      , variantPinyin = pinyin
      , variantDefinitions = splitDefinition english }
      where
        Right (count,_) = T.decimal countStr

--freqLookup_ :: FreqMap -> Text -> Maybe Int
--freqLookup_ freq key = worker (bounds freq)
--  where
--    worker (lower, upper)
--      | lower > upper = Nothing
--    worker (lower, upper) =
--      let middle = (upper - lower) `div` 2 + lower
--          val = freqNth middle freq
--          [word, countStr] = T.words val
--          Right (count,_) = T.decimal countStr
--      in case compare key word of
--        LT -> worker (lower, middle-1)
--        GT -> worker (middle+1, upper)
--        EQ -> Just count

--------------------------------------------------
-- Dictionary


-- | Dictionary entry
--data Entry = Entry
--  { entrySimplified  :: !Text
--  , entryTraditional :: !Text
--  , entryPinyin      :: [Text]
--  , entryDefinition  :: [[Text]]
--  } deriving ( Read, Show, Eq, Ord )

data Entry = Entry !Text Variant [Variant]
  deriving (Show, Read, Eq, Ord)
data Variant = Variant
  { variantSimplified    :: !Text
  , variantTraditional   :: !Text
  , variantWordFrequency :: !Int
  , variantPinyin        :: !Text
  , variantDefinitions   :: [Text]
  } deriving ( Read, Show, Eq, Ord )

entryOriginal :: Entry -> Text
entryOriginal (Entry o _v _vs) = o

entryVariants :: Entry -> [Variant]
entryVariants (Entry _o v vs) = v:vs

dominantVariant :: Entry -> Variant
dominantVariant (Entry _o v vs) =
    foldr dom v vs
  where
    -- 比较两个词的，词频
    -- 如果 v2的词频大于v1的时候，那么将v2返回
    dom v1 v2
      | variantWordFrequency v1 < variantWordFrequency v2 = v2
      | otherwise = v1

entrySimplified :: Entry -> Text
entrySimplified = variantSimplified . dominantVariant

entryTraditional :: Entry -> Text
entryTraditional = variantTraditional . dominantVariant

entryWordFrequency :: Entry -> Int
entryWordFrequency = variantWordFrequency . dominantVariant

entryPinyin :: Entry -> [Text]
entryPinyin = map variantPinyin . entryVariants

ppEntry :: Entry -> Text
ppEntry = T.intercalate "\n" . map ppVariant . entryVariants

ppVariant :: Variant -> Text
ppVariant (Variant simplified traditional frequency pinyin english) =
  T.intercalate "\t"
    [simplified, traditional, count, pinyin, english']
  where
    count = T.pack $ show frequency
    english' = T.intercalate "/" english


-- /first/second/third/ -> [first, second, third]
splitDefinition :: Text -> [Text]
splitDefinition = filter (not . T.null) . T.splitOn "/" . T.dropAround isSpace
