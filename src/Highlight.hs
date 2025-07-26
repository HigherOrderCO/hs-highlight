{-# LANGUAGE BangPatterns #-}

module Highlight
    ( highlightError
    , highlight
    , underline
    , bold
    , italic
    , parenthesize
    , strikethrough
    , inverse
    , getColor
    , posToIndex
    , indexToPos
    ) where

import Data.Char (isSpace)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

-- Cache for line start positions
data LineCache = LineCache
    { lineStarts :: !(VU.Vector Int)
    , textLength :: !Int
    }

-- Build line cache by scanning text once (CORRECT AND EFFICIENT VERSION)
buildLineCache :: T.Text -> LineCache
buildLineCache txt = LineCache (VU.fromList . reverse $ starts) (T.length txt)
  where
    -- Use a strict fold to find all newline indices in a single pass.
    -- We accumulate the indices and the current position.
    (_, starts) = T.foldl'
                    (\(!idx, acc) char ->
                       let !nextIdx = idx + 1
                       in if char == '\n'
                          then (nextIdx, nextIdx : acc) -- If newline, record the start of the next line
                          else (nextIdx, acc))
                    (0, [0]) -- Start at index 0, with the first line starting at 0.
                    txt

-- Convert position to index using line cache
posToIndexWithCache :: LineCache -> (Int, Int) -> Int
posToIndexWithCache (LineCache starts len) (line, col)
  | line <= 0 = 0
  | line > VU.length starts = len
  | otherwise = (starts VU.! (line - 1)) + (col - 1)

-- Convert index to position using line cache
indexToPosWithCache :: LineCache -> Int -> (Int, Int)
indexToPosWithCache (LineCache starts _) idx =
    let line = binarySearch 0 (VU.length starts - 1)
    in (line + 1, idx - (starts VU.! line) + 1)
  where
    binarySearch !low !high
      | low >= high = low
      | otherwise =
          let !mid = (low + high + 1) `div` 2
              !midVal = starts VU.! mid
          in if idx < midVal
             then binarySearch low (mid - 1)
             else binarySearch mid high

-- | Highlight errors with red colour and underline.
highlightError :: (Int, Int) -- ^ Start position (line, column)
               -> (Int, Int) -- ^ End   position (line, column) – exclusive
               -> String     -- ^ File contents
               -> String
highlightError sPos ePos fileStr =
    let !file = T.pack fileStr
        !cache = buildLineCache file
        (trimSPos, trimEPos) = trimRegionFast cache sPos ePos file
        colour = getColor "red"
    in  if trimSPos == trimEPos
          then
            let !startIdx = posToIndexWithCache cache sPos
                prevCharIdx = findLastNonSpace (T.take startIdx file)
            in if prevCharIdx /= -1
                then let prevCharSPos = indexToPosWithCache cache prevCharIdx
                         prevCharEPos = indexToPosWithCache cache (prevCharIdx + 1)
                     in highlightFast cache prevCharSPos prevCharEPos colour underline file
                else fileStr -- Nothing to highlight, return original text.
          else highlightFast cache trimSPos trimEPos colour underline file
  where
    findLastNonSpace txt = snd $ T.foldl' go (0, -1) txt
      where go (!idx, !maxidx) c = (idx + 1, if not (isSpace c) then idx else maxidx)

-- Fast trimRegion using cache
trimRegionFast :: LineCache -> (Int, Int) -> (Int, Int) -> T.Text -> ((Int, Int), (Int, Int))
trimRegionFast cache sPos ePos src =
    let !startIdx = posToIndexWithCache cache sPos
        !endIdx = posToIndexWithCache cache ePos
        !sub = T.drop startIdx (T.take endIdx src)
        !leading = T.length (T.takeWhile isSpace sub)
        !newStartIdx = startIdx + leading
        !remaining = T.drop leading sub
        !trailing = T.length (T.takeWhile isSpace (T.reverse remaining))
        !newEndIdx = newStartIdx + (T.length remaining - trailing)
    in if newStartIdx >= newEndIdx
          then (sPos, sPos)
          else (indexToPosWithCache cache newStartIdx,
                indexToPosWithCache cache newEndIdx)

-- Fast highlight using cache (OPTIMIZED VERSION)
highlightFast :: LineCache -> (Int, Int) -> (Int, Int) -> String -> (String -> String) -> T.Text -> String
highlightFast cache sPos@(sLine, sCol) ePos@(eLine, eCol) colour format file =
    assert (isInBounds sPos ePos)
           "Start position must be before or equal to end position" $

    let !numLen = length (show eLine)
        !lineStartsVec = lineStarts cache
        !totalLen = textLength cache
        reset = getColor "reset"

        -- Efficiently process a single line using indices from the cache
        processLine :: Int -> String
        processLine num =
            let -- Get line start and end indices from the cache
                !lineStartIdx = lineStartsVec VU.! (num - 1)
                !lineEndIdx = if num < VU.length lineStartsVec
                                 then lineStartsVec VU.! num
                                 else totalLen

                -- Extract the line text, stripping the trailing newline.
                !line = T.stripEnd $ T.take (lineEndIdx - lineStartIdx) (T.drop lineStartIdx file)

                -- Determine the highlighting columns for this specific line
                !targetStartCol = if num == sLine then sCol - 1 else 0
                !targetEndCol   = if num == eLine then eCol - 1 else T.length line

                -- Split the line into three parts: before, target, and after
                (!before, !rest)  = T.splitAt targetStartCol line
                (!target, !after) = T.splitAt (targetEndCol - targetStartCol) rest

                !beforeStr = T.unpack before
                !targetStr = T.unpack target
                !afterStr  = T.unpack after

                !formattedTarget = format targetStr
                !numStr = pad numLen (show num)

            in if null targetStr
               then numStr ++ " | " ++ beforeStr ++ afterStr
               else numStr ++ " | " ++ beforeStr ++ colour ++ formattedTarget ++ reset ++ afterStr

    in unlines $ map processLine [sLine..eLine]

-- | Trim region - public API maintained for compatibility
trimRegion :: (Int, Int) -> (Int, Int) -> T.Text -> ((Int, Int), (Int, Int))
trimRegion sPos ePos src =
    let !cache = buildLineCache src
    in trimRegionFast cache sPos ePos src

-- | Convert position to index - public API maintained for compatibility
posToIndex :: T.Text -> (Int, Int) -> Int
posToIndex src pos =
    let !cache = buildLineCache src
    in posToIndexWithCache cache pos

-- | Convert index to position - public API maintained for compatibility
indexToPos :: T.Text -> Int -> (Int, Int)
indexToPos src idx =
    let !cache = buildLineCache src
    in indexToPosWithCache cache idx

-- | Highlight text in the given range.
highlight :: (Int, Int)        -- ^ Start position (line, column)
          -> (Int, Int)        -- ^ End   position (line, column) – exclusive
          -> String            -- ^ ANSI colour code
          -> (String -> String)
          -> String            -- ^ File contents
          -> String
highlight sPos ePos colour format fileStr =
  highlight' sPos ePos colour format (T.pack fileStr)

-- | Internal highlight with Text
highlight' :: (Int, Int) -> (Int, Int) -> String -> (String -> String) -> T.Text -> String
highlight' s_pos e_pos colour format file =
    let !cache = buildLineCache file
    in highlightFast cache s_pos e_pos colour format file

-- | Pads a string with spaces to the left.
pad :: Int    -- ^ Desired length
    -> String -- ^ String to pad
    -> String -- ^ Padded string
pad len txt = replicate (max (len - length txt) 0) ' ' ++ txt

-- | Checks if the start position is before or equal to the end position.
isInBounds :: (Int, Int) -> (Int, Int) -> Bool
isInBounds (sLine, sCol) (eLine, eCol) =
    sLine < eLine || (sLine == eLine && sCol <= eCol)

-- | Simple assertion function.
assert :: Bool   -- ^ Condition to assert
       -> String -- ^ Error message if assertion fails
       -> a      -- ^ Value to return if assertion passes
       -> a
assert True _ x    = x
assert False msg _ = error msg

-- | Gets the ANSI colour code for a given colour name.
getColor :: String -- ^ Colour name
         -> String -- ^ ANSI colour code
getColor colour = case colour of
    "red"     -> "\x1b[31m"
    "green"   -> "\x1b[32m"
    "yellow"  -> "\x1b[33m"
    "blue"    -> "\x1b[34m"
    "magenta" -> "\x1b[35m"
    "cyan"    -> "\x1b[36m"
    "white"   -> "\x1b[37m"
    "reset"   -> "\x1b[0m"
    _         -> "\x1b[0m"  -- defaults to reset

-- | Applies underline formatting to text using ANSI escape codes.
underline :: String -- ^ Text to underline
          -> String
underline text = "\x1b[4m" ++ text ++ "\x1b[24m"

-- | Applies bold formatting to text using ANSI escape codes.
bold :: String -- ^ Text to make bold
     -> String
bold text = "\x1b[1m" ++ text ++ "\x1b[22m"

-- | Applies italic formatting to text using ANSI escape codes.
italic :: String -- ^ Text to italicise
       -> String
italic text = "\x1b[3m" ++ text ++ "\x1b[23m"

-- | Wraps text in parentheses.
parenthesize :: String -- ^ Text to parenthesise
             -> String
parenthesize text = "(" ++ text ++ ")"

-- | Applies strikethrough formatting to text using ANSI escape codes.
strikethrough :: String -- ^ Text to strikethrough
              -> String
strikethrough text = "\x1b[9m" ++ text ++ "\x1b[29m"

-- | Applies inverse (reverse video) formatting to text using ANSI escape codes.
inverse :: String -- ^ Text to inverse
        -> String
inverse text = "\x1b[7m" ++ text ++ "\x1b[27m"
