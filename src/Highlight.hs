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
    ) where

import Data.Char (isSpace)
import Data.List (foldl')
import qualified Data.Text as T

-- | Highlight errors with red colour and underline.
highlightError :: (Int, Int) -- ^ Start position (line, column)
               -> (Int, Int) -- ^ End   position (line, column) – exclusive
               -> String     -- ^ File contents
               -> String
highlightError sPos ePos fileStr =
    let !file = T.pack fileStr
        (trimSPos, trimEPos) = trimRegion sPos ePos file
        colour               = getColor "red"
    in  if trimSPos == trimEPos
          then
            let !startIdx = posToIndex file sPos
                prevCharIdx = findLastNonSpace (T.take startIdx file)
            in if prevCharIdx /= -1
                then let prevCharSPos = indexToPos file prevCharIdx
                         prevCharEPos = indexToPos file (prevCharIdx + 1)
                     in highlight' prevCharSPos prevCharEPos colour underline file
                else fileStr -- Nothing to highlight, return original text.
          else highlight' trimSPos trimEPos colour underline file
  where
    findLastNonSpace txt = snd $ T.foldl' go (0, -1) txt
      where go (!idx, !maxidx) c = (idx + 1, if not (isSpace c) then idx else maxidx)

-- | Trim leading and trailing whitespace (spaces, tabs, new-lines, etc.) from
--   the given range.  The returned end position is again /exclusive/.
trimRegion
    :: (Int, Int)             -- ^ Start position (line, column)
    -> (Int, Int)             -- ^ End   position (line, column) – exclusive
    -> T.Text                 -- ^ Whole source text
    -> ((Int, Int), (Int, Int))
trimRegion sPos ePos src =
    let !startIdx = posToIndex src sPos
        !endIdx   = posToIndex src ePos
        !sub      = T.drop startIdx (T.take endIdx src)
        !leading  = T.length (T.takeWhile isSpace sub)
        !newStartIdx = startIdx + leading
        !remaining = T.drop leading sub
        !trailing = T.length (T.takeWhile isSpace (T.reverse remaining))
        !newEndIdx = newStartIdx + (T.length remaining - trailing)
    in if newStartIdx >= newEndIdx
          then (sPos, sPos)  -- selection contained only whitespace
          else ( indexToPos src newStartIdx
               , indexToPos src newEndIdx
               )

-- | Convert a (line, column) pair (both 1-based) to a 0-based character index.
-- OPTIMIZED: Stop at target line instead of scanning entire file
posToIndex :: T.Text -> (Int, Int) -> Int
posToIndex src (tLine, tCol) = go 0 1 1
  where
    !len = T.length src
    go !idx !line !col
      | line > tLine = idx - 1
      | line == tLine && col == tCol = idx
      | idx >= len = idx
      | otherwise =
          let !c = T.index src idx
              (!line', !col') = if c == '\n' then (line + 1, 1) else (line, col + 1)
          in go (idx + 1) line' col'

-- | Convert a 0-based character index back to a (line, column) pair (1-based).
indexToPos :: T.Text -> Int -> (Int, Int)
indexToPos src targetIdx = go 0 1 1
  where
    !len = T.length src
    go !idx !line !col
      | idx == targetIdx || idx >= len = (line, col)
      | otherwise =
          let !c = T.index src idx
              (!line', !col') = if c == '\n' then (line + 1, 1) else (line, col + 1)
          in go (idx + 1) line' col'

-- | Highlight text in the given range.
highlight :: (Int, Int)        -- ^ Start position (line, column)
          -> (Int, Int)        -- ^ End   position (line, column) – exclusive
          -> String            -- ^ ANSI colour code
          -> (String -> String)
          -> String            -- ^ File contents
          -> String
highlight sPos ePos colour format fileStr =
  highlight' sPos ePos colour format (T.pack fileStr)

-- | OPTIMIZED: Extract lines directly without converting positions
highlight' :: (Int, Int)        -- ^ Start position (line, column)
           -> (Int, Int)        -- ^ End   position (line, column) – exclusive
           -> String            -- ^ ANSI colour code
           -> (String -> String)
           -> T.Text            -- ^ File contents as Text
           -> String
highlight' sPos@(sLine, sCol) ePos@(eLine, eCol) colour format file =
    assert (isInBounds sPos ePos)
           "Start position must be before or equal to end position" $
    
    let -- Split into lines but keep track of where we are
        allLines = T.lines file
        -- Drop lines before start line, take only what we need
        relevantLines = take (eLine - sLine + 1) $ drop (sLine - 1) allLines
        
        -- Length of the number of lines for padding
        !numLen = length (show eLine)
        
        -- Process lines more efficiently
        processLine :: T.Text -> Int -> String
        processLine line num =
            let !targetStartCol = if num == sLine then sCol - 1 else 0
                !adjustedCol    = if num == eLine then eCol - 1 else T.length line
                !targetEndCol   = min adjustedCol (T.length line)
                
                reset = getColor "reset"
                
                -- Only unpack what we need
                (!before, !rest) = T.splitAt targetStartCol line
                (!target, !after) = T.splitAt (targetEndCol - targetStartCol) rest
                
                !beforeStr = T.unpack before
                !targetStr = T.unpack target
                !afterStr = T.unpack after
                
                !formattedTarget = format targetStr
                !numStr = pad numLen (show num)
                
            in if null targetStr
               then numStr ++ " | " ++ beforeStr ++ afterStr ++ "\n"
               else numStr ++ " | " ++ beforeStr ++ colour ++ formattedTarget ++ reset ++ afterStr ++ "\n"
        
        -- Process all relevant lines
        result = concat $ zipWith processLine relevantLines [sLine..eLine]
        
    in result

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
