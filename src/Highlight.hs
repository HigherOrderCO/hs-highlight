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
import qualified Data.Vector as V

-- | Pre-computed line information for efficient position lookups
data LineInfo = LineInfo
    { lineOffsets :: !(V.Vector Int)  -- Starting position of each line
    , totalLines  :: !Int
    } deriving (Show)

-- | Build line information by scanning the text once
buildLineInfo :: T.Text -> LineInfo
buildLineInfo txt = LineInfo (V.fromList offsets) (length offsets)
  where
    offsets = 0 : go 0
    go !idx
        | idx >= T.length txt = []
        | T.index txt idx == '\n' = (idx + 1) : go (idx + 1)
        | otherwise = go (idx + 1)

-- | Convert a (line, column) pair (both 1-based) to a 0-based character index.
-- Much faster version using pre-computed line offsets.
posToIndexFast :: LineInfo -> (Int, Int) -> Int
posToIndexFast (LineInfo offsets _) (line, col) =
    if line <= 0 || line > V.length offsets
    then 0
    else (offsets V.! (line - 1)) + (col - 1)

-- | Convert a 0-based character index back to a (line, column) pair (1-based).
-- Much faster version using binary search on pre-computed line offsets.
indexToPosFast :: LineInfo -> Int -> (Int, Int)
indexToPosFast (LineInfo offsets _) idx =
    let line = binarySearch 0 (V.length offsets - 1)
    in (line + 1, idx - (offsets V.! line) + 1)
  where
    binarySearch !low !high
        | low >= high = low
        | otherwise =
            let !mid = (low + high + 1) `div` 2
                !midOffset = offsets V.! mid
            in if idx < midOffset
               then binarySearch low (mid - 1)
               else binarySearch mid high

-- | Highlight errors with red colour and underline.
highlightError :: (Int, Int) -- ^ Start position (line, column)
               -> (Int, Int) -- ^ End   position (line, column) – exclusive
               -> String     -- ^ File contents
               -> String
highlightError sPos ePos fileStr =
    let !file = T.pack fileStr
        !lineInfo = buildLineInfo file
        (trimSPos, trimEPos) = trimRegion' lineInfo sPos ePos file
        colour               = getColor "red"
    in  if trimSPos == trimEPos
          then
            let !startIdx = posToIndexFast lineInfo sPos
                prevCharIdx = findLastNonSpace (T.take startIdx file)
            in if prevCharIdx /= -1
                then let prevCharSPos = indexToPosFast lineInfo prevCharIdx
                         prevCharEPos = indexToPosFast lineInfo (prevCharIdx + 1)
                     in highlight'' lineInfo prevCharSPos prevCharEPos colour underline file
                else fileStr -- Nothing to highlight, return original text.
          else highlight'' lineInfo trimSPos trimEPos colour underline file
  where
    findLastNonSpace txt = snd $ T.foldl' go (0, -1) txt
      where go (!idx, !maxidx) c = (idx + 1, if not (isSpace c) then idx else maxidx)

-- | Trim leading and trailing whitespace (spaces, tabs, new-lines, etc.) from
--   the given range.  The returned end position is again /exclusive/.
trimRegion'
    :: LineInfo
    -> (Int, Int)             -- ^ Start position (line, column)
    -> (Int, Int)             -- ^ End   position (line, column) – exclusive
    -> T.Text                 -- ^ Whole source text
    -> ((Int, Int), (Int, Int))
trimRegion' lineInfo sPos ePos src =
    let !startIdx = posToIndexFast lineInfo sPos
        !endIdx   = posToIndexFast lineInfo ePos
        !sub      = T.drop startIdx (T.take endIdx src)
        !leading  = T.length (T.takeWhile isSpace sub)
        !newStartIdx = startIdx + leading
        !remaining = T.drop leading sub
        !trailing = T.length (T.takeWhile isSpace (T.reverse remaining))
        !newEndIdx = newStartIdx + (T.length remaining - trailing)
    in if newStartIdx >= newEndIdx
          then (sPos, sPos)  -- selection contained only whitespace
          else ( indexToPosFast lineInfo newStartIdx
               , indexToPosFast lineInfo newEndIdx
               )

-- | Highlight text in the given range.
highlight :: (Int, Int)        -- ^ Start position (line, column)
          -> (Int, Int)        -- ^ End   position (line, column) – exclusive
          -> String            -- ^ ANSI colour code
          -> (String -> String)
          -> String            -- ^ File contents
          -> String
highlight sPos ePos colour format fileStr =
  let !file = T.pack fileStr
      !lineInfo = buildLineInfo file
  in highlight'' lineInfo sPos ePos colour format file

highlight'' :: LineInfo
            -> (Int, Int)        -- ^ Start position (line, column)
            -> (Int, Int)        -- ^ End   position (line, column) – exclusive
            -> String            -- ^ ANSI colour code
            -> (String -> String)
            -> T.Text            -- ^ File contents as Text
            -> String
highlight'' lineInfo sPos@(sLine, sCol) ePos@(eLine, eCol) colour format file =
    -- Assert that the range is valid
    assert (isInBounds sPos ePos)
           "Start position must be before or equal to end position" $

    let -- Extract only the relevant substring for the lines sLine to eLine
        !startIdx = posToIndexFast lineInfo (sLine, 1)
        !nextLine = eLine + 1
        !endIdx   = if nextLine > totalLines lineInfo
                    then T.length file
                    else posToIndexFast lineInfo (nextLine, 1)
        -- Safety: ensure indices are sane
        !subLen   = max 0 (endIdx - startIdx)
        !sub      = T.take subLen (T.drop startIdx file)
        relevantLines = T.lines sub

        -- Length of the number of lines for padding
        !numLen = length (show eLine)

        -- Recursive function to process each line (using Text for efficiency)
        highlightLines :: [T.Text] -> Int -> String
        highlightLines [] _ = ""  -- Base case: no more lines to process
        highlightLines (line : rest) num
            | num > eLine = ""  -- Stop processing if past the end line
            | otherwise  =
                let -- Determine the start and end columns for highlighting
                    !targetStartCol = if num == sLine then sCol - 1 else 0
                    !adjustedCol    = if num == eLine then eCol - 1 else T.length line
                    !targetEndCol   = min adjustedCol (T.length line)

                    reset = getColor "reset"

                    -- Split the line into before, highlight, and after parts
                    !before = T.unpack $ T.take targetStartCol line
                    !target = T.unpack $ T.take (targetEndCol - targetStartCol) (T.drop targetStartCol line)
                    !after  = T.unpack $ T.drop targetEndCol line

                    -- Apply formatting function to the highlighted part
                    !formattedTarget = format target

                    -- Format the line with number, highlighted part, and colour codes
                    !numStr = pad numLen (show num)
                    !highlightedLine
                      | null target = numStr ++ " | " ++ before ++ after ++ "\n"
                      | otherwise   = numStr ++ " | "
                                     ++ before ++ colour ++ formattedTarget
                                     ++ reset  ++ after ++ "\n"
                in highlightedLine ++ highlightLines rest (num + 1)  -- Recursively process remaining lines

    -- Start highlighting from sLine
    in highlightLines relevantLines sLine

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

