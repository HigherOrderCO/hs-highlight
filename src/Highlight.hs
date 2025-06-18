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
import Debug.Trace

-- | Highlight errors with red colour and underline.
--   Leading and trailing whitespace in the supplied range is automatically
--   removed so that only meaningful characters are highlighted.
highlightError :: (Int, Int) -- ^ Start position (line, column)
               -> (Int, Int) -- ^ End   position (line, column) – exclusive
               -> String     -- ^ File contents
               -> String
highlightError sPos ePos file =
    let (trimSPos, trimEPos) = trimRegion sPos ePos file
        colour               = getColor "red"
    in  if trimSPos == trimEPos
          then file  -- nothing but whitespace was selected → keep original text
          else highlight trimSPos trimEPos colour underline file

-- | Trim leading and trailing whitespace (spaces, tabs, new-lines, etc.) from
--   the given range.  The returned end position is again /exclusive/.
trimRegion
    :: (Int, Int)             -- ^ Start position (line, column)
    -> (Int, Int)             -- ^ End   position (line, column) – exclusive
    -> String                 -- ^ Whole source text
    -> ((Int, Int), (Int, Int))
trimRegion sPos ePos src =
    let startIdx = posToIndex src sPos
        endIdx0  = posToIndex src ePos
        endIdx   = min endIdx0 (length src)

        -- move start forwards past whitespace
        forward i
          | i >= endIdx              = endIdx
          | not (isSpace (src !! i)) = i
          | otherwise                = forward (i + 1)

        -- move end backwards past whitespace
        backward i
          | i < startIdx             = startIdx - 1
          | not (isSpace (src !! i)) = i
          | otherwise                = backward (i - 1)

        newStartIdx = forward startIdx
        newEndIdx   = backward (endIdx - 1) + 1   -- keep end exclusive
    in if newStartIdx >= newEndIdx
          then (sPos, sPos)  -- selection contained only whitespace
          else ( indexToPos src newStartIdx
               , indexToPos src newEndIdx
               )

-- | Convert a (line, column) pair (both 1-based) to a 0-based character index.
posToIndex :: String -> (Int, Int) -> Int
posToIndex src (tLine, tCol) = go 0 1 1 src
  where
    go idx line col [] = idx
    go idx line col (c:cs)
        | line == tLine && col == tCol = idx
        | otherwise =
            let (line', col') =
                  if c == '\n' then (line + 1, 1) else (line, col + 1)
            in go (idx + 1) line' col' cs

-- | Convert a 0-based character index back to a (line, column) pair (1-based).
indexToPos :: String -> Int -> (Int, Int)
indexToPos src targetIdx = go 0 1 1 src
  where
    go idx line col [] = (line, col)
    go idx line col (c:cs)
        | idx == targetIdx = (line, col)
        | otherwise =
            let (line', col') =
                  if c == '\n' then (line + 1, 1) else (line, col + 1)
            in go (idx + 1) line' col' cs

-- | Highlight text in the given range.
--
-- This function highlights text within the specified range with the provided
-- colour and formatting function.
highlight :: (Int, Int)        -- ^ Start position (line, column)
          -> (Int, Int)        -- ^ End   position (line, column) – exclusive
          -> String            -- ^ ANSI colour code
          -> (String -> String)
          -> String            -- ^ File contents
          -> String
highlight sPos@(sLine, sCol) ePos@(eLine, eCol) colour format file =
    -- Assert that the range is valid
    assert (isInBounds sPos ePos)
           "Start position must be before or equal to end position" $

    let -- Split file into lines
        linesList = lines file

        -- Length of the number of lines for padding
        numLen = length (show eLine)

        -- Recursive function to process each line
        highlightLines :: [String] -> Int -> String
        highlightLines [] _ = ""  -- Base case: no more lines to process
        highlightLines (line : rest) num
            | num < sLine = highlightLines rest (num + 1)  -- Skip lines before the start line
            | num > eLine = ""  -- Stop processing if past the end line
            | otherwise  =
                let -- Determine the start and end columns for highlighting
                    targetStartCol = if num == sLine then sCol - 1 else 0
                    adjustedCol    = if num == eLine then eCol - 1 else length line
                    targetEndCol   = min adjustedCol (length line)

                    reset = getColor "reset"

                    -- Split the line into before, highlight, and after parts
                    (before, restLine) = splitAt targetStartCol line
                    (target, after)    = splitAt (targetEndCol - targetStartCol) restLine

                    -- Apply formatting function to the highlighted part
                    formattedTarget = format target

                    -- Format the line with number, highlighted part, and colour codes
                    numStr = pad numLen (show num)
                    highlightedLine
                      | null target = numStr ++ " | " ++ before ++ after ++ "\n"
                      | otherwise   = numStr ++ " | "
                                     ++ before ++ colour ++ formattedTarget
                                     ++ reset  ++ after ++ "\n"
                in highlightedLine ++ highlightLines rest (num + 1)  -- Recursively process remaining lines

    -- Start highlighting from line number 1
    in highlightLines linesList 1

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

