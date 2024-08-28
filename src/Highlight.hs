{-|
Module      : Highlight
Description : A module for highlighting and formatting text in terminal output
Copyright   : (c) Lorenzobattistela, 2024
License     : MIT
Maintainer  : lorenzobattistela@gmail.com
Stability   : experimental

This module provides functions to highlight and format text in terminal output,
particularly useful for displaying code snippets with error highlighting.
-}
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

-- | Highlights an error in the given content between the specified start and end positions.
highlightError :: (Int, Int) -> (Int, Int) -> String -> String
highlightError (sLine, sCol) (eLine, eCol) content =
  highlight (sLine, sCol) (eLine, eCol) "red" underline content

-- | Highlights a portion of the given content between the specified start and end positions with a specified color and effect.
highlight :: (Int, Int) -> (Int, Int) -> String -> (String -> String) -> String -> String
highlight sPos@(sLine, sCol) ePos@(eLine, eCol) color effect content =
    assert (isInBounds sPos ePos) "Start position must be before or equal to end position" $

    let (lineIndices, lineNumbers) = targetLines content sLine eLine
        displayText = buildDisplayText lineIndices lineNumbers sPos ePos color effect
    in displayText

-- | Extracts the target lines and their indices from the content based on the start and end line numbers.
targetLines :: String -> Int -> Int -> ([(Int, String)], [Int])
targetLines content sLine eLine =
    let
        -- Pair each line with its line number, starting from 1
        numberedLines = zip [1..] $ lines content
        
        -- Extract only the lines between startLine and endLine
        intervalLines = takeWhile (\(lineNum, _) -> lineNum <= eLine) $ 
                        dropWhile (\(lineNum, _) -> lineNum < sLine) numberedLines
        
        -- Calculate cumulative character indices for each target line
        -- Example: "Hello\nWorld" will return [0, 6] because 0 has 5 characters + 1 for newline
        indices = scanl (\accIndex (_, line) -> accIndex + length line + 1) 0 intervalLines
        -- Extract line numbers from target lines to a list
        numbers = map fst intervalLines
        -- Extract line contents from target lines to a list
        contents = map snd intervalLines
        -- Pair each line index with its corresponding line content
        -- Example: "Hello\nWorld" would return: [(0, "Hello"), (6, "World")]
        indexedLines = zip indices contents
      -- Returns (a list of)  a tuple containing the indexed lines above along with their respective line numbers
     in (indexedLines, numbers)

-- | Builds the display text with highlighting applied to the specified portion of the content.
buildDisplayText :: [(Int, String)] -> [Int] -> (Int, Int) -> (Int, Int) -> String -> (String -> String) -> String
buildDisplayText indices numbers (sLine, sCol) (eLine, eCol) colorStr effect =
    -- Calculate the maximum line number to pad the pipe
    let maxNumLineWidth = length $ show $ maximum numbers
        -- add padding to the number + pipe line
        formatLineNum n = pad maxNumLineWidth (show n) ++ " | "
        color = getColor colorStr
        reset = "\x1b[0m"

        -- helper to determine start/end column index of highlighting
        col num line col fallback = if num == line then col - 1 else fallback 

        highlightLine (start, line) num =
              -- Split the line at the starting column for highlighting (0-based index).
          let (before, rest)  = splitAt (col num sLine sCol 0) line
              -- Split the remaining part of the line at the ending column for highlighting (0-based index), 
              -- adjusted by subtracting the length of the `before` segment to correctly identify the `target` segment.
              (target, after) = splitAt (col num eLine eCol (length line) - length before) rest
              -- apply color and effects to target text
              highlighted = color ++ effect target ++ reset
          in formatLineNum num ++ before ++ highlighted ++ after

    -- zipWith calls highlightLine for every element of zipped indices and numbers, resulting in a list of strings with the highlighted ones
    -- unlines concate them with newlines, returning a highlighted string
    in unlines $ zipWith highlightLine indices numbers

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
assert True _ x = x
assert False msg _ = error msg

-- | Gets the ANSI color code for a given color name.
getColor :: String -- ^ Color name
         -> String -- ^ ANSI color code
getColor color = case color of
    "red"     -> "\x1b[31m"
    "green"   -> "\x1b[32m"
    "yellow"  -> "\x1b[33m"
    "blue"    -> "\x1b[34m"
    "magenta" -> "\x1b[35m"
    "cyan"    -> "\x1b[36m"
    "white"   -> "\x1b[37m"
    _         -> "\x1b[0m"  -- defaults to reset

-- | Applies underline formatting to text using ANSI escape codes.
underline :: String -- ^ Text to underline
          -> String -- ^ Underlined text
underline text = "\x1b[4m" ++ text ++ "\x1b[24m"

-- | Applies bold formatting to text using ANSI escape codes.
bold :: String -- ^ Text to make bold
     -> String -- ^ Bold text
bold text = "\x1b[1m" ++ text ++ "\x1b[22m"

-- | Applies italic formatting to text using ANSI escape codes.
italic :: String -- ^ Text to italicize
       -> String -- ^ Italicized text
italic text = "\x1b[3m" ++ text ++ "\x1b[23m"

-- | Wraps text in parentheses.
parenthesize :: String -- ^ Text to parenthesize
             -> String -- ^ Parenthesized text
parenthesize text = "(" ++ text ++ ")"

-- | Applies strikethrough formatting to text using ANSI escape codes.
strikethrough :: String -- ^ Text to strikethrough
              -> String -- ^ Strikethrough text
strikethrough text = "\x1b[9m" ++ text ++ "\x1b[29m"

-- | Applies inverse (reverse video) formatting to text using ANSI escape codes.
inverse :: String -- ^ Text to inverse
        -> String -- ^ Inversed text
inverse text = "\x1b[7m" ++ text ++ "\x1b[27m"
