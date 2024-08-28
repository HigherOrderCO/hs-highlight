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

-- | Highlight errors with red color and underline.
-- 
-- This function highlights text in the specified range with red color and underline.
-- 
-- * `sLine`, `sCol`: Starting line and column position.
-- * `eLine`, `eCol`: Ending line and column position.
-- * `file`: The text content to process.
-- 
-- Returns the highlighted text.
highlightError :: (Int, Int) -> (Int, Int) -> String -> String
highlightError (sLine, sCol) (eLine, eCol) file =
    let color = getColor "red"
    in highlight (sLine, sCol) (eLine, eCol) color underline file

-- | Highlight text in the given range.
-- 
-- This function highlights text within the specified range with the provided color and formatting function.
-- 
-- * `sPos`: Starting position as a tuple (line, column).
-- * `ePos`: Ending position as a tuple (line, column).
-- * `color`: The color code to apply for highlighting.
-- * `format`: A function to format the highlighted text.
-- * `file`: The text content to process.
-- 
-- Returns the highlighted text.
highlight :: (Int, Int) -> (Int, Int) -> String -> (String -> String) -> String -> String
highlight sPos@(sLine, sCol) ePos@(eLine, eCol) color format file =
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
            | otherwise =
                let -- Determine the start and end columns for highlighting
                    targetStartCol = if num == sLine then sCol - 1 else 0
                    adjustedCol = if num == eLine then eCol - 1 else length line
                    targetEndCol = min adjustedCol (length line)

                    reset = getColor "reset"
                    
                    -- Split the line into before, highlight, and after parts
                    (before, restLine) = splitAt targetStartCol line
                    (target, after) = splitAt (targetEndCol - targetStartCol) restLine

                    -- Apply formatting function to the highlighted part
                    formattedTarget = format target

                    -- Format the line with number, highlighted part, and color codes
                    numStr = pad numLen (show num)
                    highlightedLine =
                      if null target
                      then numStr ++ " | " ++ before ++ after ++ "\n"                     
                      else numStr ++ " | " ++ before ++ color ++ formattedTarget ++ reset ++ after ++ "\n"
                in highlightedLine ++ highlightLines rest (num + 1)  -- Recursively process the remaining lines

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
    "reset"   -> "\x1b[0m"
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
