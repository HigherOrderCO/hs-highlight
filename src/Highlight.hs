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

-- | Highlights an error in the given text using red underline.
highlightError :: (Int, Int) -- ^ Start position (line, column)
               -> (Int, Int) -- ^ End position (line, column)
               -> String     -- ^ The text content
               -> String     -- ^ The highlighted text
highlightError (startLine, startCol) (endLine, endCol) content =
  highlight (startLine, startCol) (endLine, endCol) "red" content underline

-- | Highlights a portion of text with a specified color and effect.
highlight :: (Int, Int)            -- ^ Start position (line, column)
          -> (Int, Int)            -- ^ End position (line, column)
          -> String                -- ^ Color name
          -> String                -- ^ The text content
          -> (String -> String)    -- ^ Effect function
          -> String                -- ^ The highlighted text
highlight (startLine, startCol) (endLine, endCol) color content effect =
    assert (startLine <= endLine && (startLine /= endLine || startCol <= endCol)) "Start position must be before or equal to end position" $
    let (lineIndices, lineNumbers) = calculateIndicesAndLineNumbers content (startLine, startCol) (endLine, endCol)
        displayText = buildDisplayText lineIndices lineNumbers (startLine, startCol) (endLine, endCol) color effect
    in displayText

-- | Calculates indices and line numbers for the relevant portion of text.
calculateIndicesAndLineNumbers :: String -> (Int, Int) -> (Int, Int) -> ([(Int, String)], [Int])
calculateIndicesAndLineNumbers content (startLine, startCol) (endLine, endCol) =
    let linesWithNumbers = zip [1..] $ lines content
        relevantLines = takeWhile (\(n, _) -> n <= endLine) $ dropWhile (\(n, _) -> n < startLine) linesWithNumbers
        lineIndices = scanl (\acc (_, l) -> acc + length l + 1) 0 relevantLines
        lineNumbers = map fst relevantLines
    in (zip lineIndices (map snd relevantLines), lineNumbers)

-- | Builds the display text with line numbers and highlighting.
buildDisplayText :: [(Int, String)] -> [Int] -> (Int, Int) -> (Int, Int) -> String -> (String -> String) -> String
buildDisplayText lineIndices lineNumbers (startLine, startCol) (endLine, endCol) colorStr effect =
    let maxLineNumWidth = length $ show $ maximum lineNumbers
        formatLineNum n = pad maxLineNumWidth (show n)
        
        highlightLine :: Int -> Int -> String -> String
        highlightLine lineNum lineStart line =
            let lineEnd = if lineNum == endLine then endCol - 1 else length line
                startCol' = if lineNum == startLine then startCol - 1 else 0
                (before, highlight) = splitAt startCol' line
                (toHighlight, after) = splitAt (lineEnd - startCol') highlight
            in formatLineNum lineNum ++ " | " ++ before ++ color ++ (effect toHighlight) ++ reset ++ after
        color = getColor colorStr
        reset = "\x1b[0m"
    in unlines $ zipWith3 highlightLine lineNumbers (map fst lineIndices) (map snd lineIndices)

-- | Pads a string with spaces to the left.
pad :: Int    -- ^ Desired length
    -> String -- ^ String to pad
    -> String -- ^ Padded string
pad len txt = replicate (max (len - length txt) 0) ' ' ++ txt

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
