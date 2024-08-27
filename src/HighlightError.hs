module HighlightError (highlightError, toByteIndex, highlight, getColor) where

import Data.Char (ord)
import Data.List (foldl')
import Text.Printf (printf)

-- Given a complete source file, highlights a section between two indexes (in bytes) with the specified color.
highlight :: Int -> Int -> String -> (String -> String) -> String -> String
highlight iniIdx endIdx colorStr effectFn file = 
    -- Makes sure the end index is lower than the end index
    assert (iniIdx <= endIdx) $
    
    -- Appends empty spaces until endIdx <= length file
    let text = if endIdx <= length file
               then file
               else file ++ replicate (endIdx - length file) ' '
        
        -- Terminal colors
        reset = "\x1b[0m"
        color = getColor colorStr
        
        -- Calculates indices and line numbers
        (curLinIdx, curLinNum, slcLinIdx, slcLinNum, slcEndIdx) = 
            foldl' (\(cli, cln, sli, sln, sei) (idx, chr) ->
                let newCli = if chr == '\n' then idx + 1 else cli
                    newCln = if chr == '\n' then cln + 1 else cln
                    newSli = if idx == iniIdx then cli else sli
                    newSln = if idx == iniIdx then cln else sln
                    newSei = if idx >= endIdx && chr == '\n' then idx else sei
                in (newCli, newCln, newSli, newSln, newSei)
            ) (0, 0, 0, 0, 0) (zip [0..] text)
        
        numLen = length $ show (curLinIdx + 1)
        slice = take (slcEndIdx - slcLinIdx) $ drop slcLinIdx text
        iniIdx' = iniIdx - slcLinIdx
        endIdx' = endIdx - slcLinIdx
        
        -- Builds the display text
        buildText :: String -> Bool -> Bool -> Int -> Int -> String -> String
        buildText acc newl high line idx [] = acc
        buildText acc newl high line idx (chr:rest) =
            let newAcc = acc ++ 
                    (if newl 
                     then reset ++ printf " %s | " (pad numLen (show (line + 1))) ++ (if high then effectFn color else "")
                     else "") ++
                    (if idx == iniIdx' then effectFn color else "") ++
                    (if chr == '\n' && high && endIdx' - iniIdx' == 1 then " " else "") ++
                    (if idx == endIdx' then reset else "") ++
                    [chr]
                newNewl = chr == '\n'
                newHigh = if idx == iniIdx' then True else if idx == endIdx' then False else high
                newLine = if newNewl then line + 1 else line
            in buildText newAcc newNewl newHigh newLine (idx + 1) rest
    
    in buildText "" True False slcLinNum 0 slice ++ reset

-- Appends empty spaces to the left of a text
pad :: Int -> String -> String
pad len txt = replicate (max (len - length txt) 0) ' ' ++ txt

-- Simple assertion function
assert :: Bool -> a -> a
assert True  x = x
assert False _ = error "Assertion failed"

-- Helper function to get the UTF-8 byte length of a character
lenUtf8 :: Char -> Int
lenUtf8 c
    | n <= 0x7F = 1
    | n <= 0x7FF = 2
    | n <= 0xFFFF = 3
    | otherwise = 4
  where n = ord c

-- Converts line and column numbers to byte indices
toByteIndex :: (Int, Int) -> (Int, Int) -> String -> (Int, Int)
toByteIndex (startLine, startCol) (endLine, endCol) text =
    let textLines = zip [1..] (lines text)
        startIndex = sum (map (sum . map lenUtf8 . snd) (takeWhile ((<startLine) . fst) textLines)) +
                     sum (map lenUtf8 (take (startCol - 1) (snd (textLines !! (startLine - 1))))) +
                     startLine - 1  -- Account for newline characters
        endIndex = sum (map (sum . map lenUtf8 . snd) (takeWhile ((<endLine) . fst) textLines)) +
                   sum (map lenUtf8 (take (endCol - 1) (snd (textLines !! (endLine - 1))))) +
                   endLine - 1  -- Account for newline characters
    in (startIndex, endIndex)

-- Wrapper function that highlights error using line and column numbers
highlightError :: (Int, Int) -> (Int, Int) -> String -> String
highlightError start end text =
    let (startIndex, endIndex) = toByteIndex start end text
        errorColor = "red"
    in highlight startIndex endIndex errorColor (underline) text

underline :: String -> String
underline str = "\ESC[4m" ++ str

-- Function to get color code based on input string
getColor :: String -> String
getColor color = case color of
    "red"    -> "\x1b[31m"
    "green"  -> "\x1b[32m"
    "yellow" -> "\x1b[33m"
    "blue"   -> "\x1b[34m"
    "purple" -> "\x1b[35m"
    "cyan"   -> "\x1b[36m"
    _        -> "\x1b[37m"  -- Default to white
