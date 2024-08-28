# Highlight

Highlight is a Haskell library that provides functionality to highlight and emphasize specific parts of code snippets or text. It aims to improve readability and debugging by visually emphasizing problematic areas in source code.

## Features

- Highlight specific portions of text with customizable colors and effects
- Support for multi-line highlighting
- Various text effects: underline, bold, italic, parenthesize, strikethrough, and inverse
- Error highlighting with predefined style
- ANSI color support for terminal output

## Installation

To use this library in your Haskell project, add the following to your `.cabal` file:

```
build-depends: highlight
```

## Usage

Here's a basic example of how to use the `highlightError` function:

```haskell
import Highlight

main :: IO ()
main = do
    let code = "function greet(name) {\n  console.log(\"Hello, \" + name + \"!\");\n  return \"Greeted \" + name;\n}\n\ngreet(\"World\");"
    putStrLn $ highlightError (2, 15) (2, 32) code
```

This will highlight the specified portion of the code in red and underline it.
Note that in the structure `(startLine, startCol), (endLine, endCol)` endCol is exclusive, which means in the line:
`abcde`
`(1, 1) , (1, 3)` will underline `ab`

## API

The main functions provided by this library are:

- `highlightError :: (Int, Int) -> (Int, Int) -> String -> String`
- `highlight :: (Int, Int) -> (Int, Int) -> String -> (String -> String) -> String -> String`

Additional utility functions:

- `underline`, `bold`, `italic`, `parenthesize`, `strikethrough`, `inverse`
- `getColor :: String -> String`

## Examples

Here are some more examples of how to use the library:

```haskell
import Highlight

main :: IO ()
main = do
    let code = "def factorial(n):\n    if n == 0:\n        return 1\n    else:\n        return n * factorial(n - 1)"
    
    -- Highlight a specific range with a custom color and formatting
    putStrLn $ highlight (2, 5) (2, 11) (getColor "blue") bold code
    
    -- Use predefined error highlighting
    putStrLn $ highlightError (4, 9) (4, 14) code
    
    -- Combine multiple effects
    putStrLn $ highlight (1, 1) (1, 3) (getColor "green") (underline . italic) code
```

## Testing

To run the tests, use:

```
cabal test
```

The test suite includes unit tests for individual functions and integration tests for more complex scenarios.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

Developed with [@NaoEhSavio](https://github.com/NaoEhSavio)

## Contact

For any queries, please contact the maintainer at lorenzobattistela@gmail.com.
