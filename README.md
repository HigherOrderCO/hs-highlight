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

## API

The main functions provided by this library are:

- `highlightError :: (Int, Int) -> (Int, Int) -> String -> String`
- `highlight :: (Int, Int) -> (Int, Int) -> String -> String -> (String -> String) -> String`

Additional utility functions:

- `underline`, `bold`, `italic`, `parenthesize`, `strikethrough`, `inverse`
- `getColor :: String -> String`

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

## Contact

For any queries, please contact the maintainer at lorenzobattistela@gmail.com.
