# What is DDHC

DDHC, synonym for Drastically Different Haskell Compiler, is a haskell compiler
built only using Haskell and LLVM nothing else...

DDHC itself is an adventure as it was my first serious compiler I have made.

No questions on the naming schemes :)

## DDHC Components

DDHC is made of couple of components, these are:
* DDHC-Scanner
* DDHC-Parser
* DDHC-Annihilation
* DDHC-Ilk

DDHC-Scanner is the scanner/lexer/recognizer for DDHC.

The scanner data type is based on the attoparsec parser type, but slightly different

DDHC-Parser is the parser for DDHC.

The parser data type is the same as the DDHC-Scanner.

DDHC-Annihilation is the information collection, error checking and warner for DDHC.

Annihilation has few components one of them is the _Checks_ data type which is _Scanner_ like
type.
