# Google's SyntaxNet API in Haskell

[![Build Status](https://travis-ci.com/mgajda/syntaxnet-haskell.svg?token=DtSUtnf37TUDy1p4x6fT&branch=master)](https://travis-ci.com/mgajda/syntaxnet-haskell)

Haskell library for using [Google's SyntaxNet](https://github.com/tensorflow/models/tree/master/syntaxnet).
SyntaxNet is natural language parser including:
* Part of Speech tagger,
* syntax tree generator,
* recognition of referential expressions.

This API allows:
1. Reading saved SyntaxNet parse tree, POS, or reference assignment from file.
2. Easy manipulation of the parse trees with extra information (given by POS and/or reference assignment.)

# Documentation
Documentation will put into [docs](docs/) folder.

# Tests
Examples of parsed [SyntaxNet inputs](test/examples/) are attached.

There are following files there for each test:
* `.txt`  file contains the input
* `.cnll` file contains the SyntaxNet output from `run.sh` script (parsed by the library)
* `.tree` file contains the SyntaxNet output from `demo.sh` script (if present)

# Usage

```
1) stack repl
2) :load src/NLP/SyntaxNet/SyntaxNet.hs 
3) > tr <- readParseTree "test/examples/test1.tree"
4) > drawTree' $ fromJust $ tr
for Testing.
```

# Acknowledgements

- [Michał J. Gajda](https://github.com/mgajda)
- [Sergey Bushnyak](https://github.com/sigrlami)
