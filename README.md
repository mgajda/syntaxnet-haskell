# Google's SyntaxNet API in Haskell

Haskell library for using [Google's SyntaxNet](https://github.com/tensorflow/models/tree/master/syntaxnet).
SyntaxNet is natural language parser including:
* Part of Speech tagger,
* syntax tree generator,
* recognition of referential expressions.

This API allows:
1. Reading saved SyntaxNet parse tree, POS, or reference assignment from file.
2. Easy manipulation of the parse trees with extra information (given by POS and/or reference assignment.)

Examples of parsed [SyntaxNet inputs](examples/) are attached.

Documentation will put into [docs/](docs/) folder.
