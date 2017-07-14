# NLP Shared Types Rationale

## Intoduction
Modern NLP libraries tend to standardatization of core types and values used in language analysis. While it seems that some language migt be very different, their structure always can be boiled down to specific set, that been already researched.

## Standard Definitions

1) Part-of-Speech(POS) tags can be of 2 types - Coarse-Grained, Fine-Grained, 

- Coarse-grained universal set initially presented in the paper http://www.petrovi.de/data/lrec.pdf
- Fine-grained 

Now, there is wide adoption within scientific community for universal things. Look at http://universaldependencies.org/

Both of those can be represented as sum type like this:

```haskell
data PosCg =
    VERB   -- verbs (all tenses and modes)
  | NOUN   -- nouns (common and proper)
  | PRON   -- pronouns 
  | ADJ    -- adjectives
  | ADV    -- adverbs
  | ADP    -- adpositions (prepositions and postpositions)
  | CONJ   -- conjunctions
  | DET    -- determiners
  | NUM    -- cardinal numbers
  | PRT    -- particles or other function words
  | X      -- other: foreign words, typos, abbreviations
  | PUNCT  -- punctuation
  deriving (Show, Eq, Generic)
```

CoreNLP and SyntaxNet different in some small things, like first one defined punctuation with `punc` while other with `.`, but generally should be converted to same ADT.

2) Parsing logic should be separate from type definition.

While it seems like a nice way to describe type with additional features it's much more difficult to adopt its use with other libraries. I think it's vetter to have simepl record types and fill out with external parsing function, specific to each library. 

3) Parsing 

CoNLL is basically tab-separated csv (!) why do we need our own parser if we already have very fast `cassava`? Also in favor of using already established solutions for parsing. We need custom parsers only when specifc format encountered.
