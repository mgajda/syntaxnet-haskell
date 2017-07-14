{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module NLP.SyntaxNet.Types.CoNLL where

import           Data.Char (toUpper)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text             as T
import           Data.Default
import           Data.Csv              as Csv
import           GHC.Generics                          
                 
--------------------------------------------------------------------------------

-- | Row entry containing row structure for CONLL
--   all relevant description can be fined at
--   https://github.com/tensorflow/models/blob/7d30a017fe50b648be6dee544f8059bde52db562/syntaxnet/syntaxnet/text_formats.cc#L62
data CoNLLEntry =
  CoNLLEntry
    { cnId        :: T.Text  -- ^ Index number
    , cnWord      :: T.Text  -- ^ Parsed word or punctuation symbol
    , cnLemma     :: T.Text  -- ^ Lemma or stem
    , cnPosCp     :: PosCg   -- ^ Part-of-Speech (POS) coarse-grained (PRON, VERB, DET, NOUN, etc) 
    , cnPosTag    :: PosFg   -- ^ Part-of-Speech (POS) fine-grained   (PRP, VBD, DT, NN etc.) 
    , cnFeats     :: T.Text  -- ^ Unordered set of syntactic and/or morphological features.
    , cnHead      :: Int     -- ^ Head of the current token, which is either a value of ID or '0'.
    , cnRel       :: T.Text  -- ^ grammatical relationships between different words in the sentence, alined with Head
    , cnHeadProj  :: T.Text  -- ^ Projective head of current token.
    , cnRelProj   :: T.Text  -- ^ Dependency relation to the PHEAD.     
    } deriving (Show, Eq, Generic)


-- | See "A Universal Part-of-Speech Tagset" by Slav Petrov, Dipanjan Das and Ryan McDonald
--   for more details http://arxiv.org/abs/1104.2086
--   Sum type for 12 universal part-of-speech tags, coarse-grained
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

-- | Sum type for fine-grained part-of-speech
data PosFg = 
    CC   -- Coordinating conjunction
  | CD   -- Cardinal number
  | DT   -- Determiner
  | EX   -- Existential there
  | FW   -- Foreign word
  | IN   -- Preposition or subordinating conjunction
  | JJ   -- Adjective
  | JJR  -- Adjective, comparative
  | JJS  -- Adjective, superlative
  | LS   -- List item marker
  | MD   -- Modal
  | NN   -- Noun, singular or mass
  | NNS  -- Noun, plural
  | NNP  -- Proper noun, singular
  | NNPS -- Proper noun, plural
  | PDT  -- Predeterminer
  | POS  -- Possessive ending
  | PRP  -- Personal pronoun
  | PRPS -- Possessive pronoun PRP$
  | RB   -- Adverb
  | RBR  -- Adverb, comparative
  | RBS  -- Adverb, superlative
  | RP   -- Particle
  | SYM  -- Symbol
  | TO   -- to
  | UH   -- Interjection
  | VB   -- Verb, base form
  | VBD  -- Verb, past tense
  | VBG  -- Verb, gerund or present participle
  | VBN  -- Verb, past participle
  | VBP  -- Verb, non-3rd person singular present
  | VBZ  -- Verb, 3rd person singular present
  | WDT  -- Wh-determiner
  | WP   -- Wh-pronoun
  | WPS  -- Possessive wh-pronoun WP$
  | WRB  -- Wh-adverb  
  deriving (Show, Eq, Generic)

data DEP =
    Acl
  | AclRelcl
  | Advcl
  | Advmod
  | Amod
  | Appos
  | Aux
  | AuxPass
  | Case
  | Cc
  | CCCoord
  | CCPreconj
  | CComp
  | Compound
  | CompoundPrt
  | Conj
  | Cop
  | Csubj
  | CsubjPass
  | Dep
  | Det
  | DetPredet
  | Discourse
  | Discolated 
  | Dobj
  | Exmpl
  | Fixed
  | FixedNot
  | Flat
  | Foreign
  | GoesWith
  | IObj
  | List
  | Mark
  | Neg
  | Nmod
  | NmodNpmod
  | NmodPoss
  | NmodTmod
  | Nsubj
  | NsubjPass
  | Nummod
  | Orphan
  | Parataxis
  | Punct
  | Reparandum
  | ROOT
  | Vocative
  | XComp
deriving(Show, Eq, Generic)

instance Csv.FromRecord CoNLLEntry where 
  parseRecord v =
    CoNLLEntry
      <$> v .! 0
      <*> v .! 1
      <*> v .! 2
      <*> ( parsePosCf <$> v .! 3)
      <*> ( parsePosFg <$> v .! 4)
      <*> v .! 5
      <*> v .! 6
      <*> v .! 7
      <*> v .! 8
      <*> v .! 9

parsePosCf :: String -> PosCg
parsePosCf s =
  case (map toUpper s) of
    "VERB" -> VERB
    "NOUN" -> NOUN
    "PRON" -> PRON
    "ADJ"  -> ADJ
    "ADV"  -> ADV
    "ADP"  -> ADP    
    "CONJ" -> CONJ   
    "DET"  -> DET    
    "NUM"  -> NUM    
    "PRT"  -> PRT    
    "X"    -> X      
    "."    -> PUNCT      

parsePosFg s =
  case (map toUpper s) of
    "PRP" -> PRP
    "VBD" -> VBD
    "DT"  -> DT
    "NN"  -> NN   
      
-- | TODO: Check if SyntaxTree can generate named output
--   where
instance Csv.FromNamedRecord CoNLLEntry where
  parseNamedRecord m =
    CoNLLEntry
      <$> m Csv..: "param1"
      <*> m Csv..: "param2"
      <*> m Csv..: "param3"
      <*> ( parsePosCf <$> m Csv..: "param4")
      <*> ( parsePosFg <$> m Csv..: "param5")
      <*> m Csv..: "param6"
      <*> m Csv..: "param7"
      <*> m Csv..: "param8"
      <*> m Csv..: "param9"
      <*> m Csv..: "param10"
