{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module NLP.SyntaxNet.Types.CoNLL where

import           Data.Char (toUpper, isUpper)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text             as T
import           Data.List
import           Data.Default
import           Data.Csv              as Csv
import           GHC.Generics                          
                 
--------------------------------------------------------------------------------

-- | Row entry containing row structure for CONLL
--   all relevant description can be fined at
--   https://github.com/tensorflow/models/blob/7d30a017fe50b648be6dee544f8059bde52db562/syntaxnet/syntaxnet/text_formats.cc#L62
--   NB: can be renamed to Token/Entry/Word/  
data Token =
  Token
    { tnId        :: Int     -- ^ Index number
    , tnWord      :: T.Text  -- ^ Parsed word or punctuation symbol
    , tnLemma     :: T.Text  -- ^ Lemma or stem
    , tnPosCG     :: PosCG   -- ^ Part-of-Speech (POS) coarse-grained (PRON, VERB, DET, NOUN, etc) 
    , tnPosFG     :: PosFG   -- ^ Part-of-Speech (POS) fine-grained   (PRP, VBD, DT, NN etc.) 
    , tnFeats     :: T.Text  -- ^ Unordered set of syntactic and/or morphological features.
    , tnHead      :: Int     -- ^ Head of the current token, which is either a value of ID or '0'.
    , tnRel       :: GER     -- ^ grammatical relationships between different words in the sentence, alined with Head
    , tnHeadProj  :: T.Text  -- ^ Projective head of current token.
    , tnRelProj   :: T.Text  -- ^ Dependency relation to the PHEAD.     
    } deriving (Show, Eq, Generic)


-- | See "A Universal Part-of-Speech Tagset" by Slav Petrov, Dipanjan Das and Ryan McDonald
--   for more details http://arxiv.org/abs/1104.2086
--   Sum type for 12 universal part-of-speech tags, coarse-grained
data PosCG =
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
  | UnkCg  -- unknown
  deriving (Show, Eq, Generic)

-- | Sum type for fine-grained part-of-speech (pos)
data PosFG = 
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
  | UnkFg
  deriving (Show, Eq, Generic)

-- | Grammatical Entity Relation (GER), also known as dependecies
--   Currently English set http://universaldependencies.org/en/dep/all.html
--   TODO: should be extender
data GER =
    Acl
  | AclRelcl
  | Advcl
  | Advmod
  | Amod
  | Appos
  | Aux
  | Auxpass
  | Case
  | Cc
  | CcPreconj
  | Ccomp
  | Compound
  | CompoundPrt
  | Conj
  | Cop
  | Csubj
  | Csubjpass
  | Dep
  | Det
  | DetPredet
  | Discourse
  | Discolated 
  | Dobj
  | Expl
  | Fixed
  | FixedNot
  | Flat
  | Foreign
  | Goeswith
  | Iobj
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
  | UnkGer           -- unknown
  deriving ( Eq, Generic)

instance Show GER where
  show v =
    case v of
      UnkGer    -> "unknown"
      otherwise ->
        intercalate ":" $ splitR isUpper $ toString $ shows v
        where
          splitR :: (Char -> Bool) -> String -> [String]
          splitR _ [] = []
          splitR p s =
            let
              go :: Char -> String -> [String]
              go m s' = case break p s' of
                (b', [])     -> [ m:b' ]
                (b', (x:xs)) -> ( m:b' ) : go x xs
            in case break p s of
              (b,  [])    -> [ b ]
              ([], (h:t)) -> go h t
              (b, (h:t))  -> b : go h t

          toString :: ShowS -> String
          toString = ($ [])
   
              

--------------------------------------------------------------------------------
        
-- | TODO: Check if SyntaxTree can generate named output
--   where
instance Csv.FromNamedRecord Token where
  parseNamedRecord m =
    Token
      <$> m Csv..: "param1"
      <*> m Csv..: "param2"
      <*> m Csv..: "param3"
      <*> ( parsePosCf <$> m Csv..: "param4")
      <*> ( parsePosFg <$> m Csv..: "param5")
      <*> m Csv..: "param6"
      <*> m Csv..: "param7"
      <*> ( parseGER   <$> m Csv..: "param8")
      <*> m Csv..: "param9"
      <*> m Csv..: "param10"

instance Csv.FromRecord Token where 
  parseRecord v =
    Token
      <$> v .! 0
      <*> v .! 1
      <*> v .! 2
      <*> ( parsePosCf <$> v .! 3)
      <*> ( parsePosFg <$> v .! 4)
      <*> v .! 5
      <*> v .! 6
      <*> ( parseGER   <$> v .! 7)
      <*> v .! 8
      <*> v .! 9

-- | Converting coarse-grained textual types int
--   ADT representation
parsePosCf :: String -> PosCG
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
    otherwise -> UnkCg
    
-- | Converting fine-grained textual types into
--   ADT representation
parsePosFg :: String -> PosFG
parsePosFg s =
  case (map toUpper s) of
    "CC"  -> CC  
    "CD"  -> CD
    "DT"  -> DT
    "EX"  -> EX
    "FW"  -> FW
    "IN"  -> IN
    "JJ"  -> JJ
    "JJR" -> JJR
    "JJS" -> JJS
    "LS"  -> LS
    "MD"  -> MD
    "NN"  -> NN
    "NNS" -> NNS
    "NNP" -> NNP
    "NNPS"-> NNPS
    "PDT" -> PDT
    "POS" -> POS
    "PRP" -> PRP
    "PRPS"-> PRPS
    "RB"  -> RB
    "RBR" -> RBR
    "RBS" -> RBS
    "RP"  -> RP
    "SYM" -> SYM
    "TO"  -> TO
    "UH"  -> UH
    "VB"  -> VB
    "VBD" -> VBD
    "VBG" -> VBG
    "VBN" -> VBN
    "VBP" -> VBP
    "VBZ" -> VBZ
    "WDT" -> WDT
    "WP"  -> WP
    "WPS" -> WPS
    "WRB" -> WRB
    otherwise -> UnkFg
    
parseGER :: String -> GER
parseGER s =
  case s of
    "acl"         -> Acl
    "acl:relcl"   -> AclRelcl
    "advck"       -> Advcl
    "advmod"      -> Advmod
    "amod"        -> Amod
    "appos"       -> Appos
    "aux"         -> Aux
    "auxpass"     -> Auxpass
    "case"        -> Case
    "cc"          -> Cc
    "cc:preconj"  -> CcPreconj
    "ccomp"       -> Ccomp
    "compound"    -> Compound
    "compound:prt"-> CompoundPrt
    "conj"        -> Conj
    "cop"         -> Cop
    "csubj"       -> Csubj
    "csubjpass"   -> Csubjpass
    "dep"         -> Dep
    "det"         -> Det
    "det:predet"  -> DetPredet
    "discource"   -> Discourse
    "discolated"  -> Discolated 
    "dobj"        -> Dobj
    "expl"        -> Expl
    "fixed"       -> Fixed
    "fixed:not"   -> FixedNot
    "flat"        -> Flat
    "foreign"     -> Foreign
    "goeswith"    -> Goeswith
    "iobj"        -> Iobj
    "list"        -> List
    "mark"        -> Mark
    "neg"         -> Neg
    "nmod"        -> Nmod
    "nmod:npmod"  -> NmodNpmod
    "nmod:poss"   -> NmodPoss
    "nmod:tmod"   -> NmodTmod
    "nsubj"       -> Nsubj
    "nsubjpass"   -> NsubjPass
    "nummod"      -> Nummod
    "orphan"      -> Orphan
    "parataxis"   -> Parataxis
    "punct"       -> Punct
    "reparandum"  -> Reparandum
    "ROOT"        -> ROOT
    "vocatile"    -> Vocative
    "xcomp"       -> XComp
    otherwise     -> UnkGer
