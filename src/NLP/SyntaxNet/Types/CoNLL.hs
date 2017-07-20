{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module NLP.SyntaxNet.Types.CoNLL where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toUpper, isUpper, toLower)
import           Data.ConllToken (ConllToken(..), SyntaxErrorCoNLL(..))
import           Data.Csv as Csv
import           Data.Default
--import           Data.List
import           Protolude
import           Data.List.Split
import           Data.SyntaxTree (SyntaxtTree(..), createSyntaxTree)
import           Data.TagLabel
import qualified Data.Text as T
import           GHC.Generics
import           Model.PennTreebank
import           Model.UniversalTreebank
import           Protolude
                 
--------------------------------------------------------------------------------

type SnConllToken a = ConllToken  PosCG POS REL T.Text a
type SnConllTree  a = SyntaxtTree PosCG POS REL T.Text a

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

--------------------------------------------------------------------------------
        
-- | TODO: Check if SyntaxTree can generate named output
--   where
instance Csv.FromNamedRecord SnConllToken where
  parseNamedRecord m =
    SnConllToken
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

instance Csv.FromRecord SnConllToken where 
  parseRecord v =
    SnConllToken
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

-- -- | Converting coarse-grained textual types int
-- --   ADT representation
-- parsePosCf :: String -> PosCG
-- parsePosCf s =
--   case (map toUpper s) of
--     "VERB" -> VERB
--     "NOUN" -> NOUN
--     "PRON" -> PRON
--     "ADJ"  -> ADJ
--     "ADV"  -> ADV
--     "ADP"  -> ADP    
--     "CONJ" -> CONJ   
--     "DET"  -> DET    
--     "NUM"  -> NUM    
--     "PRT"  -> PRT    
--     "X"    -> X      
--     "."    -> PUNCT      
--     otherwise -> UnkCg
    
-- -- | Converting fine-grained textual types into
-- --   ADT representation
-- parsePosFg :: String -> PosFG
-- parsePosFg s =
--   case (map toUpper s) of
--     "CC"  -> CC  
--     "CD"  -> CD
--     "DT"  -> DT
--     "EX"  -> EX
--     "FW"  -> FW
--     "IN"  -> IN
--     "JJ"  -> JJ
--     "JJR" -> JJR
--     "JJS" -> JJS
--     "LS"  -> LS
--     "MD"  -> MD
--     "NN"  -> NN
--     "NNS" -> NNS
--     "NNP" -> NNP
--     "NNPS"-> NNPS
--     "PDT" -> PDT
--     "POS" -> POS
--     "PRP" -> PRP
--     "PRPS"-> PRPS
--     "RB"  -> RB
--     "RBR" -> RBR
--     "RBS" -> RBS
--     "RP"  -> RP
--     "SYM" -> SYM
--     "TO"  -> TO
--     "UH"  -> UH
--     "VB"  -> VB
--     "VBD" -> VBD
--     "VBG" -> VBG
--     "VBN" -> VBN
--     "VBP" -> VBP
--     "VBZ" -> VBZ
--     "WDT" -> WDT
--     "WP"  -> WP
--     "WPS" -> WPS
--     "WRB" -> WRB
--     otherwise -> UnkFg
    
-- parseGER :: String -> GER
-- parseGER s =
--   case s of
--     "acl"         -> Acl
--     "acl:relcl"   -> AclRelcl
--     "advck"       -> Advcl
--     "advmod"      -> Advmod
--     "amod"        -> Amod
--     "appos"       -> Appos
--     "aux"         -> Aux
--     "auxpass"     -> Auxpass
--     "case"        -> Case
--     "cc"          -> Cc
--     "cc:preconj"  -> CcPreconj
--     "ccomp"       -> Ccomp
--     "compound"    -> Compound
--     "compound:prt"-> CompoundPrt
--     "conj"        -> Conj
--     "cop"         -> Cop
--     "csubj"       -> Csubj
--     "csubjpass"   -> Csubjpass
--     "dep"         -> Dep
--     "det"         -> Det
--     "det:predet"  -> DetPredet
--     "discource"   -> Discourse
--     "discolated"  -> Discolated 
--     "dobj"        -> Dobj
--     "expl"        -> Expl
--     "fixed"       -> Fixed
--     "fixed:not"   -> FixedNot
--     "flat"        -> Flat
--     "foreign"     -> Foreign
--     "goeswith"    -> Goeswith
--     "iobj"        -> Iobj
--     "list"        -> List
--     "mark"        -> Mark
--     "neg"         -> Neg
--     "nmod"        -> Nmod
--     "nmod:npmod"  -> NmodNpmod
--     "nmod:poss"   -> NmodPoss
--     "nmod:tmod"   -> NmodTmod
--     "nsubj"       -> Nsubj
--     "nsubjpass"   -> NsubjPass
--     "nummod"      -> Nummod
--     "orphan"      -> Orphan
--     "parataxis"   -> Parataxis
--     "punct"       -> Punct
--     "reparandum"  -> Reparandum
--     "ROOT"        -> Root
--     "vocatile"    -> Vocative
--     "xcomp"       -> XComp
--     otherwise     -> UnkGer
