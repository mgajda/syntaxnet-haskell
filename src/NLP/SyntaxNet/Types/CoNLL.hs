{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module NLP.SyntaxNet.Types.CoNLL where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toUpper, isUpper, toLower)
import           Data.Csv as Csv
import           Data.Maybe
import           Data.Default
import           Protolude
import           Data.List.Split
import qualified Data.Text as T
import           GHC.Generics

import           Data.ConllToken (ConllToken(..), SyntaxErrorCoNLL(..))
import           Data.SyntaxTree (SyntaxtTree(..), createSyntaxTree)
import           Model.PennTreebank
import           Model.UniversalTreebank
import           Data.TagLabel

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
        
instance Csv.FromRecord (SnConllToken T.Text) where 
  parseRecord v = do
    a0 <- v .! 0
    a1 <- v .! 1
    a2 <- v .! 2
    a3 <- ( parsePosCf <$> v .! 3)
    a4 <- ( parsePosFg <$> v .! 4)
    a5 <- v .! 5
    a6 <- v .! 6
    a7 <- ( parseGER   <$> v .! 7)
    a8 <- v .! 8
    a9 <- v .! 9
    return (ConllToken a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
         
-- | Converting coarse-grained textual types int
--   ADT representation
parsePosCf :: T.Text -> PosCG
parsePosCf s =
  case (map toUpper $ T.unpack s) of
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
parsePosFg :: T.Text -> POS
parsePosFg s =
  case (map toUpper $ T.unpack s) of
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
    "PRP$"-> fromJust $ fromLabelText "RPR$"
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
--    "WPS" -> WPS
    "WRB" -> WRB
--    otherwise -> UnkFg -- haskell-conll doesn't support fallback
    
parseGER :: T.Text -> REL
parseGER s =
  case s of
    "acl"         -> Acl
    "acl:relcl"   -> fromJust $ fromLabelText "acl:relcl"
    "advck"       -> Advcl
    "advmod"      -> Advmod
    "amod"        -> Amod
    "appos"       -> Appos
    "aux"         -> Aux
    "auxpass"     -> Auxpass
    "case"        -> Case
    "cc"          -> Cc
    "cc:preconj"  -> fromJust $ fromLabelText "cc:preconj"
    "ccomp"       -> Ccomp
    "compound"    -> Compound
    "compound:prt"-> fromJust $ fromLabelText "compound:prt"
    "conj"        -> Conj
    "cop"         -> Cop
    "csubj"       -> Csubj
    "csubjpass"   -> Csubjpass
    "dep"         -> Dep
    "det"         -> Det
    "det:predet"  -> fromJust $ fromLabelText "det:predet" -- haskell-conll have inconsistency in naming, why use _ whee you have camelcase
    "discource"   -> Discourse
    "discolated"  -> Dislocated 
    "dobj"        -> Dobj
    "expl"        -> Expl
    "fixed"       -> fromJust $ fromLabelText "fixed"
    "fixed:not"   -> fromJust $ fromLabelText "fixed:not" -- haskell-conll doesn't support this one
    "flat"        -> Flat
    "foreign"     -> Foreign
    "goeswith"    -> Goeswith
    "iobj"        -> Iobj
    "list"        -> List
    "mark"        -> Mark
    "neg"         -> Neg
    "nmod"        -> Nmod
    "nmod:npmod"  -> fromJust $ fromLabelText "nmod:npmod"
    "nmod:poss"   -> fromJust $ fromLabelText "nmod:poss"
    "nmod:tmod"   -> fromJust $ fromLabelText "nmod:tmod"
    "nsubj"       -> Nsubj
    "nsubjpass"   -> Nsubjpass
    "nummod"      -> Nummod
    "orphan"      -> Orphan
    "parataxis"   -> Parataxis
    "punct"       -> Punct
    "reparandum"  -> Reparandum
    "ROOT"        -> ROOT
    "vocatile"    -> Vocative
    "xcomp"       -> Xcomp
    otherwise     -> Punct -- haskell-conll doesn't support uknown type, like UnkGer, so fall back ro Punct
