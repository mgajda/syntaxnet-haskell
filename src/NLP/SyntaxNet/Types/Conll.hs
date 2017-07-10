module NLP.SyntaxNet.Types.Conll where

import           Data.Text
import           Data.Default
import qualified Data.Csv as Csv
                          
                 
--------------------------------------------------------------------------------

-- | Row entry containing row structure for CONLL
--   all relevant description can be fined at
--   https://github.com/tensorflow/models/blob/7d30a017fe50b648be6dee544f8059bde52db562/syntaxnet/syntaxnet/text_formats.cc#L62
data CnllEntry =
  CnllEntry
    { cnId        :: Text -- ^ Index number
    , cnWord      :: Text -- ^ Parsed word or punctuation symbol
    , cnLemma     :: Text -- ^ Lemma or stem
    , cnPosCp     :: Text -- ^ Part-of-Speech (POS) coarse-grained (PRON, VERB, DET, NOUN, etc) 
    , cnPosTag    :: Text -- ^ Part-of-Speech (POS) fine-grained   (PRP, VBD, DT, NN etc.) 
    , cnFeats     :: Text -- ^ Unordered set of syntactic and/or morphological features.
    , cnHead      :: Int  -- ^ Head of the current token, which is either a value of ID or '0'.
    , cnRel       :: Text -- ^ grammatical relationships between different words in the sentence, alined with Head
    , cnHeadProj  :: Text -- ^ Projective head of current token.
    , cnRelProj   :: Text -- ^ Dependency relation to the PHEAD.     
    } deriving (Show, Eq)

-- TODO: 1. Add Cassava based reader 
--       2. 


-- | TODO: Check if SyntaxTree can generate named output
--   where
instance Csv.FromNamedRecord CnllEntry where
  parseNamedRecord m =
    CnllEntry
      <$> m Csv..: "param1"
      <*> m Csv..: "param2"
      <*> m Csv..: "param3"
      <*> m Csv..: "param4"
      <*> m Csv..: "param5"
      <*> m Csv..: "param6"
      <*> m Csv..: "param7"
      <*> m Csv..: "param8"
      <*> m Csv..: "param9"
      <*> m Csv..: "param10"