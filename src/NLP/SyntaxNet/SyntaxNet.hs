{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module NLP.SyntaxNet.SyntaxNet
    ( readCnll
    , readParseTree
    ) where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Lens hiding (at)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Char
import qualified Data.Csv as Csv
import           Data.Tree
import           Data.Maybe
import           Protolude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor ((<$>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TEL
import qualified Data.Vector as V
import           Safe as S
import           Data.String

import           Data.ConllToken
import           Data.ParsedSentence
import           Data.SyntaxTree
import           Data.TagLabel
import           NLP.SyntaxNet.Types.CoNLL
import           NLP.SyntaxNet.Types.ParseTree

--------------------------------------------------------------------------------

readCnll :: FilePath -> IO [SnConllToken Text]
readCnll fpath = do 
  csvData <- BSL.readFile fpath 
  case TEL.decodeUtf8' csvData of
    Left  err  -> do
      putStrLn $ "error decoding" ++ (show err)
      return []
    Right dat  -> do
      let res = (Csv.decodeWith cnllOptions Csv.NoHeader $ TEL.encodeUtf8 dat) :: Either String (V.Vector (SnConllToken Text))
      case res of
        Left err  -> do
          putStrLn $ "error decoding" ++ (show err)
          return [] 
        Right vals ->   
          return $ V.toList vals

preprocess :: TL.Text -> TL.Text
preprocess txt = TL.cons '\"' $ TL.snoc escaped '\"'
  where escaped = TL.concatMap escaper txt

escaper :: Char -> TL.Text
escaper c
  | c == '\t' = "\"\t\""
  | c == '\n' = "\"\n\""
  | c == '\"' = "\"\""
  | otherwise = TL.singleton c


-- | Define custom options to read tab-delimeted files
cnllOptions =
  Csv.defaultDecodeOptions
    { Csv.decDelimiter = fromIntegral (ord '\t')
    }

--------------------------------------------------------------------------------
-- Dealing with trees

readParseTree :: FilePath -> IO (Maybe (Tree (SnConllToken Text)))
readParseTree fpath = do
  treeData <- BSC.readFile fpath
  let ls = BSC.lines treeData

  let lls  = map ( \x -> BSC.split ' ' x) ls 
      lln  = map parseNode lls

  let tree = fromList lln 
  
  return $ tree

-- | Same as readParseTree but for debugging
-- 
readParseTree' :: FilePath -> IO ()
readParseTree' fpath = do
  treeData <- BSC.readFile fpath
  let ls = BSC.lines treeData

  mapM_ (putStrLn . T.pack . show ) ls

  let lls  = map ( \x -> BSC.split ' ' x) ls
  
  lln  <- mapM parseNode' ls 
    
  tree <- fromList' $ lln

  mapM_ (putStrLn . T.pack . show ) lln
  putStrLn $ T.pack $ "----\n"
  putStrLn $ T.pack $ show $ drawTree' $ fromJust tree
  
  return $ ()
  
parseNode :: [BSC.ByteString] -> SnConllToken Text
parseNode llbs = do
  let llss  =  map BSC.unpack llbs
      lenLB = 3                             -- useful labels like TOKEN POS ER
      lenWP = (length $ filter (=="  ") llss ) -- each identation takes 2 spaces

  let lls  = map T.pack llss
      lvl  = (length lls) - lenLB - lenWP   -- calculate actual level  
      lbls = drop ((length lls) - 3) lls    -- extract only lables      

  ConllToken lvl                                 -- reuse id to indicate level, when working with trees          
               (lbls `at` 0)
               ""
               UnkCg
               (parsePosFg $ lbls `at` 1)
               ""
               0
               (parseGER $ lbls `at` 2)
               ""
               ""
        
parseNode' :: BSC.ByteString -> IO (SnConllToken Text)
parseNode' bs = do
  let llbs  = filter (/="|") $ BSC.split ' ' bs
      llss  = map BSC.unpack llbs
      lenLB = 3                                                  -- useful labels like TOKEN POS ER
      lenWP = (length $ filter (=="") llss) -- always have 1 in front,  each identation takes 2 spaces

  
  let llt  = map T.pack llss
      llsf = filter (/="") llt
      lvl  = ((mod lenWP 2))+1   -- calculate actual level  
      lbls = drop ((length llt) - 3) llt    -- extract only lables

  putStrLn $ T.pack $ show $ llss
  putStrLn $ T.pack $ show $ llsf
  putStrLn $ T.pack $ show $ lenWP
  putStrLn $ T.pack $ show $ lvl

  return $ ConllToken lvl                                 -- reuse id to indicate level, when working with trees          
                        (lbls `at` 0)
                        ""
                        UnkCg
                        (parsePosFg $ lbls `at` 1)
                        ""
                        0
                        (parseGER $ lbls `at`  2)
                        ""
                        ""
