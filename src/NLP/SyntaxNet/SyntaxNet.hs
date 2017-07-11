{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module NLP.SyntaxNet.SyntaxNet
    ( readCnll
    , readCnll'
    ) where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Char
import           Data.Tree
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor ((<$>))
import           Data.Csv ( DefaultOrdered (headerOrder)
                          , FromField (parseField)
                          , FromNamedRecord (parseNamedRecord)
                          , Header
                          , HasHeader(..)
                          , ToField (toField)
                          , ToNamedRecord (toNamedRecord)
                          , (.:)
                          , (.=)
                          , DecodeOptions(..)
                          )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TEL
import qualified Data.Csv as Csv
import qualified Data.Vector as V

import           NLP.SyntaxNet.Types.Conll
import           NLP.SyntaxNet.Types.ParseTree

--------------------------------------------------------------------------------

readCnll :: FilePath -> IO [CnllEntry]
readCnll fpath = do 
  csvData <- BSL.readFile fpath 
  case TEL.decodeUtf8' csvData of
    Left  err  -> do
      putStrLn $"error decoding" ++ (show err) -- $ Left $ show err
      return []
    Right dat  -> do
      let decodingResult = (Csv.decodeWith cnllOptions NoHeader $ TEL.encodeUtf8 dat) :: Either String (V.Vector CnllEntry)
      case decodingResult of
        Left err  -> do
          putStrLn $ "error decoding" ++ (show err)
          return [] -- $ Left err
        Right vals ->   
          return $ V.toList vals

-- | Reader for Named files with header
-- 
readCnll' :: FilePath -> IO [CnllEntry]
readCnll' fpath = do
  csvData <- BSL.readFile fpath 
  case TEL.decodeUtf8' csvData of
    Left  err  -> do
      putStrLn $"error decoding" ++ (show err) -- $ Left $ show err
      return []
    Right dat  -> do
      let dat' = dat
      case decodeEntries $ TEL.encodeUtf8 dat' of
        Left  err  -> do
          putStrLn $ "error decoding" ++ (show err)
          return [] -- $ Left err
        Right vals -> do
          -- TODO: do additional operations
          return $ V.toList vals

          
decodeEntries :: BL.ByteString -> Either String (V.Vector CnllEntry)
decodeEntries = fmap snd . Csv.decodeByName

decodeEntries' :: BL.ByteString -> Either String (V.Vector CnllEntry)
decodeEntries' = fmap snd . Csv.decodeByName

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
    { decDelimiter = fromIntegral (ord '\t')
    }

--------------------------------------------------------------------------------
-- Dealing with trees

readParseTree :: FilePath -> IO (Maybe (Tree TreeNode))
readParseTree fpath = do
  treeData <- BSC.readFile fpath
  let ls = BSC.lines treeData
  putStrLn $ show $ ls
  -- TODO: 1. parse each line
  --       2. 
  return $ Nothing
