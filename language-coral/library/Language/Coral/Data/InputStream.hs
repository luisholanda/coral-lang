module Language.Coral.Data.InputStream
  ( Word8
  , InputStream(..)
  , readInputStream
  , hReadInputStream
  , inputStreamToString
  , inputStreamFromString
  , inputStreamEmpty
  , takeByte
  , takeChar
  , dropBytes
  , peekBytes
  , peekChars
  , countLines
  )
where


import qualified Data.ByteString               as BS
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.UTF8          as BE
import           Data.Coerce                    ( coerce )
import           Data.String                    ( IsString(..) )
import           Data.Word                      ( Word8 )
import           System.IO


newtype InputStream = IS BE.ByteString deriving (Eq, Ord, Show)


readInputStream :: FilePath -> IO InputStream
readInputStream f = coerce <$> BS.readFile f
{-# INLINE readInputStream #-}


hReadInputStream :: Handle -> IO InputStream
hReadInputStream h = coerce <$> BS.hGetContents h
{-# INLINE hReadInputStream #-}


inputStreamToString :: InputStream -> String
inputStreamToString = BE.toString . coerce
{-# INLINE inputStreamToString #-}


inputStreamFromString :: String -> InputStream
inputStreamFromString = IS . BE.fromString
{-# INLINE inputStreamFromString #-}


instance IsString InputStream where fromString = inputStreamFromString


takeByte :: InputStream -> (Word8, InputStream)
takeByte bs = let (c, i) = takeChar bs in (B.c2w c, i)
{-# INLINE takeByte #-}


takeChar :: InputStream -> (Char, InputStream)
takeChar = maybe (error "takeChar: no char left") coerce . BE.uncons . coerce
{-# INLINE takeChar #-}


dropBytes :: Int -> InputStream -> InputStream
dropBytes n = coerce . BE.drop n . coerce
{-# INLINE dropBytes #-}


inputStreamEmpty :: InputStream -> Bool
inputStreamEmpty = BS.null . coerce
{-# INLINE inputStreamEmpty #-}


peekBytes :: Int -> InputStream -> BS.ByteString
peekBytes n = BE.take n . coerce
{-# INLINE peekBytes #-}


peekChars :: Int -> InputStream -> String
peekChars n = BE.toString . peekBytes n
{-# INLINE peekChars #-}


countLines :: InputStream -> Int
countLines = length . BE.lines . coerce
{-# INLINE countLines #-}
