module Model.CryptoUtils where


import qualified Data.Text as T

import System.Random
import Data.Word8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Crypto.Hash.SHA512 as SHA
import qualified Data.Text.Encoding as T

randomBytes :: Int -> StdGen -> [Word8]
randomBytes 0 _ = []
randomBytes ct g =
  let (value, nextG) = next g
  in fromIntegral value : randomBytes (ct - 1) nextG

randomBS :: Int -> StdGen -> BS.ByteString
randomBS len g = BS.pack $ randomBytes len g

hashText :: T.Text -> BS.ByteString -> BS.ByteString
hashText password salt =
  SHA.finalize $ SHA.updates SHA.init [salt, T.encodeUtf8 $ password]

makeHex :: BS.ByteString -> T.Text
makeHex = T.decodeUtf8 . BS16.encode

decodeHex :: T.Text -> BS.ByteString
decodeHex = fst . BS16.decode . T.encodeUtf8
