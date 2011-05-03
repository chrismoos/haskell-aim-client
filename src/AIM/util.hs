module AIM.Util where

import Numeric
import qualified Data.ByteString.Lazy as BS
import Data.Binary

hex_str :: BS.ByteString -> String
hex_str bs = do
	let s = concat $ map (\x -> (toHex x) ++ " ") (BS.unpack bs)
	take ((length s) - 1) s

toHex :: Word8 -> String
toHex b = case showHex b "" of
	      	a:b:[] -> a:b:[]
	      	a:[] -> '0':a:[]
