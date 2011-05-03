module AIM.Tlv where
	
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Word
import Char
import Data.Binary.Put
import Data.Binary.Get

import AIM.Util

data TLV = TLV {tag :: Word16, len :: Word16, value :: BS.ByteString}

instance Show TLV where
	show x = "Tag: " ++ (show $ tag x) ++ ", Length: " ++ (show $ len x) ++ ", Value: " ++ (show $ hex_str (value x))

tlv_get :: BS.ByteString -> (TLV, BS.ByteString)
tlv_get stuff = do
	runGet f stuff
		where
			f = do
				tag <- getWord16be
				len <- getWord16be
				stuff <- getLazyByteString (fromIntegral len)
				rest <- getRemainingLazyByteString
				return $ ((TLV tag len stuff), rest)

tlv_extract_block :: Int -> BS.ByteString -> ([TLV], BS.ByteString)
tlv_extract_block 0 stuff = ([], stuff)
tlv_extract_block num stuff = if (BS.length stuff) == 0 then ([], BS.empty) else do
	let result = tlv_get stuff
	case tlv_extract_block (num-1) (snd result) of
		([], y) -> ([fst result], y)
		(x, rest) -> ([fst result] ++ x, rest)
			
tlv_extract_lblock :: BS.ByteString -> [TLV]
tlv_extract_lblock stuff = if (BS.length stuff) == 0 then [] else do
	let result = tlv_get stuff
	case tlv_extract_lblock (snd result) of
		[] -> [fst result]
		x -> [fst result] ++ x

tlv_get_lblock :: BS.ByteString -> ([TLV], BS.ByteString)
tlv_get_lblock stuff = ([], BS.empty)


tlv_create :: Word16 -> BS.ByteString -> BS.ByteString
tlv_create tag value = runPut $ putWord16be tag >> putWord16be (fromIntegral $ BS.length value) >> putLazyByteString value

