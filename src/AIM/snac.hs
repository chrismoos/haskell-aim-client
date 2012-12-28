module AIM.Snac where
	
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Word
import Data.Char
import Data.Binary.Put
import Data.Binary.Get

import AIM.Util
import AIM.Tlv

data SnacPacket = SnacPacket {familyID :: Word16, subtypeID :: Word16, flags :: Word16, reqID :: Word32, snacData :: BS.ByteString}



instance Show SnacPacket where
	show x = "SNAC Header: Family ID: " ++ (show $ familyID x) ++ ", SubType ID: " ++ (show $ subtypeID x) ++ ", Flags: " ++ (show $ flags x)
			++ ", Request ID: " ++ (show $ reqID x) ++ ", \nSNAC Data: " ++ (hex_str $ snacData x)



snac_extract_tlvs :: BS.ByteString -> [TLV]

snac_extract_tlvs stuff = if (BS.length stuff) == 0 then [] else do
	let result = runGet f stuff
	[(fst result)] ++ (snac_extract_tlvs (snd result))
		where
			f :: Get (TLV, BS.ByteString)
			f = do
				tag <- getWord16be
				len <- getWord16be
				stuff <- getLazyByteString (fromIntegral len)
				rest <- getRemainingLazyByteString
				return $ ((TLV tag len stuff), rest)
	
	
snac_packet_bs :: SnacPacket -> BS.ByteString
snac_packet_bs sp = runPut $ do
	putWord16be (familyID sp)
	putWord16be (subtypeID sp)
	putWord16be (flags sp)
	putWord32be (reqID sp)
	putLazyByteString (snacData sp)

snac_parse_packet :: BS.ByteString -> SnacPacket 
snac_parse_packet stuff = do
	runGet f stuff
		where
			f :: Get (SnacPacket)
			f = do
				fid <- getWord16be
				sid <- getWord16be
				flgs <- getWord16be
				req <- getWord32be
				rest <- getRemainingLazyByteString
				return $ SnacPacket fid sid flgs req rest
