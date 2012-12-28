module AIM.OService where
	
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Word
import Data.Char
import Data.Binary.Put
import Data.Binary.Get

import AIM.Snac
import AIM.Tlv

data NickwInfo = NickwInfo {displayId :: String, evil :: Word16, userAttr :: [TLV]}

instance Show NickwInfo where
	show x = "Display ID: " ++ (displayId x) ++ ", Evil: " ++ (show $ evil x) ++ ", TLVs: " ++ (show $ userAttr x)	

oservice_nick_info :: BS.ByteString -> (NickwInfo, BS.ByteString)
oservice_nick_info stuff = do 
	runGet f stuff
		where 
			f = do
				nickLength <- getWord8
				name <- getLazyByteString (fromIntegral nickLength)
				evil <- getWord16be
				numTLV <- getWord16be
				stuff <- getRemainingLazyByteString
				let (tlvs, rest) = tlv_extract_block (fromIntegral numTLV) stuff
				return $ (NickwInfo (BSC.unpack name) evil tlvs, rest)
