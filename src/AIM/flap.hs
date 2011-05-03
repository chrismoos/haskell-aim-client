module AIM.Flap where
	
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Word
import Char
import Data.Binary.Put
import Data.Binary.Get

{- FLAP Frame Types -}
flap_frame_type_signon = const 1

flap_signon_tags_login_cookie = 6
flap_signon_tags_multiconn_flags = 74

data FlapHeader = FlapHeader {frameType :: Word8, seqNum :: Word16, payloadLength :: Word16}

instance Show FlapHeader where
	show x = "FLAP Header: Frame Type: " ++ (show (frameType x)) ++ ", Sequence Num: " ++ (show (seqNum x)) ++ ", Payload Length: " ++ (show (payloadLength x))


flap_parse_header :: BS.ByteString -> FlapHeader
flap_parse_header stuff = do
    runGet f stuff
		where
			f :: Get (FlapHeader)
			f = do
				frame <- getWord8 >> getWord8
				seqnum <- getWord16be
				pl <- getWord16be
				return $ FlapHeader frame seqnum pl

flap_make_tlv :: Word16 -> BS.ByteString -> BS.ByteString
flap_make_tlv tag value = runPut $ putWord16be tag >> putWord16be (fromIntegral $ BS.length value) >> putLazyByteString value

flap_signon :: String -> BS.ByteString
flap_signon cookie = do
	let cookieTLV = flap_make_tlv flap_signon_tags_login_cookie (BSC.pack cookie)
	let multiconnTLV = flap_make_tlv flap_signon_tags_multiconn_flags (BSC.singleton $ chr 0x01)
	flap_signon_frame [cookieTLV, multiconnTLV]

flap_signon_frame :: [BS.ByteString] -> BS.ByteString
flap_signon_frame tlvs = runPut $ do
	putWord32be 1
	putLazyByteString $ BS.concat tlvs
	
flap_header :: Word8 -> Word16 -> Word16 -> BS.ByteString
flap_header frame seqNum pLength = runPut $ do
	putWord8 0x2a
	putWord8 frame
	putWord16be seqNum
	putWord16be pLength