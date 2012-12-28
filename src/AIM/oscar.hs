module AIM.Oscar where
import Prelude hiding (catch)
	
import Network
import System.IO
import Control.Exception (finally, catch, IOException)
import Codec.Binary.Base64.String as B64
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSUTF
import Control.Monad.State
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Char
import Codec.Binary.UTF8.String as BUTF8
import Control.Concurrent
	
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows	
	
import AIM.Boss
import AIM.Flap
import AIM.Util
import AIM.Snac
import AIM.OService
import AIM.Tlv

data Buddy = Buddy {gid :: Word16, iid :: Word16, bName :: String}
data BuddyGroup = BuddyGroup {bgid :: Word16, bgName :: String}

data OscarVars = OscarVars {ohandle :: Handle, seqNum :: Word16, buddies :: [Buddy], groups :: [BuddyGroup], feedbagRecvd :: Bool}
data OscarPacket = OscarPacket {header :: FlapHeader, payload :: BS.ByteString}

type OscarState a = StateT OscarVars IO a


aim_html_extract_body :: String -> String
aim_html_extract_body html = do
	result <- runLA f []
	result
		where
			f = do
				constA html >>> hread >>> getXPathTrees "/html/body" >>> getChildren >>> getText
				

aim_get_group :: Word16 -> OscarState (Maybe BuddyGroup)
aim_get_group gid = do
	s <- Control.Monad.State.get
	let result = filter f (groups s)
	case result of
		(x:xs) -> return $ Just x
		_ -> return Nothing
		where
			f x = (bgid x) == gid


aim_snac_send :: SnacPacket -> OscarState ()
aim_snac_send sp = aim_oscar_send 2 (snac_packet_bs sp)

aim_oscar_send :: Word8 -> BS.ByteString -> OscarState()
aim_oscar_send frame payload = do
	s <- Control.Monad.State.get
	let pck = runPut $ putLazyByteString (flap_header frame (AIM.Oscar.seqNum s) (fromIntegral $ BS.length payload)) >> putLazyByteString payload
	liftIO $ BS.hPut (ohandle s) pck
	liftIO $ hFlush (ohandle s)
	Control.Monad.State.put $ OscarVars (ohandle s) ((AIM.Oscar.seqNum s) + 1) (buddies s) (groups s) (feedbagRecvd s)
	return ()

aim_oscar_recv :: OscarState (OscarPacket)
aim_oscar_recv = do
	s <- Control.Monad.State.get
	header <- liftIO $ BS.hGet (ohandle s) 6
	let pckHeader = flap_parse_header header
	payload <- liftIO $ BS.hGet (ohandle s) (fromIntegral $ payloadLength pckHeader)
	return $ OscarPacket pckHeader payload




aim_oscar_feedbag_query :: OscarState ()
aim_oscar_feedbag_query = aim_snac_send $ SnacPacket 19 4 0 0 BS.empty

{-

Rights

-}

aim_oscar_buddy_rights :: OscarState ()
aim_oscar_buddy_rights = do
	let tlv = tlv_create 5 (runPut $ putWord16be 0xf)
	let snacPacket = SnacPacket 3 2 0 0 tlv
	aim_snac_send snacPacket

aim_oscar_feedbag_rights :: OscarState ()
aim_oscar_feedbag_rights = do
	let tlv = tlv_create 11 (runPut $ putWord16be 0x7f)
	let snacPacket = SnacPacket 19 2 0 0 tlv
	aim_snac_send snacPacket

aim_oscar_pd_rights :: OscarState ()
aim_oscar_pd_rights = aim_snac_send $ SnacPacket 9 2 0 0 BS.empty

aim_oscar_location_rights :: OscarState ()
aim_oscar_location_rights = aim_snac_send $ SnacPacket 2 2 0 0 BS.empty


oscar_group_version :: Word16 -> Word16 -> Word16 -> Word16 -> BS.ByteString
oscar_group_version f v ti tv = runPut $ putWord16be f >> putWord16be v >> putWord16be ti >> putWord16be tv

oscar_group_versions :: BS.ByteString
oscar_group_versions = do
	let gv = [oscar_group_version 1 0x4 0x41 0xaa] ++ [oscar_group_version 0x19 0x3 0x41 0xaa] ++ [oscar_group_version 0x3 0x1 0x41 0xaa] ++ [oscar_group_version 0x2 0x1 0x41 0xaa] ++ [oscar_group_version 0x6 0x1 0x41 0xaa] ++ [oscar_group_version 0x4 0x1 0x41 0xaa]
	BS.concat gv

aim_oscar_client_online :: OscarState ()
aim_oscar_client_online = do
	aim_snac_send $ SnacPacket 19 7 0 0 BS.empty
	aim_snac_send $ SnacPacket 1 2 0 0 oscar_group_versions


process_feedbag_item :: String -> Word16 -> Word16 -> Word16 -> BS.ByteString -> OscarState ()

{-process_feedbag_item 0 0 1 block = do-}

{- Buddy Group Name -}	
process_feedbag_item name gid 0 1 block = do
	s <- Control.Monad.State.get
	Control.Monad.State.put $ OscarVars (ohandle s) (AIM.Oscar.seqNum s) (buddies s) ((groups s) ++ [BuddyGroup gid name]) (feedbagRecvd s)

{- Buddy -}
process_feedbag_item name gid iid 0 block = do
	s <- Control.Monad.State.get
	name <- aim_get_group gid
	case name of
		Just j -> addBuddy (bgName j) s
		Nothing -> addBuddy "Unknown group" s
	where
		addBuddy x s = do
			Control.Monad.State.put $ OscarVars (ohandle s) (AIM.Oscar.seqNum s) ((buddies s) ++ [Buddy gid iid x]) (groups s) (feedbagRecvd s)
	
process_feedbag_item name gid iid cid block = do
	liftIO $ putStrLn $ "Name: " ++ name ++ ", Group ID: " ++ (show gid) ++ ", Item ID: " ++ (show iid) ++ ", Class ID: " ++ (show cid)

get_feedbag_items :: Int -> BS.ByteString -> OscarState ()
get_feedbag_items 0 stuff = do
	s <- Control.Monad.State.get
	let bs = (buddies s)
	let gs = (groups s)
	liftIO $ putStrLn "End of feedbag items."
	liftIO $ sequence_ [do { num <- countBuddies bs x; putStrLn ("Group: " ++ (bgName x) ++ ", Buddies: " ++ (show num)) } | x <- gs]
		where
			countBuddies bs group = return $ length $ filter (\y -> (bgid group) == (gid y)) bs
	
get_feedbag_items num stuff = do
	let (((name, groupid), (itemid, classid)), (tlvBlock,rest)) = runGet f stuff
	process_feedbag_item name groupid itemid classid tlvBlock
	get_feedbag_items (num-1) rest	
		where
			f = do
				nLength <- getWord16be
				nStr <- getLazyByteString (fromIntegral nLength)
				gid <- getWord16be
				iid <- getWord16be
				cid <- getWord16be
				blockLen <- getWord16be
				tlvBlock <- getLazyByteString (fromIntegral blockLen)
				rest <- getRemainingLazyByteString
				return ((((BUTF8.decode (BS.unpack nStr)), gid), (iid, cid)), (tlvBlock, rest))

snac_handle :: Word16 -> Word16 -> SnacPacket -> OscarState ()
{-

SNAC Handlers - 0x0001
Main

-}

snac_handle 1 3 pck = do
	liftIO $ putStrLn "Login accepted by OSCAR server. Setting up rights."
	aim_oscar_buddy_rights
	aim_oscar_pd_rights
	aim_oscar_location_rights
	aim_oscar_feedbag_rights
	aim_oscar_feedbag_query
	
	
{-

SNAC Handlers - 0x0002
Locate

-}

{- SNAC: LOCATE__RIGHTS_REPLY - Foodgroup:2 Type:3 -}
snac_handle 2 3 pck = do
	liftIO $ sequence_ [tg x | x <- tlv_extract_lblock (snacData pck)]
		where
			tg x = do
				case (tag x) of
					1 -> putStrLn $ "Max Signature Length: " ++ (show $ runGet (getWord16be) (value x))
					2 -> putStrLn $ "Max Capabilities Length: " ++ (show $ runGet (getWord16be) (value x))
					3 -> putStrLn $ "Max Find By Email List: " ++ (show $ runGet (getWord16be) (value x))
					4 -> putStrLn $ "Max CERT length: " ++ (show $ runGet (getWord16be) (value x))
					5 -> putStrLn $ "Max Short Capabilities: " ++ (show $ runGet (getWord16be) (value x))
					_ -> return ()

{-

SNAC Handlers - 0x0003
Buddy
-}

{- SNAC: BUDDY__RIGHTS_REPLY - Foodgroup:3 Type:3 -}
snac_handle 3 3 pck = do
	liftIO $ sequence_ [tg x | x <- tlv_extract_lblock (snacData pck)]
		where
			tg x = do
				case (tag x) of
					1 -> putStrLn $ "Max Buddies: " ++ (show $ runGet (getWord16be) (value x))
					2 -> putStrLn $ "Max Watchers: " ++ (show $ runGet (getWord16be) (value x))
					4 -> putStrLn $ "Max Temp Buddies: " ++ (show $ runGet (getWord16be) (value x))
					_ -> return ()
	
{- SNAC: BUDDY__ARRIVED - Foodgroup:3 Type:11 -}
snac_handle 3 11 pck = do
	let ((loginID, evil), rest) = runGet f (snacData pck)
	liftIO $ putStrLn $ loginID ++ " is now online."
		where 
			f :: Get ((String, Word16), BS.ByteString)
			f = do
				nickLength <- getWord8
				name <- getLazyByteString (fromIntegral nickLength)
				evil <- getWord16be
				rest <- getRemainingLazyByteString
				return ((BSC.unpack name, evil), rest)

{- SNAC: BUDDY__DEPARTED - Foodgroup:3 Type:12 -}
snac_handle 3 12 pck = do
	let (nickinfo, rest) = oservice_nick_info (snacData pck)
	liftIO $ putStrLn $ show nickinfo
	liftIO $ putStrLn $ (displayId nickinfo) ++ " is offline."

{- SNAC: BUDDY__REJECT_NOTIFICATION - Foodgroup:3 Type:10 -}
snac_handle 3 10 pck = do
	liftIO $ putStrLn $ "Buddies Rejected: " ++ (hex_str (snacData pck))


{- 

SNAC Handlers - 0x0004
ICBM

-}

{- SNAC: ICBM__CHANNEL_MSG_TOCLIENT - Foodgroup:4 Type:7 -}
snac_handle 4 7 pck = do
	let ((cookie, channel), (nickinfo, tlvs)) = runGet f (snacData pck)
	liftIO $ putStrLn $ "From: " ++ (displayId nickinfo) ++ ", Message: " ++ (aim_html_extract_body $ getMsg tlvs)
	return ()
		where
			f = do
				cookie <- getLazyByteString 8
				channel <- getWord16be
				stuff <- getRemainingLazyByteString
				let (nickinfo, rest) = oservice_nick_info stuff
				let tlvs = tlv_extract_lblock rest
				return ((cookie, channel), (nickinfo, tlvs))
			getMsg :: [TLV] -> String
			getMsg tlvs = do
				case filter (\x -> (tag x) == 2) tlvs of
					[] -> "No message data :("
					(x:xs) -> do
						let tlvs = tlv_extract_lblock (value x)
						case filter (\x -> (tag x) == 0x0101) tlvs of
							[] -> "No IM_TEXT TLV :("
							(y:ys) -> runGet getImData (value y)
			getImData = do
				encoding <- getWord16be
				language <- getWord16be
				msg <- getRemainingLazyByteString
				return $ BSC.unpack msg
				
						

{-

SNAC Handlers - 0x0009
PD (Permit/Deny)

-}

{- SNAC: PD__RIGHTS_REPLY - Foodgroup:9 Type:3 -}
snac_handle 9 3 pck = do
	liftIO $ sequence_ [tg x | x <- tlv_extract_lblock (snacData pck)]
		where
			tg x = do
				case (tag x) of
					1 -> putStrLn $ "Max Permits: " ++ (show $ runGet (getWord16be) (value x))
					2 -> putStrLn $ "Max Denies: " ++ (show $ runGet (getWord16be) (value x))
					3 -> putStrLn $ "Max Temp Permits: " ++ (show $ runGet (getWord16be) (value x))
					_ -> return ()

{-

SNAC Handlers - 0x0019
Feedbag

-}

{- SNAC: FEEDBAG__RIGHTS_REPLY - Foodgroup:19 Type:3 -}
snac_handle 19 3 pck = do
	liftIO $ sequence_ [tg x | x <- tlv_extract_lblock (snacData pck)]
		where
			tg x = do
				case (tag x) of
					3 -> putStrLn $ "Max Item Attributes: " ++ (show $ runGet (getWord16be) (value x))
					4 -> putStrLn $ "Max items by class: NULL for now"
					5 -> putStrLn $ "Max Client Items: " ++ (show $ runGet (getWord16be) (value x))
					6 -> putStrLn $ "Max Length Item Name: " ++ (show $ runGet (getWord16be) (value x))
					7 -> putStrLn $ "Max Recent Buddies: " ++ (show $ runGet (getWord16be) (value x))
					8 -> putStrLn $ "Max Interactions w/ Buddies: " ++ (show $ runGet (getWord16be) (value x))
					9 -> putStrLn $ "Half Life: " ++ (show $ runGet (getWord32be) (value x))
					10 -> putStrLn $ "Max Score: " ++ (show $ runGet (getWord32be) (value x))
					12 -> putStrLn $ "Max Buddies Per Group: " ++ (show $ runGet (getWord16be) (value x))
					13 -> putStrLn $ "Max Mega Bots: " ++ (show $ runGet (getWord16be) (value x))
					14 -> putStrLn $ "Max Smart Groups: " ++ (show $ runGet (getWord16be) (value x))
					_ -> putStrLn $ "Unknown tag: " ++ (show (tag x))
					
{- SNAC: FEEDBAG__REPLY - Foodgroup:19 Type:6 -}


snac_handle 19 6 pck = do
	let ((numClasses, numItems), rest) = runGet replyHead (snacData pck)
	liftIO $ putStrLn $ "Number of Feedbag Items: " ++ (show numItems)
	get_feedbag_items (fromIntegral numItems) rest
	s <- Control.Monad.State.get 
	case (feedbagRecvd s) of
		False -> do
			Control.Monad.State.put $ OscarVars (ohandle s) (AIM.Oscar.seqNum s) (buddies s) (groups s) True
			aim_oscar_client_online
		True -> return ()
		where
			replyHead :: Get ((Word8, Word16), BS.ByteString)
			replyHead = do
				x <- getWord8
				y <- getWord16be
				rest <- getRemainingLazyByteString
				return $ ((x,y), rest)


{-

Handle SNAC Packet

-}

snac_handle fid sid pck = do
	liftIO $ putStrLn $ "Unable to handle SNAC Packet, Family ID: " ++ (show fid) ++ ", Subtype ID: " ++ (show sid)

{-

Handle Frames

-}
aim_oscar_handle_frame :: Word8 -> BS.ByteString -> OscarState ()

{- data frame w/ snac header -}
aim_oscar_handle_frame 2 payload = do
	let snacPck = snac_parse_packet payload	
	snac_handle (familyID snacPck) (subtypeID snacPck) snacPck
	
	return ()
	
{- unknown frame -}	
aim_oscar_handle_frame t payload = do
	liftIO $ putStrLn $ "Unknown frame type: " ++ (show t)
	return ()

aim_oscar_handle :: OscarPacket -> OscarState ()
aim_oscar_handle pck = do
	aim_oscar_handle_frame (frameType (header pck)) (payload pck)
	return ()

aim_oscar_loop :: OscarState ()
aim_oscar_loop = do
	
	packet <- aim_oscar_recv
	aim_oscar_handle packet
	aim_oscar_loop

{-

AIM OSCAR Sign-On

-}

aim_oscar_signon :: String -> OscarState ()
aim_oscar_signon cookie = do
	aim_oscar_send (fromIntegral $ flap_frame_type_signon()) $ flap_signon cookie
	packet <- aim_oscar_recv
	case (frameType (header packet)) of
		1 -> do
			liftIO $ putStrLn "Entered OSCAR client loop."
			aim_oscar_loop
		_ -> liftIO $ putStrLn "Received an invalid reply from the OSCAR server."
	return ()
	
aim_oscar_connect :: AIMBossInfo -> IO ()
aim_oscar_connect info = do
	handle <- connectTo (AIM.Boss.host info) (PortNumber $ fromIntegral $ AIM.Boss.port info)
	putStrLn "Connected to AIM server."
	x <- runStateT (aim_oscar_signon (B64.decode (cookie info))) (OscarVars handle 10 [] [] False)
	return ()
	
aim_oscar_login :: AIMBossInfo -> IO ()
aim_oscar_login info = withSocketsDo $ do
 	aim_oscar_connect info `catch` handleErr
	where 
		handleErr e = putStrLn $ "AIM server error: " ++ (show (e :: IOException))
	
