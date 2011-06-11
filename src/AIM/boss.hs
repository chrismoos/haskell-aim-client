module AIM.Boss where
	
import Network.Curl
import Network.Curl.Post
import Network.HTTP

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Digest.Pure.SHA
import Codec.Binary.Base64.String
import System.Time

import AIM.Client
import AIM.OpenAuth

import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import Text.XML.HaXml
import Text.XML.HaXml.Xtract.Parse


data AIMBossInfo = AIMBossInfo {host :: String, port :: Int, cookie :: String}

instance Show AIMBossInfo where
	show x = "Boss Server: " ++ (AIM.Boss.host x) ++ ":" ++ (show (AIM.Boss.port x)) ++ ", Cookie: " ++ (cookie x)

process_resp :: String -> IOSArrow b String
process_resp resp = 
	readString [(withValidate no)] resp >>>
	((getXPathTrees "/response/data/host" >>> getChildren >>> getText) <+>
	(getXPathTrees "/response/data/port" >>> getChildren >>> getText)) <+>
	(getXPathTrees "/response/data/cookie" >>> getChildren >>> getText) 

aim_boss_response :: String -> IO (Maybe AIMBossInfo)
aim_boss_response resp = do
	result <- runX (process_resp resp)
	case result of
		(host:(port:(cookie:xs))) -> return $ Just $ AIMBossInfo host (read $ port :: Int) cookie
		_ -> return Nothing

aim_boss_params :: AIMClientInfo -> OAInfo -> IO (String)
aim_boss_params ci info = do
	TOD curTime _ <- getClockTime
	let params = "a=" ++ (urlEncode (token info)) ++ "&clientName=" ++ (urlEncode (name ci)) ++ "&clientVersion=" ++ (urlEncode (show $ clientVersion ci)) ++ "&f=xml&k=" ++ (urlEncode (key ci)) ++ "&ts=" ++ (show $ curTime)
	return params

aim_boss_session :: AIMClientInfo -> OAInfo -> IO (Maybe AIMBossInfo)
aim_boss_session ci info = withCurlDo $ do
	let uri = "http://api.oscar.aol.com/aim/startOSCARSession"
	params <- aim_boss_params ci info
	let hashData = "GET&" ++ (urlEncode uri) ++ "&" ++ (urlEncode params)
	let sig = encode $ BS.unpack $ bytestringDigest $ hmacSha256  (BS.pack $ sessionKey info) (BS.pack hashData)
	let url = uri ++ "?" ++ params ++ "&sig_sha256=" ++ sig
	curl <- initialize
	resp <- do_curl_ curl url method_GET :: IO CurlResponse
	aim_boss_response $ respBody resp