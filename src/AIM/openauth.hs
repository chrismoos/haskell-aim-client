module AIM.OpenAuth where

import Network.HTTP
import Network.URI
import Network.HTTP.Headers
import Network.Curl
import Network.Curl.Post

import AIM.Client

import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Digest.Pure.SHA
import Codec.Binary.Base64.String

data OAInfo = OAInfo {token :: String, sessionKey :: String}

instance Show OAInfo where
	show x = "Token: " ++ (token x) ++ ", Session Key: " ++ (sessionKey x)

oa_client_login_response :: String -> (Either String (String, String))
oa_client_login_response resp = do
	case runLA status [] of 
		[] -> Left "Invalid OpenAuth Server response."
		(("200",_):xs) -> do
			case runLA sess [] of
				[] -> Left "Invalid OpenAuth Server response."
				((token,sess):xs) -> Right $ (token, sess)
		((_, statusText):xs) -> do
			Left statusText
		
		
	where 
		status = 
			constA resp >>> xread 
			>>> 
			((getXPathTrees "/response/statusCode" >>> getChildren >>> getText) &&&
			(getXPathTrees "/response/statusText" >>> getChildren >>> getText))
		sess = 
			constA resp >>> xread 
			>>> 
			(getXPathTrees "/response/data/token/a" >>> getChildren >>> getText) &&&
			(getXPathTrees "/response/data/sessionSecret" >>> getChildren >>> getText)
		

oa_client_login_opts :: String -> String -> String -> String -> String -> [CurlOption]
oa_client_login_opts app_key sn pwd app_version app_name = 
	[(CurlPost True), (CurlPostFields ["k=" ++ app_key, "s=" ++ sn, "pwd=" ++ pwd, "clientVersion=" ++ app_version, "clientName=" ++ app_name])]

oa_client_login :: AIMClientInfo -> String -> String -> IO (Maybe OAInfo)
oa_client_login ci sn pwd = withCurlDo $ do
	let url = "https://api.screenname.aol.com/auth/clientLogin?f=xml"
	let login_opts = oa_client_login_opts (key ci) sn pwd (show $ clientVersion ci) (name ci)
	
	curl <- initialize
	resp <- do_curl_ curl url login_opts :: IO CurlResponse
	case oa_client_login_response $ respBody resp of
		Right (t,s) -> do
			let sessKey = encode $ BS.unpack $ bytestringDigest $ hmacSha256 (BS.pack pwd) (BS.pack s)
			return $ Just $ OAInfo t sessKey
		Left err -> do
			putStrLn $ "Open Auth Error: " ++ err
			return Nothing

aim_open_auth :: AIMClientInfo -> String -> String -> IO (Maybe OAInfo)
aim_open_auth clientInfo screenName password = do
	oa_client_login clientInfo screenName password