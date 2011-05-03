module Main where
	
import Control.Concurrent	
	
import AIM.OpenAuth	
import AIM.Client
import AIM.Boss
import AIM.Oscar
import System.IO


getScreenName :: IO (String)
getScreenName = do
	putStr "AIM Screen Name> "
	hFlush stdout
	getLine

getPassword :: IO (String)
getPassword = do
	putStr "AIM Password> "
	hFlush stdout
	hSetEcho stdout False
	password <- getLine
	hSetEcho stdout False
	putChar '\n'
	return password

getAppKey :: IO (String)
getAppKey = do
	putStrLn "An Open AIM Developer key is required: http://developer.aim.com/manageKeys.jsp"
	putStr "Open AIM Key> "
	hFlush stdout
	getLine

start_aim :: IO ()
start_aim = do
	putStrLn "Haskell AIM Client - OSCAR Protocol"
	appKey <- getAppKey
	screenName <- getScreenName
	password <- getPassword
	let clientInfo = AIMClientInfo 1 "HaskellClient" appKey
	result <- aim_open_auth clientInfo screenName password
	case result of
		Just info -> do
			boss_result <- aim_boss_session clientInfo info
			case boss_result of
				Just si -> aim_oscar_login si
				Nothing -> putStrLn "Unable to authenticate with BOSS server."
		Nothing -> return ()
	
main :: IO ()
main = do
	start_aim
	