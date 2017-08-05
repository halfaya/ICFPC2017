module Main where

import Network.Simple.TCP
import Data.ByteString.Char8 as BS (ByteString, pack, unpack, break, length, append, tail)
import Data.ByteString.Lazy.Char8 as LBS (unpack, fromStrict, toStrict)
import Data.Aeson.Encode.Pretty
import Data.Aeson (Value, decode, encode, toJSON, fromJSON, Result(..))
import Data.Maybe (fromJust)
import Control.Concurrent (threadDelay)
import GHC.Generics
import System.Environment

import Data
import Graph
import Strategy

hostname = "punter.inf.ed.ac.uk"

maxMessageLength :: Int
maxMessageLength = 1000000

prependLength :: ByteString -> ByteString
prependLength bs = pack (show (BS.length bs) ++ ":" ++ BS.unpack bs)

me :: ByteString
me = pack "{\"me\" : \"half\"}"

ready :: Int -> ByteString
ready punterId = pack $ "{\"ready\" : " ++ show punterId ++ " }"

parseLength :: ByteString -> (Int, ByteString)
parseLength bs =
  let (bs1, bs') = BS.break (== ':') bs
      bs2        = BS.tail bs'
  in (read (BS.unpack bs1) - BS.length bs2, bs2)

getMessage :: Socket -> IO Value
getMessage socket = do
  Just bs1 <- recv socket maxMessageLength
  let (n, bs) = parseLength bs1
  Just bs2 <- if n > 0 then recv socket n else return $ Just (pack "")
  let bs3 = append bs bs2
  let val = fromJust $ decode (fromStrict bs3) :: Value
  --putStrLn (LBS.unpack (encodePretty $ toJSON val))
  return val
  
tcpGame :: String -> IO ()
tcpGame port = connect hostname port $ \(socket, remoteAddr) -> do
  putStrLn $ "Connection established to " ++ show remoteAddr
  send socket (prependLength me)
  getMessage socket -- handshake
  val <- getMessage socket -- initial state
  let initialState = fromJSON val :: Result InitialState
  putStrLn (show initialState)
  case initialState of
    Error s    -> putStrLn s
    Success is -> do
      send socket (prependLength (ready (punter is)))
      turns socket (punter is) (riversToEdges (rivers (Data.map is)))
  return ()

turn :: Int -> Edges -> Value -> IO (Maybe (Move, Edges))
turn punterId es v =
  let rsm = fromJSON v :: Result ServerMessage
  in case rsm of
    Error s           -> putStrLn s         >> return Nothing
    Success (Move sm) -> do
      putStrLn (show sm)
      let usedEdges = serverMoveToEdges sm
      let newEdges  = removeEdges es usedEdges
      case pickEdge newEdges of
        Nothing -> return (Just (Pass (PassMove punterId), newEdges))
        Just e  -> return (Just (Claim (edgeToClaimMove punterId e), newEdges))
    Success (Stop st) -> putStrLn (show st) >> return Nothing

turns :: Socket -> Int -> Edges -> IO ()
turns socket punterId es = do
  putStrLn "turns"
  val <- getMessage socket
  --putStrLn (LBS.unpack (encodePretty $ toJSON val))
  mmove <- turn punterId es val
  case mmove of
    Nothing -> return ()
    Just (move, edges) -> do
      putStrLn (show move)
      putStrLn (show (encode move))
      putStrLn (show edges)
      send socket (prependLength (toStrict (encode move)))
      turns socket punterId edges

main :: IO ()
main = do
  args <- getArgs
  tcpGame (head args)
