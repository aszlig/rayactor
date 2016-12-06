-- This file is part of the RayActor Lighting Software.
--
-- RayActor is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- RayActor is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
-- more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with Rayactor. If not, see <http://www.gnu.org/licenses/>.
--
module Connection (withConnection, netReceived, Connection) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket, tryJust, SomeException)
import Control.Monad (forever, when)
import Data.Int (Int64)
import Network.Socket
import System.IO (stderr)

import FRP.Elerea.Param (effectful)
import Helm (Sub(Sub))

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy as BS
import qualified Network.Socket.ByteString.Lazy as NBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString.Char8 as BC

type EitherError = Either BC.ByteString

data Connection = Connection
    { queueIn :: STM.TQueue BS.ByteString
    , queueOut :: STM.TQueue BS.ByteString
    , signalExit :: STM.TVar Bool
    }

sender :: Connection -> Socket -> IO ()
sender conn sock = do
    result <- STM.atomically $ do
        val <- STM.tryReadTQueue (queueOut conn)
        shouldExit <- STM.readTVar (signalExit conn)
        case (val, shouldExit) of
             (_, True)    -> return Nothing
             (Nothing, _) -> STM.retry
             (hasVal, _)  -> return hasVal
    case result of
         Nothing -> return ()
         Just buf -> NBS.send sock buf >> sender conn sock

connHandler :: Connection -> Socket -> IO ()
connHandler conn sock = do
    receiver <- forkIO . forever $ do
        buf <- NBS.recv sock 512
        STM.atomically $ STM.writeTQueue (queueIn conn) buf
    sender conn sock
    killThread receiver
    STM.atomically $ STM.writeTVar (signalExit conn) False

tryString :: IO a -> IO (EitherError a)
tryString = tryJust (\e -> Just . BC.pack $ show (e :: SomeException))

safeRecv :: Socket -> Int64 -> IO (EitherError BS.ByteString)
safeRecv sock len = tryString (NBS.recv sock len)

expectSeq :: Socket -> BS.ByteString -> IO (EitherError BS.ByteString)
expectSeq sock expect =
    (>>= checkVal) <$> safeRecv sock (BS.length expect)
  where
    checkVal val = if val == expect then Right val else Left $ mkMsg val
    mkMsg wrong = BC.concat [ BC.pack "Expected "
                            , BC.pack $ show expect
                            , BC.pack " but got "
                            , BC.pack $ show wrong
                            ]

startHandler :: Socket -> IO Connection
startHandler sock = do
    conn <- STM.atomically $ Connection <$> STM.newTQueue
                                        <*> STM.newTQueue
                                        <*> STM.newTVar False
    forkIO (connHandler conn sock) >> return conn

stopHandler :: Connection -> IO ()
stopHandler conn = do
    STM.atomically $ STM.writeTVar (signalExit conn) True
    STM.atomically $ do
        sync <- STM.readTVar (signalExit conn)
        when sync STM.retry

withConnection :: HostName -> ServiceName -> (Connection -> IO ()) -> IO ()
withConnection host port f = withSocketsDo $ do
    (addr:_) <- getAddrInfo Nothing (Just host) (Just port)
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    connect sock (addrAddress addr)
    _ <- NBS.send sock $
        BS.pack [0x52, 0x61, 0x79, 0x43, 0x6c, 0x69, 0x65, 0x6e, 0x74]
    result <- expectSeq sock (LBC.pack "TODO")
    case result of
         Left err -> BC.hPutStrLn stderr $ BC.append (BC.pack msg) err
         Right _  -> bracket (startHandler sock) stopHandler f
  where msg = "Connection to " ++ host ++ ":" ++ port ++ " failed: "

netReceived :: Connection -> (BS.ByteString -> a) -> Sub e a
netReceived conn f = Sub $ effectful $ do
    result <- STM.atomically . STM.tryReadTQueue $ queueIn conn
    case result of
         Just val -> return [f val]
         Nothing -> return []
