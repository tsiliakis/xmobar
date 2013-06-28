-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.ShowIP
-- Copyright   :  (c) Christos Tsiliakis
-- License     :  BSD-3
--
-- Maintainer  :  Christos Tsiliakis <mail@tsiliakis.net>
-- Stability   :  unstable
-- Portability :  untested
--
-- A simple plugin to display the external IP or any other String from a webserver.
--
-----------------------------------------------------------------------------

module Plugins.ShowIP where

import Plugins
import Network.HTTP


data ShowIP = ShowIP String Int deriving (Read, Show)

-- | Retrieves the string from the given address.
-- Note, that the webserver has to reply with a simple string.
getIP :: IO String
getIP = do ip <- simpleHTTP (getRequest "http://srv.u0.org/ip.php") >>=  fmap (take 100) . getResponseBody
           return $ stripWspc ip
                      
-- | Clears a string from spaces, new lines or tabs
stripWspc :: String -> String
stripWspc [] = []
stripWspc list = filter (\x -> notElem x " \n\t") list
                                                                   
-- | Returns the string with a prefix string.
printIP :: String -> IO String                       
printIP preFix = do ip <- getIP
                    return $ preFix ++ ip
                        
instance Exec ShowIP where
    alias ( ShowIP _ _ ) = "ShowIP"
    run   ( ShowIP s _ ) = printIP s
    rate  ( ShowIP _ r ) = r