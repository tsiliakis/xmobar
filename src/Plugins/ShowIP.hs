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
-- This plugin can be pointed to a website, retrieves its content with a Http GET and
-- displays the string. It needs 3 parameters : a String, which is the prefix
-- (e.g. 'IP : '), a String which is the url of the server and a int which is the 
-- rate the plugin updates in tenths of seconds.
--
-- As a simple server that returns a IP, feel free to use my server at 
-- http://srv.u0.org/ip.php , but I can't guarantee that it will be up forever.
-- An alternative is http://wtfismyip.com/text.
--
-----------------------------------------------------------------------------

module Plugins.ShowIP where

import Plugins
import Network.HTTP


data ShowIP = ShowIP String String Int deriving (Read, Show)

-- | Retrieves the string from the given address.
-- Note, that the webserver has to reply with a simple string.
getIP :: String -> IO String
getIP url = do ip <- simpleHTTP (getRequest url ) >>=  fmap (take 100) . getResponseBody
               return $ stripWspc ip
                      
-- | Clears a string from spaces, new lines or tabs
stripWspc :: String -> String
stripWspc [] = []
stripWspc list = filter (\x -> notElem x " \n\t") list
                                                                   
-- | Returns the string with a prefix string.
printIP :: String -> String -> IO String                       
printIP preFix url = do ip <- getIP url
                        return $ preFix ++ ip
                        
instance Exec ShowIP where
    alias ( ShowIP _ _ _ ) = "ShowIP"
    run   ( ShowIP s u _ ) = printIP s u
    rate  ( ShowIP _ _ r ) = r