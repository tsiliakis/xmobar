-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.LoadAverage
-- Copyright   :  (c) Christos Tsiliakis
-- License     :  BSD-3
--
-- Maintainer  :  Christos Tsiliakis <mail@tsiliakis.net>
-- Stability   :  unstable
-- Portability :  untested
--
-- A simple plugin to display the load average of the system. 
--
-- This plugin is using uptime to retrieve the load average and then parses the result
-- to a definable form. It needs 2 paramters : a String that will be displayed 
-- before the load average (e.g. 'Load average :') and another String, which
-- will be used as the delimiter. E.g. " - ", which will result in 
-- '0.93 - 0.97 - 1.01'. of course a simple space ' ' can also be used).
--
-----------------------------------------------------------------------------

module Plugins.LoadAverage where

import Plugins
import System.Process

data LoadAverage = LoadAverage String String Int deriving (Read,Show)

-- | Calls uptime and displays the results with a definable prefix and delimiter between
-- the load values.
showLoadAverage :: String -> String -> IO String
showLoadAverage preFix delimiter = do (_,out,_) <- readProcessWithExitCode "uptime" [] ""
                                      return $ preFix ++ parse2 delimiter out

-- | Parses uptime's output and delimits the values with the string specified in 
-- showLoadAverage.
parse :: String -> String -> String
parse delimiter = concatMap (\x -> if x == ',' then delimiter else [x]) . concat  . drop 7  . words

parse2 :: String -> String -> String
parse2 delimiter = reverse . concatMap (\x -> if x == ',' then delimiter else [x]) . concat . take 3 . words . reverse

instance Exec LoadAverage where
    alias ( LoadAverage _ _ _ ) = "LoadAverage"
    run   ( LoadAverage p d _ ) = showLoadAverage p d
    rate  ( LoadAverage _ _ r ) = r
