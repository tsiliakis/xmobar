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
-----------------------------------------------------------------------------

module Plugins.LoadAverage where

import Plugins
import System.Process

data LoadAverage = LoadAverage String String Int deriving (Read,Show)

showLoadAverage :: String -> String -> IO String
showLoadAverage preFix delimiter = do (_,out,_) <- readProcessWithExitCode "uptime" [] ""
                                      return $ preFix ++ parse delimiter out


parse :: String -> String -> String
parse delimiter = concatMap (\x -> if x == ',' then delimiter else [x]) . concat  . drop 7  . words

instance Exec LoadAverage where
    alias ( LoadAverage _ _ _ ) = "LoadAverage"
    run   ( LoadAverage p d _ ) = showLoadAverage p d
    rate  ( LoadAverage _ _ r ) = r