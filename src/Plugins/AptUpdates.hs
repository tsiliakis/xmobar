-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.AptUpdates
-- Copyright   :  (c) Christos Tsiliakis
-- License     :  BSD-3
--
-- Maintainer  :  Christos Tsiliakis <mail@tsiliakis.net>
-- Stability   :  unstable
-- Portability :  debian
--
-- A simple plugin to display the number of packages that can be updated via
-- aptitude. 
--
-- The plugin needs 2 parameters : a string to display after the number
-- of updatable packages and the rate the plugin should check for new packages
-- in tenth of seconds.
--
-- It's best when you have a cron-job to update the package database
-- (aptitude update) and use this plugin to show the packages that are available 
-- to be updated. This plugin does not(!) automatically update your repository, since
-- root rights are needed for that.
--
-- Since it uses aptitude, you have to run a system with aptitude.
-- This plugin was tested on Debian Wheezy 3.2.0-4-amd64 and was build with 
-- aptitude 0.6.8.2 in mind.
--
-----------------------------------------------------------------------------

module Plugins.AptUpdates where

import Plugins
import System.Process


data AptUpdates = AptUpdates String Int deriving (Read,Show)

-- | Executes aptitude and get's the packages that can be updated over StdOut which is then 
-- fed to cNewLines. 
showUpdates :: String -> IO String
showUpdates postFix = do (_,out,_) <- readProcessWithExitCode "aptitude" ["search","~U"] ""
                         let nr = cNewLines out
                         return $ nr ++ postFix
                 
-- | Counts the lines of a string and returns the number.
cNewLines :: String -> String
cNewLines str = show $ length $ lines str

instance Exec AptUpdates where
    alias ( AptUpdates _ _ ) = "AptUpdates"
    run   ( AptUpdates s _ ) = showUpdates s
    rate  ( AptUpdates _ r ) = r
