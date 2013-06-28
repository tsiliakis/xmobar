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
-- A simple plugin to display the number of packages that can be updated.
--
-----------------------------------------------------------------------------

module Plugins.AptUpdates where

import Plugins
import System.Process

data AptUpdates = AptUpdates String Int deriving (Read,Show)

showUpdates :: String -> IO String
showUpdates postFix = do (_,out,_) <- readProcessWithExitCode "aptitude" ["search","~U"] ""
                         let nr = cNewLines out
                         return $ nr ++ postFix
                 

cNewLines :: String -> String
cNewLines str = show $ length $ lines str

instance Exec AptUpdates where
    alias ( AptUpdates _ _ ) = "AptUpdates"
    run   ( AptUpdates s _ ) = showUpdates s
    rate  ( AptUpdates _ r ) = r