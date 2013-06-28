-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Countdown
-- Copyright   :  (c) Christos Tsiliakis
-- License     :  BSD-3
--
-- Maintainer  :  Christos Tsiliakis <mail@tsiliakis.net>
-- Stability   :  unstable
-- Portability :  untested
--
-- A simple plugin to display the remaining days till a specific date.
--
-----------------------------------------------------------------------------

module Plugins.Countdown where

import Data.Time
import Plugins

-- | The Year of the date                                                                                  
type Cyear = Integer
-- | The month of the date
type Cmonth = Int
-- | The day of the date
type Cday = Int

data Countdown = Countdown Cyear Cmonth Cday Int deriving (Read, Show)
   

-- | Counts the days to a specific daye from today                                                             
cdw :: Cyear -> Cmonth -> Cday -> IO String
cdw y m d = do  currentTime <- getCurrentTime
                let currentDay = utctDay currentTime
                let countdownDay = fromGregorian y m d
                return $ show (diffDays countdownDay currentDay) ++ " Days"



instance Exec Countdown where
    alias ( Countdown _ _ _ _ ) = "Countdown"
    run   ( Countdown y m d _ ) = cdw y m d
    rate  ( Countdown _ _ _ r ) = r
