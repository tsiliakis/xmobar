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
-- This plugin needs 5 parameters : The date's Year Month Day and a string
-- that can be shown after the difference of days. The recommendet value for
-- the string is ' Days'. The last parameter is the update rate of the plugin
-- in tenth of seconds.
--
-- It can be used as a countdown till e.g. New Years or vacations. If
-- you set the date to New Years you automatically get the day of the year.
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

data Countdown = Countdown Cyear Cmonth Cday String Int deriving (Read, Show)
   

-- | Counts the days to a specific date from today. This function returns the number 
-- of days followed by a String
cdw :: Cyear -> Cmonth -> Cday -> String -> IO String
cdw y m d s = do  currentTime <- getCurrentTime
                  let currentDay = utctDay currentTime
                  let countdownDay = fromGregorian y m d
                  return $ show (diffDays countdownDay currentDay) ++ s



instance Exec Countdown where
    alias ( Countdown _ _ _ _ _ ) = "Countdown"
    run   ( Countdown y m d s _ ) = cdw y m d s
    rate  ( Countdown _ _ _ _ r ) = r
