-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Actions
-- Copyright   :  (c) Alexander Polakov
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Actions (Action(..), runAction, stripActions) where

import System.Process (system)
import Control.Monad (void)
import Text.Regex (subRegex, mkRegex)

data Action = Spawn String
                deriving (Eq)

runAction :: Action -> IO ()
runAction (Spawn s) = void $ system (s ++ "&")

stripActions :: String -> String
stripActions s = subRegex actionRegex s "[action=\1]\2[action]"
  where actionRegex = mkRegex "<action=([^>])*>(.+?)</action>"
