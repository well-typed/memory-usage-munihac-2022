module Handlers (handlers) where

import Data.Text.Lazy         qualified as L

import Web.Scotty.Trans

import Handlers.Languages qualified as Languages
import Handlers.Survey    qualified as Survey
import State

handlers :: ScottyT L.Text HiobeM ()
handlers = mconcat [
    Languages.handlers
  , Survey.handlers
  ]
