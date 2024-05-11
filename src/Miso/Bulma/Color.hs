{-# LANGUAGE OverloadedStrings          #-}
module Miso.Bulma.Color
  ( BulmaColor(..)
  , colorClass
  ) where

import Miso.String(MisoString)

--------------------------------------------------------------------------------

-- | The Bulma Color names
data BulmaColor = Primary
                | Secondary
                | Dark
                | Link
                | Info
                | Success
                | Warning
                | Danger
                deriving (Show,Read,Eq)

colorClass :: BulmaColor -> MisoString
colorClass = \case
  Primary   -> "is-primary"
  Secondary -> "is-seconary"
  Dark      -> "is-dark"
  Link      -> "is-link"
  Info      -> "is-info"
  Success   -> "is-success"
  Warning   -> "is-warning"
  Danger    -> "is-danger"
