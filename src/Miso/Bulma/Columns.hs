{-# LANGUAGE OverloadedStrings          #-}
module Miso.Bulma.Columns
  ( columns_
  , column_
  ) where


import Miso

--------------------------------------------------------------------------------

-- | A Section with Columns
columns_     :: [Attribute action] -> [View action] -> View action
columns_ ats = section_ ([class_ "columns"] <> ats)

-- | A single column
column_     :: [Attribute action] -> [View action] -> View action
column_ ats = div_ ([class_ "column"] <> ats)
