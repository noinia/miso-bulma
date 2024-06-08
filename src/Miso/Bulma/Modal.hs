{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell          #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Miso.Bulma.Modal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Dealing with Bulma Colors
--
--------------------------------------------------------------------------------
module Miso.Bulma.Modal
  ( Status(..), _InActive, _Active
  , flipStatus

  , ModalAction(..)
  , handleModalAction
  , modal_
  ) where

import Control.Lens hiding (view, element)
import Miso

--------------------------------------------------------------------------------

-- | The status of a modal is either InActive or Active
data Status = InActive | Active
  deriving (Show,Read,Eq,Ord)

makePrisms ''Status

-- | Flip the status
flipStatus :: Status -> Status
flipStatus = \case
  Active   -> InActive
  InActive -> Active

--------------------------------------------------------------------------------

-- | We can toggle a modal
data ModalAction = ToggleModalStatus
  deriving (Show,Read,Eq,Ord)

-- | Handles internal modal actions, in particular support toggling
handleModalAction        :: Status -> ModalAction -> Effect action Status
handleModalAction status = \case
  ToggleModalStatus -> noEff $ flipStatus status



-- | Renders a modal
modal_                :: Status -> [View action] -> View (Either ModalAction action)
modal_ status content =
    div_ [ class_ "modal"
         , class_ $ case status of
                      InActive   -> ""
                      Active     -> "is-active"
         ]
         [ div_ [class_ "modal-background"] []
         , div_ [class_ "modal-content"]
                (fmap Right <$> content)
         , button_ [ class_ "modal-close is-large"
                   -- aria-label "close"
                   , onClick $ Left ToggleModalStatus
                   ] []
         ]
