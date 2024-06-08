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
  , toggleStatus
  , HasStatus(..)

  , ModalAction(..)
  , handleModalAction
  , modal_

  , modalCard_
  ) where

import Control.Lens hiding (view, element)
import Miso
import Miso.String (MisoString)

--------------------------------------------------------------------------------

-- | The status of a modal is either InActive or Active
data Status = InActive | Active
  deriving (Show,Read,Eq,Ord)

makePrisms ''Status

-- | Flip the status
toggleStatus :: Status -> Status
toggleStatus = \case
  Active   -> InActive
  InActive -> Active


class HasStatus t where
  -- | Lens to access the status
  status :: Lens' t Status


--------------------------------------------------------------------------------

-- | We can toggle a modal
data ModalAction = ToggleModalStatus
  deriving (Show,Read,Eq,Ord)

-- | Handles internal modal actions, in particular support toggling
handleModalAction         :: Status -> ModalAction -> Effect action Status
handleModalAction status' = \case
  ToggleModalStatus -> noEff $ toggleStatus status'



-- | Renders a modal
modal_                :: Status -> [View action] -> View (Either ModalAction action)
modal_ status' content =
  modalWith status'
    [ div_ [class_ "modal-content"]
           (fmap Right <$> content)
    , button_ [ class_ "modal-close is-large"
              , textProp "aria-label" "close"
              , onClick $ Left ToggleModalStatus
              ] []
    ]

-- | Renders a modal card
modalCard_                                         :: Status
                                                   -> MisoString -- ^ the title
                                                   -> [View action] -- ^ body content
                                                   -> [View action] -- ^ footer content
                                                   -> View (Either ModalAction action)
modalCard_ status' title bodyContent footerContent =
  modalWith status'
    [ div_ [class_ "modal-card"]
           [ header_ [class_ "modal-card-head"]
                     [ p_ [class_ "modal-card-title"]
                          [text title]
                     , button_ [ class_ "delete"
                               , textProp "aria-label" "close"
                               , onClick $ Left ToggleModalStatus
                               ] []
                     ]
           , section_ [class_ "modal-card-body"]
                      (fmap Right <$> bodyContent)
           , footer_ [class_ "modal-card-foot"]
                     (fmap Right <$> footerContent)
           ]
    ]

-- | Helper function to implement the common stuff of modal and modalCard
modalWith                 :: Status -> [View action] -> View action
modalWith status' content =
    div_ [ class_ "modal"
         , class_ $ case status' of
                      InActive   -> ""
                      Active     -> "is-active"
         ]
         ( div_ [class_ "modal-background"] []
         : content
         )
