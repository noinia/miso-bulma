{-# LANGUAGE OverloadedStrings #-}
-- * Helper utils for producing a Navbar
module Miso.Bulma.NavBar
  ( navBarBurger_
  , navBarBurgerItem_
  , navBarSelectedItemA_
  , navBarMenu_
  , navBarStart_
  , navBarEnd_
  , navBarBrand_
  , navBarItemA_
  , navBarDivider_
  ) where

import Miso
import Miso.String (MisoString)

--------------------------------------------------------------------------------

navBarBurger_           :: MisoString -> [View action] -> View action
navBarBurger_ theMenuId = a_ [class_ "navbar-burger"
                             , textProp "role"          "button"
                             , textProp "aria-label"    "menu"
                             , boolProp "aria-expanded" False
                             , textProp "data-target"   theMenuId
                             ]



navBarSelectedItemA_     :: [Attribute action] ->  [View action] -> View action
navBarSelectedItemA_ ats = a_ ([class_ "navbar-item is-selected"] <> ats)


navBarMenu_       :: MisoString -> [View action] -> View action
navBarMenu_ theId = div_ [ class_ "navbar-menu"
                         , id_    theId
                         ]

-- | Navbar start
navBarStart_ :: [View action] -> View action
navBarStart_ = div_ [class_ "navbar-start"]

-- | A horizontal navbar divider
navBarDivider_ :: View action
navBarDivider_ = hr_ [class_ "navbar-divider"]


navBarEnd_ :: [View action] -> View action
navBarEnd_ = div_ [class_ "navbar-end"]


navBarBurgerItem_ :: View action
navBarBurgerItem_ = span_ [boolProp "aria-hidden" True] []

navBarBrand_ :: [View action] -> View action
navBarBrand_ = div_ [class_ "navbar-brand"]

navBarItemA_     :: [Attribute action] -> [View action] -> View action
navBarItemA_ ats = a_ ([class_ "navbar-item"] <> ats)
