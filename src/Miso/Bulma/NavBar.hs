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
import Miso.Html.Element
import Miso.Html.Property

--------------------------------------------------------------------------------

navBarBurger_           :: MisoString -> [View model action] -> View model action
navBarBurger_ theMenuId = a_ [class_ "navbar-burger"
                             , textProp "role"          "button"
                             , textProp "aria-label"    "menu"
                             , boolProp "aria-expanded" False
                             , textProp "data-target"   theMenuId
                             ]



navBarSelectedItemA_     :: [Attribute action] ->  [View model action] -> View model action
navBarSelectedItemA_ ats = a_ ([class_ "navbar-item is-selected"] <> ats)


navBarMenu_       :: MisoString -> [View model action] -> View model action
navBarMenu_ theId = div_ [ class_ "navbar-menu"
                         , id_    theId
                         ]

-- | Navbar start
navBarStart_ :: [View model action] -> View model action
navBarStart_ = div_ [class_ "navbar-start"]

-- | A horizontal navbar divider
navBarDivider_ :: View model action
navBarDivider_ = hr_ [class_ "navbar-divider"]


navBarEnd_ :: [View model action] -> View model action
navBarEnd_ = div_ [class_ "navbar-end"]


navBarBurgerItem_ :: View model action
navBarBurgerItem_ = span_ [boolProp "aria-hidden" True] []

navBarBrand_ :: [View model action] -> View model action
navBarBrand_ = div_ [class_ "navbar-brand"]

navBarItemA_     :: [Attribute action] -> [View model action] -> View model action
navBarItemA_ ats = a_ ([class_ "navbar-item"] <> ats)
