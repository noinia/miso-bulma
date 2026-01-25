{-# LANGUAGE OverloadedStrings          #-}
module Miso.Bulma.Panel
  ( panel
  , panelBlock
  , panelIcon

  , panelTabs
  ) where

import Miso
import Miso.Bulma.Generic
import Miso.Html.Element
import Miso.Html.Property

--------------------------------------------------------------------------------

-- | Renders a panel
panel                 :: [Attribute action]
                      -> [View model action] -- ^ The heading
                      -> [View model action] -- ^ The content
                      -> View model action
panel ats heading chs =
    nav_ ([class_ "panel"] <> ats)
         ([p_ [class_ "panel-heading"] heading
          ]
          <> chs
         )

-- | A div that is a panel block
panelBlock :: [View model action] -> View model action
panelBlock = div_ [class_ "panel-block"]

-- | A panel icon
panelIcon   :: MisoString -> View model action
panelIcon i = span_ [ class_ "panel-icon"]
                    [ icon i [] ]


-- | Panel tabs
panelTabs :: [View model action] -> View model action
panelTabs = p_ [class_ "panel-tabs"]
