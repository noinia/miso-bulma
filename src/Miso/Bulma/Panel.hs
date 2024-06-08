{-# LANGUAGE OverloadedStrings          #-}
module Miso.Bulma.Panel
  ( panel
  , panelBlock
  , panelIcon

  , panelTabs
  ) where

import           Miso
import           Miso.String (MisoString)
import           Miso.Bulma.Generic

--------------------------------------------------------------------------------

-- | Renders a panel
panel                 :: [Attribute action]
                      -> [View action] -- ^ The heading
                      -> [View action] -- ^ The content
                      -> View action
panel ats heading chs =
    nav_ ([class_ "panel"] <> ats)
         ([p_ [class_ "panel-heading"] heading
          ]
          <> chs
         )

-- | A div that is a panel block
panelBlock :: [View action] -> View action
panelBlock = div_ [class_ "panel-block"]

-- | A panel icon
panelIcon   :: MisoString -> View action
panelIcon i = span_ [ class_ "panel-icon"]
                    [ icon i [] ]


-- | Panel tabs
panelTabs :: [View action] -> View action
panelTabs = p_ [class_ "panel-tabs"]
