{-# LANGUAGE OverloadedStrings          #-}
module Miso.Bulma.Generic
  ( useBulmaRemote
  , bulmaLink
  , iconLink
  , icon
  , styleM_
  ) where

import qualified Data.Map as Map
import           Miso
import           Miso.String (MisoString)

--------------------------------------------------------------------------------

-- | Use bulma from a CDN
useBulmaRemote :: View action
useBulmaRemote = div_ [] [bulmaLink,iconLink]

-- | Insert a bulma link
bulmaLink :: View action
bulmaLink = link_ [ rel_ "stylesheet"
                   , href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.min.css"
                   , textProp "integrity" "sha256-vK3UTo/8wHbaUn+dTQD0X6dzidqc5l7gczvH+Bnowwk="
                   , textProp "crossorigin" "anonymous"
                   ]

-- | Produce a linked icon
iconLink :: View action
iconLink = Miso.script_ [ src_ "https://use.fontawesome.com/releases/v5.14.4/js/all.js"
                        , defer_ "true"
                        ] mempty

--------------------------------------------------------------------------------

-- | Produce an icon
icon      :: MisoString -- ^ icon name
          -> [Attribute action] -- ^ attributes
          -> View action
icon cs ats = span_ ([ class_ "icon"]
                     <> ats
                    )
                    [ i_ [ class_ cs, textProp "aria-hidden" "true"] []
                    ]


--------------------------------------------------------------------------------

-- | Style attribute, convenient together with '=:'
styleM_    :: [Map.Map MisoString MisoString] -> Attribute action
styleM_ xs = style_ $ mconcat xs
