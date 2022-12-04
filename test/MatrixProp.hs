{-# LANGUAGE TemplateHaskell #-}

module MatrixProp (generated_prop_safe_add_solo) where

import Language.Haskell.TH
import TH (genProp)

$(genProp)
