{-# LANGUAGE TemplateHaskell #-}

import Gen

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

fsSchema = $(listFields ''FileString)

main = do
	print fsSchema
