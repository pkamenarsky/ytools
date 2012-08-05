{-# LANGUAGE TemplateHaskell #-}

import Gen

main = print $(listFields ''FileString)
