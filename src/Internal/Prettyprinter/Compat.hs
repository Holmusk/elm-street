{-# LANGUAGE CPP #-}
module Internal.Prettyprinter.Compat (module PP) where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter as PP
#else
import Data.Text.Prettyprint.Doc as PP
#endif
