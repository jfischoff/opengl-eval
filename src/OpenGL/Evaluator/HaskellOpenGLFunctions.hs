{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
module OpenGL.Evaluator.HaskellOpenGLFunctions where
import OpenGL.Evaluator.ParseOpenGLFunction
import OpenGL.Evaluator.HaskellOpenGLFunctionsTH
import OpenGL.Evaluator.GLFunction
import Control.Applicative ((<$>))
import Language.Haskell.TH
import Control.Monad
import OpenGL.Evaluator.GLPrimitives
import GHC.Generics
import Language.C.Simple.Evaluator
import MRP
import Data.Default
import Language.C.Simple.CType

$(do
    (Right fs) <- runIO $ load_and_parse_header_file "OpenGLFunctions.h" 
    make_haskell_interface' "to_run" fs)
   
