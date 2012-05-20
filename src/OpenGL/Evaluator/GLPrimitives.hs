{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
module OpenGL.Evaluator.GLPrimitives where
import Data.Word
import qualified Data.ByteString as BS
import Data.Data
import Data.Typeable 
import Foreign
import Foreign.C.Types
import Foreign.Storable
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.DeriveTH
import Test.QuickCheck
import GHC.Generics


newtype GLbitfield = GLbitfield Word32 
    deriving(Show, Eq, Num, Storable, Binary, Data, Typeable, Arbitrary, Generic)
    
newtype GLboolean  = GLboolean Bool
    deriving(Show, Eq, Storable, Data, Typeable, Arbitrary, Generic)

instance Binary GLboolean where
    put (GLboolean x) = if x then putWord8 1 else putWord8 0    
    get = undefined

newtype GLbyte     = GLbyte Word8
    deriving(Show, Eq, Num, Storable, Binary, Data, Typeable, Arbitrary, Generic)
newtype GLchar     = GLchar Char
    deriving(Show, Eq, Storable, Binary, Data, Typeable, Arbitrary, Generic)
newtype GLclampf   = GLclampf Float
    deriving(Show, Eq, Num, Storable, Ord, Fractional, Real, Data, Typeable, Arbitrary, Generic)

instance Binary GLclampf where
    put (GLclampf x) = putFloat32le x
    get = undefined

newtype GLenum     = GLenum Word32
    deriving(Show, Eq, Num, Storable, Binary, Data, Typeable, Arbitrary, Generic)
newtype GLfloat    = GLfloat Float
    deriving(Show, Eq, Num, Storable, Ord, Fractional, Real, Data, Typeable, Arbitrary, Generic)

instance Binary GLfloat where
    put (GLfloat x) = putFloat32le x
    get = undefined  

newtype GLint = GLint Int32
    deriving(Show, Eq, Num, Storable, Data, Typeable, Arbitrary, Generic)

instance Binary GLint where
    put (GLint x) = putWord32le $ fromIntegral x
    get = undefined 

newtype GLshort    = GLshort Int16
    deriving(Show, Eq, Num, Storable, Data, Typeable, Arbitrary, Generic)

instance Binary GLshort where
    put (GLshort x) = putWord16le $ fromIntegral x
    get = undefined 
    


newtype GLsizei    = GLsizei Word32
    deriving(Show, Eq, Num, Storable, Binary, Data, Typeable, Arbitrary, Generic)
newtype GLubyte    = GLubyte Word8
    deriving(Show, Eq, Num, Storable, Binary, Data, Typeable, Arbitrary, Generic)
newtype GLuint     = GLuint Word32
    deriving(Show, Eq, Num, Storable, Binary, Data, Typeable, Arbitrary, Generic)
newtype GLushort   = GLushort Word16
    deriving(Show, Eq, Num, Storable, Binary, Data, Typeable, Arbitrary, Generic)
newtype GLfixed    = GLfixed Word32
        deriving(Show, Eq, Num, Storable, Binary, Data, Typeable, Arbitrary, Generic)
newtype GLclampx    = GLclampx Word32
        deriving(Show, Eq, Num, Storable, Binary, Data, Typeable, Arbitrary, Generic)

type GLIntPtr = GLuint
type GLsizeiptr = GLuint

data MatrixUniformType = MATRIX_UNIFORM_2X2
                       | MATRIX_UNIFORM_3X3
                       | MATRIX_UNIFORM_4X4
                     deriving(Show, Eq, Data, Typeable, Generic)
                     
$(derive makeBinary ''MatrixUniformType)
$(derive makeArbitrary ''MatrixUniformType)

type GLError = GLenum

type GLPointer = GLuint

type Id  = GLuint
type GLId = GLuint