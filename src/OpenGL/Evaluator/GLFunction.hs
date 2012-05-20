module OpenGL.Evaluator.GLFunction where
import Data.Char (toUpper, toLower, isUpper, isNumber)
import Data.List

data TGLPrimitive = TGLbitfield
                  | TGLboolean
                  | TGLbyte
                  | TGLchar
                  | TGLclampf
                  | TGLenum
                  | TGLfloat
                  | TGLint
                  | TGLshort
                  | TGLsizei
                  | TGLubyte
                  | TGLuint
                  | TGLushort
                  | TVoid
                  | TGLsizeiptr
                  | TGLIntPtr
                  | TGLvoid
                 deriving(Show, Eq)

data TypeQualifier = Const
                   | None
                   deriving(Show, Eq)

data GLType = GLType 
    {
        qualifier           :: TypeQualifier,
        indirection_count   :: Int,
        typ                 :: TGLPrimitive
    }
                   deriving(Show, Eq)

is_output x = qualifier x == None && indirection_count x > 0

function_name_to_struct_name (x:y:xs) = (toUpper x):(toUpper y):xs

fn_to_sn = function_name_to_struct_name

--ugly  
consume :: String -> (String, [String]) -> (String, [String])    
consume [] (current, old)       = ([], (reverse current):old) 
consume (x:[]) (current, old)   = if (isUpper x || isNumber x) then ([], [x]:(reverse current):old) else ([], (reverse $ x:current):old)
consume (x:y:xs) (current, old) = if (isUpper x || isNumber x) then consume xs (toLower y:(toLower x):[], (reverse $ current):old) else consume (y:xs) (x:current, old)  


to_under_score :: String -> String    
to_under_score camel_case = result where
    (x:concatted) = concat $ intersperse "_" $ reverse $ snd $ consume (camel_case) ([], [])
    result = if x == '_'
                then concatted
                else x:concatted

data GLFunction = GLFunction 
    {
        gl_function_name         :: String,
        gl_function_return_value :: GLType,
        gl_parameters            :: [(String, GLType)]
    }
    deriving(Show, Eq)
    
size_of_tgl TGLbitfield = 4
size_of_tgl TGLboolean  = 1
size_of_tgl TGLbyte     = 1
size_of_tgl TGLchar     = 1
size_of_tgl TGLclampf   = 4
size_of_tgl TGLenum     = 4
size_of_tgl TGLfloat    = 4
size_of_tgl TGLint      = 4
size_of_tgl TGLshort    = 2
size_of_tgl TGLsizei    = 4
size_of_tgl TGLubyte    = 1
size_of_tgl TGLuint     = 4
size_of_tgl TGLushort   = 2
size_of_tgl TVoid       = error "TVoid does not have a size"
size_of_tgl TGLsizeiptr = 4
size_of_tgl TGLIntPtr   = 4
size_of_tgl TGLvoid     = error "TGLvoid does not have a size"