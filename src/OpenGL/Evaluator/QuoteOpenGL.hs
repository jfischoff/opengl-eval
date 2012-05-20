module OpenGL.Evaluator.QuoteOpenGL where
    
import qualified Language.C.Parser as P
import qualified Language.C.Syntax as C
import Language.C.Quote.Base (ToExp(..), quasiquote)

exts :: [C.Extensions]
exts = []

typenames :: [String]
typenames = ["GLbitfield", "GLboolean", "GLbyte", "GLchar", "GLclampf", "GLenum",
     "GLfloat" , "GLint", "GLshort", "GLsizei", "GLubyte", "GLuint", "GLushort",
    "GLsizeiptr", "GLintptr", "GLvoid"]



cdecl  = quasiquote exts typenames P.parseDecl
cedecl = quasiquote exts typenames P.parseEdecl
cenum  = quasiquote exts typenames P.parseEnum
cexp   = quasiquote exts typenames P.parseExp
cfun   = quasiquote exts typenames P.parseFunc
cinit  = quasiquote exts typenames P.parseInit
cparam = quasiquote exts typenames P.parseParam
csdecl = quasiquote exts typenames P.parseStructDecl
cstm   = quasiquote exts typenames P.parseStm
cty    = quasiquote exts typenames P.parseType
cunit  = quasiquote exts typenames P.parseUnit