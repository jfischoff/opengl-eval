{-# LANGUAGE NoMonomorphismRestriction #-}
module OpenGL.Evaluator.ParseOpenGLFunction where
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Control.Applicative hiding ((<|>), many, Const, optional)
import OpenGL.Evaluator.GLFunction
import OpenGL.Evaluator.FunctionStruct
import OpenGL.Evaluator.QuoteOpenGL

parse_header_file = many1 parse_function

parse_function = do
    return_value <- parse_return_value
    spaces
    name         <- parse_name
    spaces
    parameters   <- parse_parameters
    spaces
    char ';' 
    optional spaces
    return $ GLFunction name return_value parameters
    
parse_return_value = parse_type
    
parse_name = identifier haskell
parse_parameters = do
    (parens haskell) (try void_param <|> many_params)
    
void_param = do
    string "void"
    return []
    
many_params = sepBy parse_param (do {spaces; char ','; spaces})
    
parse_param = do
    typ  <- parse_type
    spaces
    name <- (identifier $ makeTokenParser javaStyle)
    return (name, typ)
 
parse_type = do
    qualifier <- parse_qualifier
    spaces
    typ <- parse_primitive
    spaces
    indirection_count <- parse_indirection_count
    return $ GLType qualifier indirection_count typ
    
parse_qualifier = try (do {string "const"; return Const}) <|> (return None)
    
parse_indirection_count = length <$> many (do {spaces; string "*"; spaces})
    
parse_primitive = choice $ map (uncurry parse_primitive_of) [
    ("GLbitfield", TGLbitfield),
    ("GLintptr", TGLIntPtr),
    ("GLboolean", TGLboolean),
    ("GLbyte", TGLbyte),
    ("GLchar", TGLchar),
    ("GLclampf", TGLclampf),
    ("GLenum", TGLenum),
    ("GLfloat", TGLfloat),
    ("GLint", TGLint),
    ("GLshort", TGLshort),
    ("GLsizeiptr", TGLsizeiptr), 
    ("GLsizei", TGLsizei),
    ("GLubyte", TGLubyte),
    ("GLuint", TGLuint),
    ("GLushort", TGLushort),
    ("void", TVoid),
    ("GLvoid", TGLvoid)]
    
parse_primitive_of name typ = do
    try (string name)
    return typ
    
fromRight (Right x) = x
    
load_and_parse_header_file filepath = do
    file <- readFile filepath
    return $ runParser parse_header_file () filepath file 
    
run str = runParser parse_function () "" str

--now I need write the structs
--then I can write the 
    
to_eval = do
    (Right xs) <- load_and_parse_header_file "OpenGLFunctions.h"
    return $ evaluator "Command" xs



