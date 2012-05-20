{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module OpenGL.Evaluator.HaskellOpenGLFunctionsTH where
import OpenGL.Evaluator.GLFunction 
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Arrow

make_haskell_interface' :: String -> [GLFunction] -> Q [Dec]
make_haskell_interface' n fs = make_haskell_interface (mkName n) fs   
   
make_haskell_interface :: Name -> [GLFunction] -> Q [Dec]
make_haskell_interface name fs = do
    let parentCmd = make_gl_parent_command fs
    command <- make_to_resource_command_f name fs
    let result = (concatMap f_to_h fs) ++ [parentCmd, command]
    return result
   
f_to_h :: GLFunction -> [Dec]
f_to_h f@(GLFunction f_name return_value params) = result where
    result = [make_error name] ++ (f_to_c f) 
    name   = fn_to_sn f_name
             
f_to_c :: GLFunction -> [Dec]
f_to_c f@(GLFunction f_name return_value params) = result where
  result = [make_command name, 
            make_input   name (filter_input params), 
            make_output  name (filter_output params)]
  name   = fn_to_sn f_name
    
make_input name inputs = result where   
    result  = DataD [] (mkName (name ++ "Input")) [] [con] []
    con     = RecC (mkName (name ++ "Input")) members
    members = map (make_gl_primitive_input_member (to_under_score name)) inputs
    
make_gl_primitive_member f prefix (name, ty) = result where
    result = (mkName (prefix ++ name), NotStrict, f ty)
   
make_gl_primitive_input_member  = make_gl_primitive_member make_gl_input_type 
make_gl_primitive_output_member = make_gl_primitive_member make_gl_output_type 
    
make_gl_input_type (GLType Const 0 typ) = make_simple_gl_type typ
make_gl_input_type (GLType None  0 typ) = make_simple_gl_type typ
make_gl_input_type (GLType Const _ typ) = make_id_type
make_gl_input_type (GLType None  _ typ) = error "inputs should be const pointers!"

make_id_type = ConT $ mkName "GLPointer"

filter_output = filter (is_output . snd)

filter_input  = filter (not . is_output . snd)

make_output name outputs = result where   
    result  = DataD [] (mkName (name ++ "Output")) [] [con] []
    con     = RecC (mkName (name ++ "Output")) members
    members = map (make_gl_primitive_output_member (to_under_score name)) outputs

make_gl_output_type (GLType Const 0 typ) = error "Not an input type"
make_gl_output_type (GLType None  0 typ) = error "Not an input type"
make_gl_output_type (GLType Const _ typ) = error "Not an input type"
make_gl_output_type (GLType None  _ typ) = make_id_type
    
make_error name   = TySynD (mkName $ name ++ "Error")   [] $ ConT $ mkName "GLError"

make_g_command prefix name = TySynD (mkName $ name ++ "Command") [] $ 
    foldl AppT (ConT $ mkName (prefix ++ "Command")) $ map (ConT . mkName . ((name++prefix)++)) 
    ["Input", "Output", "Error"]

make_command   = make_g_command ""
make_r_command = make_g_command "R"

--after this I should make the generic ToCType class for this guy
--ResourceCommands
--These are like the commands above but they have resource ids
--Instead of pointers
--It is a perhaps unnecessary distinction
--yeah I just have the pointer hold an id
--They also have a function that can
--Convert them to RunCommand Command
--The commands above are used by the run command during serialization

--so I need to make a function that 
--goes from Command -> ResourceCommand Command
--basically I am going to add offsets that need to get fixed up

-- TODO get the template haskell stuff written working
--  assume it is working
-- Get the generic CType stuff working
-- Get the List and Loop working
-- Generate the Evaluator for ResourceManager
-- Generate the Evaluator for List and Loop

make_gl_parent_command :: [GLFunction] -> Dec
make_gl_parent_command fs = result where
    f_names = map (fn_to_sn . gl_function_name) fs
    result  = DataD [] (mkName "GLCommand") [] cons []
    cons    = map (\n -> NormalC (mkName ("G" ++ n)) [(NotStrict, ConT $ mkName (n ++ "Command"))]) f_names

make_to_resource_command_f :: Name -> [GLFunction] -> Q Dec
make_to_resource_command_f name gl_functions = funD name $ map make_cmd_to_r_cmd_clause gl_functions
              
mkNameM = return . VarE . mkName
              
make_cmd_to_r_cmd_clause f@(GLFunction f_name return_value params) = do
    let pattern   = AsP (mkName "c") $ ConP (mkName ("G" ++ name)) $ [VarP $ mkName "x"]
        fixups    = map (LitE . IntegerL) $ compute_fixups $ map snd $ filter_input params
        name      = fn_to_sn f_name
        body      = normalB [| $(conE $ mkName "RunCommand") $(mkNameM "c") $(return $ ListE fixups) |]
    
    clause [return pattern] body []


compute_fixups params = result where
    result = snd $ foldl add_to_offsets (0, []) params

add_to_offsets (offset, xs) param = result where
    result = (offset + size_of_gl_type param, xs')
    xs' = if is_pointer param then offset:xs else xs 
    
is_pointer (GLType _ 0 _) = False
is_pointer _ = True

size_of_gl_type x | is_pointer x = 4
size_of_gl_type x = size_of_tgl $ typ x

make_simple_gl_type = ConT . make_simple_gl_con

make_simple_gl_con = mkName . make_simple_gl_name

make_simple_gl_name TGLbitfield = "GLbitfield"
make_simple_gl_name TGLboolean  = "GLboolean"
make_simple_gl_name TGLbyte     = "GLbyte"
make_simple_gl_name TGLchar     = "GLchar"
make_simple_gl_name TGLclampf   = "GLclampf"
make_simple_gl_name TGLenum     = "GLenum"
make_simple_gl_name TGLfloat    = "GLfloat"
make_simple_gl_name TGLint      = "GLint"
make_simple_gl_name TGLshort    = "GLshort"
make_simple_gl_name TGLsizei    = "GLsizei"
make_simple_gl_name TGLubyte    = "GLubyte"
make_simple_gl_name TGLuint     = "GLuint"
make_simple_gl_name TGLushort   = "GLushort"
make_simple_gl_name TVoid       = error "can't make type void!"
make_simple_gl_name TGLsizeiptr = "GLsizeiptr"
make_simple_gl_name TGLIntPtr   = "GLIntPtr"
make_simple_gl_name TGLvoid     = "GLvoid"