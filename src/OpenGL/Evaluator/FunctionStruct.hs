{-# LANGUAGE QuasiQuotes, DeriveDataTypeable, TemplateHaskell, StandaloneDeriving, NoMonomorphismRestriction #-}
module OpenGL.Evaluator.FunctionStruct where
import OpenGL.Evaluator.GLFunction
import OpenGL.Evaluator.QuoteOpenGL
import Control.Arrow
import Language.C hiding (Const)
import Data.Loc
import Language.C.Syntax hiding (Const)
import Data.Symbol
import Data.Char (toUpper, toLower, isUpper, isNumber)
import Data.List

test_function = GLFunction 
    {
        gl_function_name = "glBufferData", 
        gl_function_return_value = GLType {qualifier = None, indirection_count = 0, typ = TVoid}, 
        gl_parameters = [
            ("target",GLType {qualifier = None, indirection_count = 0, typ = TGLenum}),
            ("size",GLType   {qualifier = None, indirection_count = 0, typ = TGLsizeiptr}),
            ("data",GLType   {qualifier = Const, indirection_count = 1, typ = TGLvoid}),
            ("usage",GLType  {qualifier = None, indirection_count = 0, typ = TGLenum})]
    }
    
--convert the function name to a better name
--

mk_field_group name typ = result where
    result  = [csdecl| $ty:typ $id:name; |]

mk_record_struct name name_and_types = result where
    result = [cedecl| typedef struct $id:name_t { $sdecls:members } $id:name; |]
    members = map (uncurry mk_field_group) name_and_types
    name_t = name ++ "_t"
    
mk_struct_type name_and_types = result where
    result = [cty| struct { $sdecls:members }|]
    members = map (uncurry mk_field_group) name_and_types
 
mk_enum x = [cenum| $id:x|] 
    
mk_enum_decl name options = result where
    result = [cedecl| typedef enum { $enums:enum_options } $id:name ;|]
    enum_options = map mk_enum options

function_struct function = result where
    result            = mk_record_struct (function_name_to_struct_name $ gl_function_name function) names_and_types
    (inputs, outputs) = split_input_and_output $ gl_parameters function
    cmd_type          = mk_struct_type $ map (second to_type) $ inputs
    result_types      = filter (\(_, y) -> typ y /= TVoid && typ y /= TGLvoid) $ ("return", gl_function_return_value function):outputs
    result_struct     = mk_struct_type $ map (second to_type) $ result_types
    names_and_types   = if not $ null result_types 
        then [("cmd", cmd_type)]
        else [("cmd", cmd_type), ("result", result_struct)]
        




split_input_and_output xs = (xs \\ outputs, outputs) where
    outputs = filter (\(_, x) -> is_output x) xs



dup x = (x, x)

mk_named name = (Tnamed (mk_id name) noSrcLoc)   

mk_named' name = mk_type $ mk_named name

mk_id name = Id name noSrcLoc

mk_type ty = Type (DeclSpec [] [] ty noSrcLoc) (DeclRoot noSrcLoc) noSrcLoc

mk_struct_decl other_defs name names = result where
    result = [cunit| $edecls:other_defs typedef struct $id:name_t { 
        $sdecl:type_enum_field $sdecl:union_field } $id:name; |]

    name_t = name ++ "_t"
    
    type_enum_name = "E" ++ name ++ "Type"
    type_enum_type = mk_named' type_enum_name
    type_enum_field = mk_field_group "type" type_enum_type
    
    members = map (uncurry mk_field_group . (to_under_score *** mk_named'))  $ map dup names
    union_field = FieldGroup (DeclSpec [] [] (Tunion Nothing (Just members) [] noSrcLoc) noSrcLoc) [] noSrcLoc
    
enum_name_from_name data_type_name = "E" ++ data_type_name ++ "Type"


capitalize = map toUpper

name_to_enum_option (_:enum_name) name = result where
    result = caps_enum_name ++ "_" ++ caps_name
    caps_enum_name = capitalize $ to_under_score enum_name
    caps_name = capitalize $ to_under_score name

command_union name functions = result where
    cmds         = map function_struct functions
    enum_name    = enum_name_from_name name
    names        = map (function_name_to_struct_name . gl_function_name) functions
    enum_options = map (name_to_enum_option enum_name) names 
    decl         = mk_enum_decl enum_name enum_options
    result       = mk_struct_decl (decl:cmds) name names
    
-----------------------------------------------------------------------------------------------------------------------
--make the evaluator

evaluator_def name command_type_name = result where
    result = [cedecl| void evaluate( $param:param ); |]
    param  = [cparam| $ty:typ_id *command |]
    typ_id = mk_named' command_type_name

evaluator command_type_name functions = result where
    result = [cfun| void evaluate( $param:param ){
        switch(command->cmd.type) 
        $stm:switch_block
     }|]
    param  = [cparam| $ty:typ_id *command |]
    typ_id = mk_named' command_type_name
    case_statements = concatMap (dispatch_function_case command_type_name) functions
    switch_block = Block case_statements noSrcLoc


dispatch_function_case command_name function = result where
    result           = [BlockStm cas, BlockStm $ Break noSrcLoc]
    cas              = Case (Var enum_name noSrcLoc) (Exp (Just $ case_exp) noSrcLoc) noSrcLoc
    enum_name        = function_name_to_enum_name command_name (function_name_to_struct_name $ gl_function_name function)
    func_id          = mk_id $ gl_function_name function
    func_args        = map mk_func_arg $ gl_parameters function
    func_call        = FnCall (Var func_id noSrcLoc) func_args noSrcLoc
    case_exp         = if (typ $ gl_function_return_value function) == TVoid || (typ $ gl_function_return_value function) == TGLvoid
                            then func_call 
                            else Assign (Member (PtrMember (Var (mk_id "command") noSrcLoc) (mk_id "result") noSrcLoc) (mk_id "return") noSrcLoc) JustAssign func_call noSrcLoc

function_name_to_enum_name cmd_name fn_name = result where
    enum_name = enum_name_from_name cmd_name
    result    = mk_id $ name_to_enum_option enum_name fn_name

mk_func_arg (param, ty) = result where
    result   = if is_output ty
                    then Member (PtrMember (Var (mk_id "command") noSrcLoc) (mk_id "result") noSrcLoc) param_id noSrcLoc
                    else Member (PtrMember (Var (mk_id "command") noSrcLoc) (mk_id "cmd") noSrcLoc) param_id noSrcLoc 
    param_id = mk_id param


-----------------------------------------------------------------------------------------------------------------------
 
to_type (GLType None  0 TGLbitfield) = [cty| GLbitfield |]
to_type (GLType Const 0 TGLbitfield) = [cty| const GLbitfield |]
to_type (GLType None  1 TGLbitfield) = [cty| GLbitfield* |]
to_type (GLType Const 1 TGLbitfield) = [cty| const GLbitfield* |]

to_type (GLType None  0 TGLboolean) = [cty| GLboolean |]
to_type (GLType Const 0 TGLboolean) = [cty| const GLboolean |]
to_type (GLType None  1 TGLboolean) = [cty| GLboolean* |]
to_type (GLType Const 1 TGLboolean) = [cty| const GLboolean* |]

to_type (GLType None  0 TGLbyte) = [cty| GLbyte |]
to_type (GLType Const 0 TGLbyte) = [cty| const GLbyte |]
to_type (GLType None  1 TGLbyte) = [cty| GLbyte* |]
to_type (GLType Const 1 TGLbyte) = [cty| const GLbyte* |]

to_type (GLType None  0 TGLchar) = [cty| GLchar |]
to_type (GLType Const 0 TGLchar) = [cty| const GLchar |]
to_type (GLType None  1 TGLchar) = [cty| GLchar* |]
to_type (GLType Const 1 TGLchar) = [cty| const GLchar* |]

to_type (GLType None  0 TGLclampf) = [cty| GLclampf |]
to_type (GLType Const 0 TGLclampf) = [cty| const GLclampf |]
to_type (GLType None  1 TGLclampf) = [cty| GLclampf* |]
to_type (GLType Const 1 TGLclampf) = [cty| const GLclampf* |]

to_type (GLType None  0 TGLenum) = [cty| GLenum |]
to_type (GLType Const 0 TGLenum) = [cty| const GLenum |]
to_type (GLType None  1 TGLenum) = [cty| GLenum* |]
to_type (GLType Const 1 TGLenum) = [cty| const GLenum* |]

to_type (GLType None  0 TGLfloat) = [cty| GLfloat |]
to_type (GLType Const 0 TGLfloat) = [cty| const GLfloat |]
to_type (GLType None  1 TGLfloat) = [cty| GLfloat* |]
to_type (GLType Const 1 TGLfloat) = [cty| const GLfloat* |]

to_type (GLType None  0 TGLint) = [cty| GLint |]
to_type (GLType Const 0 TGLint) = [cty| const GLint |]
to_type (GLType None  1 TGLint) = [cty| GLint* |]
to_type (GLType Const 1 TGLint) = [cty| const GLint* |]

to_type (GLType None  0 TGLshort) = [cty| GLshort |]
to_type (GLType Const 0 TGLshort) = [cty| const GLshort |]
to_type (GLType None  1 TGLshort) = [cty| GLshort* |]
to_type (GLType Const 1 TGLshort) = [cty| const GLshort* |]

to_type (GLType None  0 TGLsizei) = [cty| GLsizei |]
to_type (GLType Const 0 TGLsizei) = [cty| const GLsizei |]
to_type (GLType None  1 TGLsizei) = [cty| GLsizei* |]
to_type (GLType Const 1 TGLsizei) = [cty| const GLsizei* |]

to_type (GLType None  0 TGLubyte) = [cty| GLubyte |]
to_type (GLType Const 0 TGLubyte) = [cty| const GLubyte |]
to_type (GLType None  1 TGLubyte) = [cty| GLubyte* |]
to_type (GLType Const 1 TGLubyte) = [cty| const GLubyte* |]

to_type (GLType None  0 TGLuint) = [cty| GLuint |]
to_type (GLType Const 0 TGLuint) = [cty| const GLuint |]
to_type (GLType None  1 TGLuint) = [cty| GLuint* |]
to_type (GLType Const 1 TGLuint) = [cty| const GLuint* |]

to_type (GLType None  0 TGLushort) = [cty| GLushort |]
to_type (GLType Const 0 TGLushort) = [cty| const GLushort |]
to_type (GLType None  1 TGLushort) = [cty| GLushort* |]
to_type (GLType Const 1 TGLushort) = [cty| const GLushort* |]

to_type (GLType None  0 TVoid) = [cty| void |]
to_type (GLType Const 0 TVoid) = [cty| const void |]
to_type (GLType None  1 TVoid) = [cty| void* |]
to_type (GLType Const 1 TVoid) = [cty| const void* |]

to_type (GLType None  0 TGLsizeiptr) = [cty| GLsizeiptr |]
to_type (GLType Const 0 TGLsizeiptr) = [cty| const GLsizeiptr |]
to_type (GLType None  1 TGLsizeiptr) = [cty| GLsizeiptr* |]
to_type (GLType Const 1 TGLsizeiptr) = [cty| const GLsizeiptr* |]

to_type (GLType None  0 TGLIntPtr) = [cty| GLintptr |]
to_type (GLType Const 0 TGLIntPtr) = [cty| const GLintptr |]
to_type (GLType None  1 TGLIntPtr) = [cty| GLintptr* |]
to_type (GLType Const 1 TGLIntPtr) = [cty| const GLintptr* |]

to_type (GLType None  0 TGLvoid) = [cty| GLvoid |]
to_type (GLType Const 0 TGLvoid) = [cty| const GLvoid |]
to_type (GLType None  1 TGLvoid) = [cty| GLvoid* |]
to_type (GLType Const 1 TGLvoid) = [cty| const GLvoid* |]





