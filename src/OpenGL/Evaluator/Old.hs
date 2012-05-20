{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, DeriveDataTypeable, NoMonomorphismRestriction, TemplateHaskell #-}
module Types where
import Data.Word
import qualified Data.ByteString as BS
import Data.Data
import Data.Typeable
import Foreign
import Foreign.C.Types
import Foreign.Storable
import qualified Data.Binary as DB
import qualified Data.Binary.Get as DB
import qualified Data.Binary.Put as DB
import qualified Data.Binary.IEEE754 as DB
import GLPrimitives
import StructCreation
import Conversion
import GHC.Generics
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Checkers
import Data.DeriveTH
import ToCTypeTemplate
import Control.Applicative

instance ToValue MatrixUniformType
instance (ToValue a, ToValue b) => ToValue (Either a b)

instance ToValue BS.ByteString where
    to_v x = VArray $ map (VPrimitive . PGLubyte . GLubyte) $ BS.unpack x
    
instance ToCType BS.ByteString where
    to_c_type x = mk_record_ctype "ByteString" [
        "bytes" <:::> (TVariable $ TPrimitive TGLubyte)]
        
instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary

data ResourceMapper = ResourceMapper 
    {
        resource_mapper_map   :: GLboolean,
        resource_mapper_ids :: [Id]
    }
    deriving(Show, Eq, Generic)
    
instance ToValue ResourceMapper

instance ToCType ResourceMapper where
    to_c_type x = mk_record_ctype "ResourceMapper" [
        "resource_mapper_map" <::> (undefined :: GLboolean),
        "resource_mapper_ids" <:::> 10 <||> (undefined :: Id)]

instance Arbitrary ResourceMapper where
    arbitrary = do
        x <- arbitrary
        ids <- vectorOf 10 arbitrary
        return $ ResourceMapper x ids 

data ResourceId = NamedResource Id
                | GLResource GLId
                deriving(Show, Eq, Generic)
                
instance ToValue ResourceId

instance ToCType ResourceId where
    to_c_type x = TUnion "ResourceId" [
        "named_resource" <::> (undefined :: Id),
        "gl_resource"    <::> (undefined :: GLId)]
                
$(derive makeArbitrary ''ResourceId)
                
data MemoryLocation = MemoryLocation 
    {
        memory_location_id     :: Id,
        memory_location_offset :: GLuint
    }
    deriving(Show, Eq, Generic)
    

    
instance ToValue MemoryLocation
$(derive makeArbitrary ''MemoryLocation)
$(mk_simple_c_type_record ''MemoryLocation)
    
type Result a b = Either a b
    

data GCommand a b c = GCommand
    {
        cmd :: a,
        result :: Result b c
    }
    deriving(Show, Eq, Generic)
    
instance (ToValue a, ToValue b, ToValue c) => ToValue (GCommand a b c)


--AddData
data AddDataInput = AddDataInput
    {
        add_data_id     :: Id,
        add_data_buffer :: BS.ByteString
    }   
    deriving(Show, Eq, Generic)

    
instance ToValue AddDataInput 
$(derive makeArbitrary ''AddDataInput)
$(mk_simple_c_type_record ''AddDataInput)
    
data AddDataOutput = AddDataOutput
             deriving(Show, Eq, Generic)
instance ToValue AddDataOutput  
$(derive makeArbitrary ''AddDataOutput)
-- $(mk_simple_c_type_record ''AddDataOutput)
            
data AddDataError = AddDataNoRoom
             deriving(Show, Eq, Generic)
instance ToValue AddDataError 
$(derive makeArbitrary ''AddDataError)
--  $(mk_simple_c_type_record ''AddDataNoRoom)

type AddData = GCommand AddDataInput AddDataOutput AddDataError
--CopyData
data CopyDataInput = CopyDataInput
    {
        copy_data_id      :: Id,
        copy_data_buffer  :: BS.ByteString
    }
             deriving(Show, Eq, Generic)
instance ToValue CopyDataInput
$(derive makeArbitrary ''CopyDataInput)
$(mk_simple_c_type_record ''CopyDataInput)
             
data CopyDataOutput = CopyDataOutput
             deriving(Show, Eq, Generic)
instance ToValue CopyDataOutput
$(derive makeArbitrary ''CopyDataOutput)
-- $(mk_simple_c_type_record ''CopyDataOutput)
             
data CopyDataError  = CopyErrorNoRoom
                    | TooBig
                             deriving(Show, Eq, Generic)
$(derive makeArbitrary ''CopyDataError)
-- $(mk_simple_c_type_record ''CopyDataOutput)
                             
instance ToValue CopyDataError
                             
type CopyData = GCommand CopyDataInput CopyDataOutput CopyDataError
--DeleteData
data DeleteDataInput = DeleteDataInput
    {
        delete_data_id :: Id
    }
             deriving(Show, Eq, Generic)
instance ToValue DeleteDataInput
             
data DeleteDataOutput = DeleteDataOutput
             deriving(Show, Eq, Generic)
instance ToValue DeleteDataOutput
data DeleteDataError = DeleteIdNotFound
             deriving(Show, Eq, Generic)
instance ToValue DeleteDataError
type DeleteData = GCommand DeleteDataInput DeleteDataOutput DeleteDataError
--UpdateData
data UpdateDataInput = UpdateDataInput 
    {
        update_data_id     :: Id,
        update_data_buffer :: BS.ByteString
    }
             deriving(Show, Eq, Generic)
instance ToValue UpdateDataInput
$(derive makeArbitrary ''UpdateDataInput)
$(mk_simple_c_type_record ''UpdateDataInput)

data UpdateDataOutput = UpdateDataOutput
             deriving(Show, Eq, Generic)
instance ToValue UpdateDataOutput
data UpdateDataError  = UpdateIdNotFound
             deriving(Show, Eq, Generic)
instance ToValue UpdateDataError
type UpdateData = GCommand UpdateDataInput UpdateDataOutput UpdateDataError
--Enable
data EnableInput = EnableInput
    {
        enable_state :: GLenum
    }
             deriving(Show, Eq, Generic)
instance ToValue EnableInput
    
data EnableOutput = EnableOutput
   deriving(Show, Eq, Generic)

instance ToValue EnableOutput
$(derive makeArbitrary ''EnableOutput)
-- $(mk_simple_c_type_record ''EnableOutput)

type EnableError = GLError
type Enable = GCommand EnableInput EnableOutput EnableError
--GenBuffersInput
data GenBuffersInput = GenBuffersInput
    {
        gen_buffers_input_count  :: GLint,
        gen_buffers_input_mapper :: ResourceMapper
    }
    deriving(Show, Eq, Generic)

instance ToValue GenBuffersInput
$(derive makeArbitrary ''GenBuffersInput)
-- $(mk_simple_c_type_record ''GenBuffersInput)
    
data GenBuffersOutput = GenBuffersOutput 
    {
        gen_buffers_output_buffers :: [GLuint]
    }
    deriving(Show, Eq, Generic)
instance ToValue GenBuffersOutput
$(derive makeArbitrary ''GenBuffersOutput)

instance ToCType GenBuffersOutput where
    to_c_type = const $ mk_record_ctype "GenBuffersOutput" [
        "gen_buffers_output_buffers" <:::> 10 <||> (undefined :: GLuint)]
    
type GenBuffersError = GLError
type GenBuffers = GCommand GenBuffersInput GenBuffersOutput GenBuffersError 
--DeleteBuffers 
data DeleteBuffersInput = DeleteBuffersInput
    {
        delete_buffers_input_buffers :: [ResourceId]
    }
             deriving(Show, Eq, Generic)
instance ToValue DeleteBuffersInput
$(derive makeArbitrary ''DeleteBuffersInput)
instance ToCType DeleteBuffersInput where
    to_c_type = const $ mk_record_ctype "DeleteBuffersInput" [
        "delete_buffers_input_buffers" <:::> 10 <||> (undefined :: ResourceId)]

data DeleteBuffersOutput = DeleteBuffersOutput
             deriving(Show, Eq, Generic)
instance ToValue DeleteBuffersOutput

type DeleteBuffersError = GLError
type DeleteBuffers = GCommand DeleteBuffersInput DeleteBuffersOutput DeleteBuffersError
--BindBuffer
data BindBufferInput = BindBufferInput
    {
        bind_buffer_input_buffer_target :: GLenum,
        bind_buffer_input_id            :: ResourceId
    }
             deriving(Show, Eq, Generic)
instance ToValue BindBufferInput
$(derive makeArbitrary ''BindBufferInput)
$(mk_simple_c_type_record ''BindBufferInput)
             
data BindBufferOutput = BindBufferOutput
             deriving(Show, Eq, Generic)
instance ToValue BindBufferOutput
$(derive makeArbitrary ''BindBufferOutput)
-- $(mk_simple_c_type_record ''BindBufferOutput)
             
type BindBufferError = GLError
type BindBuffer = GCommand BindBufferInput BindBufferOutput BindBufferError
--BufferData
data BufferDataInput = BufferDataInput
    {
        buffer_data_input_buffer_target   :: GLenum,
        buffer_data_input_size            :: GLuint,
        buffer_data_input_memory_location :: MemoryLocation,
        buffer_data_input_usage           :: GLenum
    }
             deriving(Show, Eq, Generic)
instance ToValue BufferDataInput
$(derive makeArbitrary ''BufferDataInput)
$(mk_simple_c_type_record ''BufferDataInput)
          
data BufferDataOutput = BufferDataOutput
             deriving(Show, Eq, Generic)
instance ToValue BufferDataOutput
             
type BufferDataError = GLError
type BufferData = GCommand BufferDataInput BufferDataOutput BufferDataError
--EnableVertexAttribArray
data EnableVertexAttribArrayInput = EnableVertexAttribArrayInput
    {
        enable_vertex_attrib_array_input_index :: GLuint
    }
                 deriving(Show, Eq, Generic)
                 
instance ToValue EnableVertexAttribArrayInput

data EnableVertexAttribArrayOutput = EnableVertexAttribArrayOutput
              deriving(Show, Eq, Generic)
              
instance ToValue EnableVertexAttribArrayOutput

type EnableVertexAttribArrayError = GLError
type EnableVertexAttribArray = GCommand EnableVertexAttribArrayInput 
    EnableVertexAttribArrayOutput EnableVertexAttribArrayError 
--VertexAttribPointer
data VertexAttribPointerInput = VertexAttribPointerInput 
    {
        vertex_attrib_pointer_index      :: GLuint,
        vertex_attrib_pointer_size       :: GLint,
        vertex_attrib_pointer_type       :: GLenum,
        vertex_attrib_pointer_normalized :: GLuint,
        vertex_attrib_pointer_stride     :: GLsizei,
        vertex_attrib_pointer_offset     :: GLuint
    }
             deriving(Show, Eq, Generic)
             
instance ToValue VertexAttribPointerInput

data VertexAttribPointerOutput = VertexAttribPointerOutput
             deriving(Show, Eq, Generic)
instance ToValue VertexAttribPointerOutput
             
type VertexAttribPointerError = GLError
type VertexAttribPoint = GCommand VertexAttribPointerInput VertexAttribPointerOutput
    VertexAttribPointerError
--GenVertexArrayOES
data GenVertexArraysOESInput = GenVertexArraysOESInput 
    {
        gen_vertex_arrays_oes_input_count           :: GLuint,
        gen_vertex_arrays_oes_input_resource_mapper :: ResourceMapper
    }
             deriving(Show, Eq, Generic)
instance ToValue GenVertexArraysOESInput
             
data GenVertexArraysOESOutput = GenVertexArraysOESOutput 
    {
        gen_vertex_array_oes_output_buffers :: [GLuint]
    }
             deriving(Show, Eq, Generic)
             
instance ToValue GenVertexArraysOESOutput

type GenVertexArraysOESError = GLError
--BindVertexArrayOES    
data BindVertexArrayOESInput = BindVertexArrayOESInput 
    {
        bind_vertex_array_oes_input :: ResourceId
    }
             deriving(Show, Eq, Generic)
             
instance ToValue BindVertexArrayOESInput

data BindVertexArrayOESOutput = BindVertexArrayOESOutput
             deriving(Show, Eq, Generic)
             
instance ToValue BindVertexArrayOESOutput
             
type BindVertexArrayOESError = GLError
type BindVertexArray = GCommand BindVertexArrayOESInput BindVertexArrayOESOutput
    BindVertexArrayOESError
--CommandList   
data CommandListInput = CommandListInput
    {
        command_list_input_commands :: [Command]
    }
             deriving(Show, Eq, Generic)
             
instance ToValue CommandListInput
             
data CommandListOutput  = CommandListOutput
             deriving(Show, Eq, Generic)
             
instance ToValue CommandListOutput
             
type CommandListError = GLError
type CommandList = GCommand CommandListInput CommandListOutput CommandListError
--ClearColor
data ClearColorInput = ClearColorInput
    {
        clear_color_input_r :: GLfloat,
        clear_color_input_g :: GLfloat,
        clear_color_input_b :: GLfloat,
        clear_color_input_a :: GLfloat
    }
             deriving(Show, Eq, Generic)
             
instance ToValue ClearColorInput
             
data ClearColorOutput = ClearColorOutput
             deriving(Show, Eq, Generic)
             
instance ToValue ClearColorOutput
             
type ClearColorError = GLError
type ClearColor = GCommand ClearColorInput ClearColorOutput ClearColorError
--Clear
data ClearInput = ClearInput
    {
        clear_input_clear_flags :: GLint
    }
     deriving(Show, Eq, Generic)
     
instance ToValue ClearInput
     
data ClearOutput = ClearOutput
     deriving(Show, Eq, Generic)
     
instance ToValue ClearOutput
     
type ClearError = GLError
type Clear = GCommand ClearInput ClearOutput ClearError
--DrawArrays
data DrawArraysInput = DrawArraysInput
    {
        draw_arrays_input_id :: ResourceId
    }
     deriving(Show, Eq, Generic)
     
instance ToValue DrawArraysInput
     
data DrawArraysOutput = DrawArraysOutput
     deriving(Show, Eq, Generic)
     
instance ToValue DrawArraysOutput
     
type DrawArraysError = GLError
type DrawArrays = GCommand DrawArraysInput DrawArraysOutput DrawArraysError
--UseProgram
data UseProgramInput = UseProgramInput 
    {
        use_program_input_id :: ResourceId
    }
     deriving(Show, Eq, Generic)
     
instance ToValue UseProgramInput
     
data UseProgramOutput = UseProgramOutput
     deriving(Show, Eq, Generic)
     
instance ToValue UseProgramOutput
     
type UseProgramError = GLError
type UseProgram = GCommand UseProgramInput UseProgramOutput UseProgramError
--UniformMatrix
data UniformMatrixInput = UniformMatrixInput 
    {
        uniform_matrix_input_uniform_type    :: MatrixUniformType,
        uniform_matrix_input_uniform_index   :: GLuint,
        uniform_matrix_input_count           :: GLuint,
        uniform_matrix_input_transpose       :: GLboolean,
        uniform_matrix_input_memory_location :: MemoryLocation 
    }
     deriving(Show, Eq, Generic)
     
instance ToValue UniformMatrixInput
     
data UniformMatrixOutput = UniformMatrixOutput
     deriving(Show, Eq, Generic)
     
instance ToValue UniformMatrixOutput
     
type UniformMatrixError = GLError
type UniformMatrix = GCommand UniformMatrixInput UniformMatrixOutput UniformMatrixError
--AttachShader
data AttachShaderInput = AttachShaderInput 
    {
        attach_shader_input_program_id :: ResourceId,
        attach_shader_input_shader_id :: ResourceId
    }
     deriving(Show, Eq, Generic)
     
instance ToValue AttachShaderInput
     
data AttachShaderOutput = AttachShaderOutput
         deriving(Show, Eq, Generic)
         
instance ToValue AttachShaderOutput
         
type AttachShaderError = GLError

type AttachShader = GCommand AttachShaderInput AttachShaderOutput AttachShaderError
--BindAttribLocation 
data BindAttribLocationInput = BindAttribLocationInput 
    {
       bind_attrib_location_input_program_id :: ResourceId,
       bind_attrib_location_input_index      :: GLuint,
       bind_attrib_location_input_name       :: [GLchar]
    }
     deriving(Show, Eq, Generic)
     
instance ToValue BindAttribLocationInput
     
data BindAttribLocationOutput = BindAttribLocationOutput
     deriving(Show, Eq, Generic)
     
instance ToValue BindAttribLocationOutput
     
type BindAttribLocationError = GLError
type BindAttribLocation = GCommand BindAttribLocationInput BindAttribLocationOutput 
                                BindAttribLocationError
--CreateProgram
data CreateProgramInput = CreateProgramInput
    {
        create_program_input :: ResourceMapper
    }
     deriving(Show, Eq, Generic)
     
instance ToValue CreateProgramInput
     
data CreateProgramOutput = CreateProgramOutput
     deriving(Show, Eq, Generic)
     
instance ToValue CreateProgramOutput
     
type CreateProgramError = GLError
type CreateProgram = GCommand CreateProgramInput CreateProgramOutput CreateProgramError
--CreateShader
data CreateShaderInput = CreateShaderInput
    {
        create_shader_input_mapper :: ResourceMapper,
        create_shader_input_type   :: GLenum
    }
     deriving(Show, Eq, Generic)
     
instance ToValue CreateShaderInput    
     
data CreateShaderOutput = CreateShaderOutput
    {
        create_shader_output_id :: GLuint
    }
     deriving(Show, Eq, Generic)
     
instance ToValue CreateShaderOutput
     
type CreateShaderError = GLError
type CreateShader = GCommand CreateShaderInput CreateShaderOutput CreateShaderError
--ShaderSource  
data ShaderSourceInput = ShaderSourceInput
    {
        shader_source_id               :: ResourceId,
        shader_source_count            :: [GLuint],
        shader_source_source_location  :: MemoryLocation,
        shader_source_lengths          :: [GLint] 
    }
     deriving(Show, Eq, Generic)
     
instance ToValue ShaderSourceInput
             
data ShaderSourceOutput = ShaderSourceOutput
     deriving(Show, Eq, Generic)
     
instance ToValue ShaderSourceOutput
     
type ShaderSourceError = GLError
type ShaderSource = GCommand ShaderSourceInput ShaderSourceOutput ShaderSourceError
--CompileShader    
data CompileShaderInput = CompileShaderInput
    {
        compile_shader_id :: ResourceId
    }
     deriving(Show, Eq, Generic)
     
instance ToValue CompileShaderInput

data CompileShaderOutput = CompileShaderOutput
     deriving(Show, Eq, Generic)
     
instance ToValue CompileShaderOutput
     
type CompileShaderError = GLError
type CompileShader = GCommand CompileShaderInput CompileShaderOutput CompileShaderError
--LinkProgram
data LinkProgramInput = LinkProgramInput
    {
        link_program_input :: ResourceId
    }
             deriving(Show, Eq, Generic)
             
instance ToValue LinkProgramInput
             
data LinkProgramOutput = LinkProgramOutput
             deriving(Show, Eq, Generic)
             
instance ToValue LinkProgramOutput
             
type LinkProgramError = GLError
type LinkProgram = GCommand LinkProgramInput LinkProgramOutput LinkProgramError
--GetUniformLocation    
data GetUniformLocationInput = GetUniformLocationInput
    {
        get_uniform_location_program_id :: ResourceId,
        get_uniform_location_name       :: [GLchar]
    }
     deriving(Show, Eq, Generic)
     
instance ToValue GetUniformLocationInput
     
data GetUniformLocationOutput = GetUniformLocationOutput
    {
        get_uniform_location_output_index :: GLuint
    }
     deriving(Show, Eq, Generic)
     
instance ToValue GetUniformLocationOutput
     
type GetUniformLocationError = GLError
type GetUniformLocation = GCommand GetUniformLocationInput GetUniformLocationOutput 
                                GetUniformLocationError

data Command = CLoop Loop
             | CAddData AddData
             | CCopyData CopyData
             | CDeleteData DeleteData
             | CUpdateData UpdateData
             | CEnable Enable
             | CGenBuffers GenBuffers
             | CDeleteBuffers DeleteBuffers
             | CBindBuffer BindBuffer
             | CBufferData BufferData
             | CEnableVertexAttribArray EnableVertexAttribArray
             | CVertexAttribPoint VertexAttribPoint
             | CBindVertexArray BindVertexArray
             | CCommandList CommandList
             | CClearColor ClearColor
             | CClear Clear
             | CDrawArrays DrawArrays
             | CUseProgram UseProgram
             | CUniformMatrix UniformMatrix
             | CAttachShader AttachShader
             | CBindAttribLocation BindAttribLocation
             | CCreateProgram CreateProgram
             | CCreateShader CreateShader
             | CShaderSource ShaderSource
             | CCompileShader CompileShader
             | CLinkProgram LinkProgram 
             | CGetUniformLocation GetUniformLocation
             deriving(Show, Eq, Generic)


instance ToValue Command





--Loop
data LoopInput = LoopInput
    {
        loop_input_command :: Command
    }
    deriving(Show, Eq, Generic)
    
instance ToValue LoopInput
-- $(derive makeArbitrary ''LoopInput)
-- $(mk_simple_c_type_record ''LoopInput)
    
data LoopOutput = LoopOutput
             deriving(Show, Eq, Generic)
             
-- $(derive makeArbitrary ''LoopOutput)
-- $(mk_simple_c_type_record ''LoopOutput)

instance ToValue LoopOutput             
             
data LoopError = LoopError
    deriving(Show, Eq, Generic)
    
instance ToValue LoopError
    
type Loop = GCommand LoopInput LoopOutput LoopError












