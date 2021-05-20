module SpirDemo (compileToDemoSpir, compileToDemo2Spir) where

import SPIRV.SpirManager

compileToDemoSpir :: SpirManager -> String
compileToDemoSpir sm = 
    unlines shaderPrologue ++ unlines (show <$> _constants sm) ++ 
    unlines shaderMain ++ "\n" ++ unlines (show <$> _functions sm)

compileToDemo2Spir :: SpirManager -> String
compileToDemo2Spir sm = 
    unlines shaderPrologue2 ++ unlines (show <$> _opEntryPoints sm) ++
    unlines shaderPrologue3 ++ unlines (show <$> _annotations sm) ++
    unlines (show <$> _typeDeclarations sm) ++ unlines (show <$> _constants sm) ++
    unlines (show <$> _globalVariables sm) ++ unlines (show <$> _functions sm)

shaderPrologue :: [String]
shaderPrologue = 
    [ "; SPIR-V"
    , "; Version: 1.0"
    , "; Generator: Google Shaderc over Glslang; 10"
    , "; Bound: 59"
    , "; Schema: 0"
    , "               OpCapability Shader"
    , "               OpCapability VariablePointers"
    , "          %1 = OpExtInstImport \"GLSL.std.450\""
    , "               OpMemoryModel Logical GLSL450"
    , "               OpEntryPoint Fragment %main \"main\" %fragColor %outColor"
    , "               OpExecutionMode %main OriginUpperLeft"
    , "               OpSource GLSL 450"
    , "               OpSourceExtension \"GL_ARB_separate_shader_objects\""
    , "               OpSourceExtension \"GL_GOOGLE_cpp_style_line_directive\""
    , "               OpSourceExtension \"GL_GOOGLE_include_directive\""
    , "               OpName %main \"main\""
    , "               OpName %color \"color\""
    , "               OpName %fragColor \"fragColor\""
    , "               OpName %UniformBufferObject \"UniformBufferObject\""
    , "               OpMemberName %UniformBufferObject 0 \"u_time\""
    , "               OpName %ubo \"ubo\""
    , "               OpName %param \"param\""
    , "               OpName %param_0 \"param\""
    , "               OpName %param_1 \"param\""
    , "               OpName %param_2 \"param\""
    , "               OpName %outColor \"outColor\""
    , "               OpDecorate %fragColor Location 0"
    , "               OpMemberDecorate %UniformBufferObject 0 Offset 0"
    , "               OpDecorate %UniformBufferObject Block"
    , "               OpDecorate %ubo DescriptorSet 0"
    , "               OpDecorate %ubo Binding 0"
    , "               OpDecorate %outColor Location 0"
    , "       %void = OpTypeVoid"
    , "          %3 = OpTypeFunction %void"
    , "      %float = OpTypeFloat 32"
    , "%_ptr_Function_float = OpTypePointer Function %float"
    , "    %v3float = OpTypeVector %float 3"
    , "          %9 = OpTypeFunction %v3float %_ptr_Function_float %_ptr_Function_float %_ptr_Function_float %_ptr_Function_float"
    , "%_ptr_Function_v3float = OpTypePointer Function %v3float"
    , "    %v4float = OpTypeVector %float 4"
    , "%_ptr_Input_v4float = OpTypePointer Input %v4float"
    , "  %fragColor = OpVariable %_ptr_Input_v4float Input"
    , "%UniformBufferObject = OpTypeStruct %float"
    , "%_ptr_Uniform_UniformBufferObject = OpTypePointer Uniform %UniformBufferObject"
    , "        %ubo = OpVariable %_ptr_Uniform_UniformBufferObject Uniform"
    , "        %int = OpTypeInt 32 1"
    , "      %int_0 = OpConstant %int 0"
    , "       %uint = OpTypeInt 32 0"
    , "     %uint_0 = OpConstant %uint 0"
    , "%_ptr_Input_float = OpTypePointer Input %float"
    , "     %uint_1 = OpConstant %uint 1"
    , "     %uint_2 = OpConstant %uint 2"
    , "%_ptr_Uniform_float = OpTypePointer Uniform %float"
    , "%_ptr_Output_v4float = OpTypePointer Output %v4float"
    , "   %outColor = OpVariable %_ptr_Output_v4float Output"
    , "    %float_1 = OpConstant %float 1"
    ]

shaderPrologue2 :: [String]
shaderPrologue2 = 
    [ "; SPIR-V"
    , "; Version: 1.0"
    , "; Generator: Google Shaderc over Glslang; 10"
    , "; Bound: 59"
    , "; Schema: 0"
    , "               OpCapability Shader"
    , "               OpCapability VariablePointers"
    , "          %1 = OpExtInstImport \"GLSL.std.450\""
    , "               OpMemoryModel Logical GLSL450"
    ]
    -- , "               OpEntryPoint Fragment %main \"main\" %fragColor %outColor"
shaderPrologue3 :: [String]
shaderPrologue3 =
    -- [ "               OpExecutionMode %main OriginUpperLeft"
    [ "               OpSource GLSL 450"
    , "               OpSourceExtension \"GL_ARB_separate_shader_objects\""
    , "               OpSourceExtension \"GL_GOOGLE_cpp_style_line_directive\""
    , "               OpSourceExtension \"GL_GOOGLE_include_directive\""
    ]

shaderMain :: [String]
shaderMain =
    [ "       %main = OpFunction %void None %3"
    , "          %5 = OpLabel"
    , "      %color = OpVariable %_ptr_Function_v3float Function"
    , "      %param = OpVariable %_ptr_Function_float Function"
    , "    %param_0 = OpVariable %_ptr_Function_float Function"
    , "    %param_1 = OpVariable %_ptr_Function_float Function"
    , "    %param_2 = OpVariable %_ptr_Function_float Function"
    , "         %36 = OpAccessChain %_ptr_Input_float %fragColor %uint_0"
    , "         %37 = OpLoad %float %36"
    , "               OpStore %param %37"
    , "         %40 = OpAccessChain %_ptr_Input_float %fragColor %uint_1"
    , "         %41 = OpLoad %float %40"
    , "               OpStore %param_0 %41"
    , "         %44 = OpAccessChain %_ptr_Input_float %fragColor %uint_2"
    , "         %45 = OpLoad %float %44"
    , "               OpStore %param_1 %45"
    , "         %48 = OpAccessChain %_ptr_Uniform_float %ubo %int_0"
    , "         %49 = OpLoad %float %48"
    , "               OpStore %param_2 %49"
    , "         %50 = OpFunctionCall %v3float %prototype %param %param_0 %param_1 %param_2"
    , "               OpStore %color %50"
    , "         %53 = OpLoad %v3float %color"
    , "         %55 = OpCompositeExtract %float %53 0"
    , "         %56 = OpCompositeExtract %float %53 1"
    , "         %57 = OpCompositeExtract %float %53 2"
    , "         %58 = OpCompositeConstruct %v4float %55 %56 %57 %float_1"
    , "               OpStore %outColor %58"
    , "               OpReturn"
    , "               OpFunctionEnd"
    ]
