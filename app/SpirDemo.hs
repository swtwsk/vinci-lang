module SpirDemo (compileToDemoSpir) where

import Data.Bifunctor

import SPIRV.SpirOps

compileToDemoSpir :: [SpirOp] -> String
compileToDemoSpir spir = 
    unlines shaderPrologue ++ consts' ++ unlines shaderMain ++ fnOps'
    where
        (consts, fnOps) = extractConst spir
        consts' = unlines $ show <$> consts
        fnOps'  = unlines $ show <$> fnOps

extractConst :: [SpirOp] -> ([SpirOp], [SpirOp])
extractConst (c@OpConstant {}:t) = first (c:) $ extractConst t
extractConst (h:t) = second (h:) $ extractConst t
extractConst [] = ([], [])

shaderPrologue :: [String]
shaderPrologue = 
    [ "; SPIR-V"
    , "; Version: 1.0"
    , "; Generator: Google Shaderc over Glslang; 10"
    , "; Bound: 61"
    , "; Schema: 0"
    , "               OpCapability Shader"
    , "          %1 = OpExtInstImport \"GLSL.std.450\""
    , "               OpMemoryModel Logical GLSL450"
    , "               OpEntryPoint Fragment %main \"main\" %fragColor %outColor"
    , "               OpExecutionMode %main OriginUpperLeft"
    , "               OpSource GLSL 450"
    , "               OpSourceExtension \"GL_ARB_separate_shader_objects\""
    , "               OpSourceExtension \"GL_GOOGLE_cpp_style_line_directive\""
    , "               OpSourceExtension \"GL_GOOGLE_include_directive\""
    , "               OpName %main \"main\""
    , "               OpName %prototype_f1_f1_f1_f1_ \"prototype(f1;f1;f1;f1;\""
    , "               OpName %color_x \"color_x\""
    , "               OpName %fragColor \"fragColor\""
    , "               OpName %UniformBufferObject \"UniformBufferObject\""
    , "               OpMemberName %UniformBufferObject 0 \"u_time\""
    , "               OpName %ubo \"ubo\""
    , "               OpName %param \"param\""
    , "               OpName %param_0 \"param\""
    , "               OpName %param_1 \"param\""
    , "               OpName %param_2 \"param\""
    , "               OpName %color \"color\""
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
    , "          %8 = OpTypeFunction %float %_ptr_Function_float %_ptr_Function_float %_ptr_Function_float %_ptr_Function_float"
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
    , "    %v3float = OpTypeVector %float 3"
    , "%_ptr_Function_v3float = OpTypePointer Function %v3float"
    , "%_ptr_Output_v4float = OpTypePointer Output %v4float"
    , "   %outColor = OpVariable %_ptr_Output_v4float Output"
    , "    %float_1 = OpConstant %float 1" 
    ]

shaderMain :: [String]
shaderMain =
    [ "       %main = OpFunction %void None %3"
    , "          %5 = OpLabel"
    , "    %color_x = OpVariable %_ptr_Function_float Function"
    , "      %param = OpVariable %_ptr_Function_float Function"
    , "    %param_0 = OpVariable %_ptr_Function_float Function"
    , "    %param_1 = OpVariable %_ptr_Function_float Function"
    , "    %param_2 = OpVariable %_ptr_Function_float Function"
    , "      %color = OpVariable %_ptr_Function_v3float Function"
    , "         %31 = OpAccessChain %_ptr_Input_float %fragColor %uint_0"
    , "         %32 = OpLoad %float %31"
    , "               OpStore %param %32"
    , "         %35 = OpAccessChain %_ptr_Input_float %fragColor %uint_1"
    , "         %36 = OpLoad %float %35"
    , "               OpStore %param_0 %36"
    , "         %39 = OpAccessChain %_ptr_Input_float %fragColor %uint_2"
    , "         %40 = OpLoad %float %39"
    , "               OpStore %param_1 %40"
    , "         %43 = OpAccessChain %_ptr_Uniform_float %ubo %int_0"
    , "         %44 = OpLoad %float %43"
    , "               OpStore %param_2 %44"
    , "         %45 = OpFunctionCall %float %prototype_f1_f1_f1_f1_ %param %param_0 %param_1 %param_2"
    , "               OpStore %color_x %45"
    , "         %49 = OpLoad %float %color_x"
    , "         %50 = OpLoad %float %color_x"
    , "         %51 = OpLoad %float %color_x"
    , "         %52 = OpCompositeConstruct %v3float %49 %50 %51"
    , "               OpStore %color %52"
    , "         %55 = OpLoad %v3float %color"
    , "         %57 = OpCompositeExtract %float %55 0"
    , "         %58 = OpCompositeExtract %float %55 1"
    , "         %59 = OpCompositeExtract %float %55 2"
    , "         %60 = OpCompositeConstruct %v4float %57 %58 %59 %float_1"
    , "               OpStore %outColor %60"
    , "               OpReturn"
    , "               OpFunctionEnd"
    ]
