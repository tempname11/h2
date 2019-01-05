module Web.GLES.Const where
-- WARNING: plagiarized from:
-- https://github.com/ziocroc/Ombra/blob/master/Graphics/Rendering/Ombra
-- kudos to ziocroc

import Prelude

gl_DEPTH_BUFFER_BIT :: Num a => a
gl_DEPTH_BUFFER_BIT = 0x00000100

gl_STENCIL_BUFFER_BIT :: Num a => a
gl_STENCIL_BUFFER_BIT = 0x00000400

gl_COLOR_BUFFER_BIT :: Num a => a
gl_COLOR_BUFFER_BIT = 0x00004000

gl_POINTS :: Num a => a
gl_POINTS = 0x0000

gl_LINES :: Num a => a
gl_LINES = 0x0001

gl_LINE_LOOP :: Num a => a
gl_LINE_LOOP = 0x0002

gl_LINE_STRIP :: Num a => a
gl_LINE_STRIP = 0x0003

gl_TRIANGLES :: Num a => a
gl_TRIANGLES = 0x0004

gl_TRIANGLE_STRIP :: Num a => a
gl_TRIANGLE_STRIP = 0x0005

gl_TRIANGLE_FAN :: Num a => a
gl_TRIANGLE_FAN = 0x0006

gl_ZERO :: Num a => a
gl_ZERO = 0

gl_ONE :: Num a => a
gl_ONE = 1

gl_SRC_COLOR :: Num a => a
gl_SRC_COLOR = 0x0300

gl_ONE_MINUS_SRC_COLOR :: Num a => a
gl_ONE_MINUS_SRC_COLOR = 0x0301

gl_SRC_ALPHA :: Num a => a
gl_SRC_ALPHA = 0x0302

gl_ONE_MINUS_SRC_ALPHA :: Num a => a
gl_ONE_MINUS_SRC_ALPHA = 0x0303

gl_DST_ALPHA :: Num a => a
gl_DST_ALPHA = 0x0304

gl_ONE_MINUS_DST_ALPHA :: Num a => a
gl_ONE_MINUS_DST_ALPHA = 0x0305

gl_DST_COLOR :: Num a => a
gl_DST_COLOR = 0x0306

gl_ONE_MINUS_DST_COLOR :: Num a => a
gl_ONE_MINUS_DST_COLOR = 0x0307

gl_SRC_ALPHA_SATURATE :: Num a => a
gl_SRC_ALPHA_SATURATE = 0x0308

gl_FUNC_ADD :: Num a => a
gl_FUNC_ADD = 0x8006

gl_BLEND_EQUATION :: Num a => a
gl_BLEND_EQUATION = 0x8009

gl_BLEND_EQUATION_RGB :: Num a => a
gl_BLEND_EQUATION_RGB = 0x8009   

gl_BLEND_EQUATION_ALPHA :: Num a => a
gl_BLEND_EQUATION_ALPHA = 0x883D

gl_FUNC_SUBTRACT :: Num a => a
gl_FUNC_SUBTRACT = 0x800A

gl_FUNC_REVERSE_SUBTRACT :: Num a => a
gl_FUNC_REVERSE_SUBTRACT = 0x800B

gl_BLEND_DST_RGB :: Num a => a
gl_BLEND_DST_RGB = 0x80C8

gl_BLEND_SRC_RGB :: Num a => a
gl_BLEND_SRC_RGB = 0x80C9

gl_BLEND_DST_ALPHA :: Num a => a
gl_BLEND_DST_ALPHA = 0x80CA

gl_BLEND_SRC_ALPHA :: Num a => a
gl_BLEND_SRC_ALPHA = 0x80CB

gl_CONSTANT_COLOR :: Num a => a
gl_CONSTANT_COLOR = 0x8001

gl_ONE_MINUS_CONSTANT_COLOR :: Num a => a
gl_ONE_MINUS_CONSTANT_COLOR = 0x8002

gl_CONSTANT_ALPHA :: Num a => a
gl_CONSTANT_ALPHA = 0x8003

gl_ONE_MINUS_CONSTANT_ALPHA :: Num a => a
gl_ONE_MINUS_CONSTANT_ALPHA = 0x8004

gl_BLEND_COLOR :: Num a => a
gl_BLEND_COLOR = 0x8005

gl_ARRAY_BUFFER :: Num a => a
gl_ARRAY_BUFFER = 0x8892

gl_ELEMENT_ARRAY_BUFFER :: Num a => a
gl_ELEMENT_ARRAY_BUFFER = 0x8893

gl_ARRAY_BUFFER_BINDING :: Num a => a
gl_ARRAY_BUFFER_BINDING = 0x8894

gl_ELEMENT_ARRAY_BUFFER_BINDING :: Num a => a
gl_ELEMENT_ARRAY_BUFFER_BINDING = 0x8895

gl_STREAM_DRAW :: Num a => a
gl_STREAM_DRAW = 0x88E0

gl_STATIC_DRAW :: Num a => a
gl_STATIC_DRAW = 0x88E4

gl_DYNAMIC_DRAW :: Num a => a
gl_DYNAMIC_DRAW = 0x88E8

gl_BUFFER_SIZE :: Num a => a
gl_BUFFER_SIZE = 0x8764

gl_BUFFER_USAGE :: Num a => a
gl_BUFFER_USAGE = 0x8765

gl_CURRENT_VERTEX_ATTRIB :: Num a => a
gl_CURRENT_VERTEX_ATTRIB = 0x8626

gl_FRONT :: Num a => a
gl_FRONT = 0x0404

gl_BACK :: Num a => a
gl_BACK = 0x0405

gl_FRONT_AND_BACK :: Num a => a
gl_FRONT_AND_BACK = 0x0408

gl_CULL_FACE :: Num a => a
gl_CULL_FACE = 0x0B44

gl_BLEND :: Num a => a
gl_BLEND = 0x0BE2

gl_DITHER :: Num a => a
gl_DITHER = 0x0BD0

gl_STENCIL_TEST :: Num a => a
gl_STENCIL_TEST = 0x0B90

gl_DEPTH_TEST :: Num a => a
gl_DEPTH_TEST = 0x0B71

gl_SCISSOR_TEST :: Num a => a
gl_SCISSOR_TEST = 0x0C11

gl_POLYGON_OFFSET_FILL :: Num a => a
gl_POLYGON_OFFSET_FILL = 0x8037

gl_SAMPLE_ALPHA_TO_COVERAGE :: Num a => a
gl_SAMPLE_ALPHA_TO_COVERAGE = 0x809E

gl_SAMPLE_COVERAGE :: Num a => a
gl_SAMPLE_COVERAGE = 0x80A0

gl_NO_ERROR :: Num a => a
gl_NO_ERROR = 0

gl_INVALID_ENUM :: Num a => a
gl_INVALID_ENUM = 0x0500

gl_INVALID_VALUE :: Num a => a
gl_INVALID_VALUE = 0x0501

gl_INVALID_OPERATION :: Num a => a
gl_INVALID_OPERATION = 0x0502

gl_OUT_OF_MEMORY :: Num a => a
gl_OUT_OF_MEMORY = 0x0505

gl_CW :: Num a => a
gl_CW = 0x0900

gl_CCW :: Num a => a
gl_CCW = 0x0901

gl_LINE_WIDTH :: Num a => a
gl_LINE_WIDTH = 0x0B21

gl_ALIASED_POINT_SIZE_RANGE :: Num a => a
gl_ALIASED_POINT_SIZE_RANGE = 0x846D

gl_ALIASED_LINE_WIDTH_RANGE :: Num a => a
gl_ALIASED_LINE_WIDTH_RANGE = 0x846E

gl_CULL_FACE_MODE :: Num a => a
gl_CULL_FACE_MODE = 0x0B45

gl_FRONT_FACE :: Num a => a
gl_FRONT_FACE = 0x0B46

gl_DEPTH_RANGE :: Num a => a
gl_DEPTH_RANGE = 0x0B70

gl_DEPTH_WRITEMASK :: Num a => a
gl_DEPTH_WRITEMASK = 0x0B72

gl_DEPTH_CLEAR_VALUE :: Num a => a
gl_DEPTH_CLEAR_VALUE = 0x0B73

gl_DEPTH_FUNC :: Num a => a
gl_DEPTH_FUNC = 0x0B74

gl_STENCIL_CLEAR_VALUE :: Num a => a
gl_STENCIL_CLEAR_VALUE = 0x0B91

gl_STENCIL_FUNC :: Num a => a
gl_STENCIL_FUNC = 0x0B92

gl_STENCIL_FAIL :: Num a => a
gl_STENCIL_FAIL = 0x0B94

gl_STENCIL_PASS_DEPTH_FAIL :: Num a => a
gl_STENCIL_PASS_DEPTH_FAIL = 0x0B95

gl_STENCIL_PASS_DEPTH_PASS :: Num a => a
gl_STENCIL_PASS_DEPTH_PASS = 0x0B96

gl_STENCIL_REF :: Num a => a
gl_STENCIL_REF = 0x0B97

gl_STENCIL_VALUE_MASK :: Num a => a
gl_STENCIL_VALUE_MASK = 0x0B93

gl_STENCIL_WRITEMASK :: Num a => a
gl_STENCIL_WRITEMASK = 0x0B98

gl_STENCIL_BACK_FUNC :: Num a => a
gl_STENCIL_BACK_FUNC = 0x8800

gl_STENCIL_BACK_FAIL :: Num a => a
gl_STENCIL_BACK_FAIL = 0x8801

gl_STENCIL_BACK_PASS_DEPTH_FAIL :: Num a => a
gl_STENCIL_BACK_PASS_DEPTH_FAIL = 0x8802

gl_STENCIL_BACK_PASS_DEPTH_PASS :: Num a => a
gl_STENCIL_BACK_PASS_DEPTH_PASS = 0x8803

gl_STENCIL_BACK_REF :: Num a => a
gl_STENCIL_BACK_REF = 0x8CA3

gl_STENCIL_BACK_VALUE_MASK :: Num a => a
gl_STENCIL_BACK_VALUE_MASK = 0x8CA4

gl_STENCIL_BACK_WRITEMASK :: Num a => a
gl_STENCIL_BACK_WRITEMASK = 0x8CA5

gl_VIEWPORT :: Num a => a
gl_VIEWPORT = 0x0BA2

gl_SCISSOR_BOX :: Num a => a
gl_SCISSOR_BOX = 0x0C10

gl_COLOR_CLEAR_VALUE :: Num a => a
gl_COLOR_CLEAR_VALUE = 0x0C22

gl_COLOR_WRITEMASK :: Num a => a
gl_COLOR_WRITEMASK = 0x0C23

gl_UNPACK_ALIGNMENT :: Num a => a
gl_UNPACK_ALIGNMENT = 0x0CF5

gl_PACK_ALIGNMENT :: Num a => a
gl_PACK_ALIGNMENT = 0x0D05

gl_MAX_TEXTURE_SIZE :: Num a => a
gl_MAX_TEXTURE_SIZE = 0x0D33

gl_MAX_VIEWPORT_DIMS :: Num a => a
gl_MAX_VIEWPORT_DIMS = 0x0D3A

gl_SUBPIXEL_BITS :: Num a => a
gl_SUBPIXEL_BITS = 0x0D50

gl_RED_BITS :: Num a => a
gl_RED_BITS = 0x0D52

gl_GREEN_BITS :: Num a => a
gl_GREEN_BITS = 0x0D53

gl_BLUE_BITS :: Num a => a
gl_BLUE_BITS = 0x0D54

gl_ALPHA_BITS :: Num a => a
gl_ALPHA_BITS = 0x0D55

gl_DEPTH_BITS :: Num a => a
gl_DEPTH_BITS = 0x0D56

gl_STENCIL_BITS :: Num a => a
gl_STENCIL_BITS = 0x0D57

gl_POLYGON_OFFSET_UNITS :: Num a => a
gl_POLYGON_OFFSET_UNITS = 0x2A00

gl_POLYGON_OFFSET_FACTOR :: Num a => a
gl_POLYGON_OFFSET_FACTOR = 0x8038

gl_TEXTURE_BINDING_2D :: Num a => a
gl_TEXTURE_BINDING_2D = 0x8069

gl_SAMPLE_BUFFERS :: Num a => a
gl_SAMPLE_BUFFERS = 0x80A8

gl_SAMPLES :: Num a => a
gl_SAMPLES = 0x80A9

gl_SAMPLE_COVERAGE_VALUE :: Num a => a
gl_SAMPLE_COVERAGE_VALUE = 0x80AA

gl_SAMPLE_COVERAGE_INVERT :: Num a => a
gl_SAMPLE_COVERAGE_INVERT = 0x80AB

gl_COMPRESSED_TEXTURE_FORMATS :: Num a => a
gl_COMPRESSED_TEXTURE_FORMATS = 0x86A3

gl_DONT_CARE :: Num a => a
gl_DONT_CARE = 0x1100

gl_FASTEST :: Num a => a
gl_FASTEST = 0x1101

gl_NICEST :: Num a => a
gl_NICEST = 0x1102

gl_GENERATE_MIPMAP_HINT :: Num a => a
gl_GENERATE_MIPMAP_HINT = 0x8192

gl_BYTE :: Num a => a
gl_BYTE = 0x1400

gl_UNSIGNED_BYTE :: Num a => a
gl_UNSIGNED_BYTE = 0x1401

gl_SHORT :: Num a => a
gl_SHORT = 0x1402

gl_UNSIGNED_SHORT :: Num a => a
gl_UNSIGNED_SHORT = 0x1403

gl_INT :: Num a => a
gl_INT = 0x1404

gl_UNSIGNED_INT :: Num a => a
gl_UNSIGNED_INT = 0x1405

gl_FLOAT :: Num a => a
gl_FLOAT = 0x1406

gl_DEPTH_COMPONENT :: Num a => a
gl_DEPTH_COMPONENT = 0x1902

gl_ALPHA :: Num a => a
gl_ALPHA = 0x1906

gl_RGB :: Num a => a
gl_RGB = 0x1907

gl_RGBA :: Num a => a
gl_RGBA = 0x1908

gl_LUMINANCE :: Num a => a
gl_LUMINANCE = 0x1909

gl_LUMINANCE_ALPHA :: Num a => a
gl_LUMINANCE_ALPHA = 0x190A

gl_UNSIGNED_SHORT_4_4_4_4 :: Num a => a
gl_UNSIGNED_SHORT_4_4_4_4 = 0x8033

gl_UNSIGNED_SHORT_5_5_5_1 :: Num a => a
gl_UNSIGNED_SHORT_5_5_5_1 = 0x8034

gl_UNSIGNED_SHORT_5_6_5 :: Num a => a
gl_UNSIGNED_SHORT_5_6_5 = 0x8363

gl_FRAGMENT_SHADER :: Num a => a
gl_FRAGMENT_SHADER = 0x8B30

gl_VERTEX_SHADER :: Num a => a
gl_VERTEX_SHADER = 0x8B31

gl_MAX_VERTEX_ATTRIBS :: Num a => a
gl_MAX_VERTEX_ATTRIBS = 0x8869

gl_MAX_VERTEX_UNIFORM_VECTORS :: Num a => a
gl_MAX_VERTEX_UNIFORM_VECTORS = 0x8DFB

gl_MAX_VARYING_VECTORS :: Num a => a
gl_MAX_VARYING_VECTORS = 0x8DFC

gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: Num a => a
gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS = 0x8B4D

gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: Num a => a
gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS = 0x8B4C

gl_MAX_TEXTURE_IMAGE_UNITS :: Num a => a
gl_MAX_TEXTURE_IMAGE_UNITS = 0x8872

gl_MAX_FRAGMENT_UNIFORM_VECTORS :: Num a => a
gl_MAX_FRAGMENT_UNIFORM_VECTORS = 0x8DFD

gl_SHADER_TYPE :: Num a => a
gl_SHADER_TYPE = 0x8B4F

gl_DELETE_STATUS :: Num a => a
gl_DELETE_STATUS = 0x8B80

gl_LINK_STATUS :: Num a => a
gl_LINK_STATUS = 0x8B82

gl_VALIDATE_STATUS :: Num a => a
gl_VALIDATE_STATUS = 0x8B83

gl_ATTACHED_SHADERS :: Num a => a
gl_ATTACHED_SHADERS = 0x8B85

gl_ACTIVE_UNIFORMS :: Num a => a
gl_ACTIVE_UNIFORMS = 0x8B86

gl_ACTIVE_ATTRIBUTES :: Num a => a
gl_ACTIVE_ATTRIBUTES = 0x8B89

gl_SHADING_LANGUAGE_VERSION :: Num a => a
gl_SHADING_LANGUAGE_VERSION = 0x8B8C

gl_CURRENT_PROGRAM :: Num a => a
gl_CURRENT_PROGRAM = 0x8B8D

gl_NEVER :: Num a => a
gl_NEVER = 0x0200

gl_LESS :: Num a => a
gl_LESS = 0x0201

gl_EQUAL :: Num a => a
gl_EQUAL = 0x0202

gl_LEQUAL :: Num a => a
gl_LEQUAL = 0x0203

gl_GREATER :: Num a => a
gl_GREATER = 0x0204

gl_NOTEQUAL :: Num a => a
gl_NOTEQUAL = 0x0205

gl_GEQUAL :: Num a => a
gl_GEQUAL = 0x0206

gl_ALWAYS :: Num a => a
gl_ALWAYS = 0x0207

gl_KEEP :: Num a => a
gl_KEEP = 0x1E00

gl_REPLACE :: Num a => a
gl_REPLACE = 0x1E01

gl_INCR :: Num a => a
gl_INCR = 0x1E02

gl_DECR :: Num a => a
gl_DECR = 0x1E03

gl_INVERT :: Num a => a
gl_INVERT = 0x150A

gl_INCR_WRAP :: Num a => a
gl_INCR_WRAP = 0x8507

gl_DECR_WRAP :: Num a => a
gl_DECR_WRAP = 0x8508

gl_VENDOR :: Num a => a
gl_VENDOR = 0x1F00

gl_RENDERER :: Num a => a
gl_RENDERER = 0x1F01

gl_VERSION :: Num a => a
gl_VERSION = 0x1F02

gl_NEAREST :: Num a => a
gl_NEAREST = 0x2600

gl_LINEAR :: Num a => a
gl_LINEAR = 0x2601

gl_NEAREST_MIPMAP_NEAREST :: Num a => a
gl_NEAREST_MIPMAP_NEAREST = 0x2700

gl_LINEAR_MIPMAP_NEAREST :: Num a => a
gl_LINEAR_MIPMAP_NEAREST = 0x2701

gl_NEAREST_MIPMAP_LINEAR :: Num a => a
gl_NEAREST_MIPMAP_LINEAR = 0x2702

gl_LINEAR_MIPMAP_LINEAR :: Num a => a
gl_LINEAR_MIPMAP_LINEAR = 0x2703

gl_TEXTURE_MAG_FILTER :: Num a => a
gl_TEXTURE_MAG_FILTER = 0x2800

gl_TEXTURE_MIN_FILTER :: Num a => a
gl_TEXTURE_MIN_FILTER = 0x2801

gl_TEXTURE_WRAP_S :: Num a => a
gl_TEXTURE_WRAP_S = 0x2802

gl_TEXTURE_WRAP_T :: Num a => a
gl_TEXTURE_WRAP_T = 0x2803

gl_TEXTURE_2D :: Num a => a
gl_TEXTURE_2D = 0x0DE1

gl_TEXTURE :: Num a => a
gl_TEXTURE = 0x1702

gl_TEXTURE_CUBE_MAP :: Num a => a
gl_TEXTURE_CUBE_MAP = 0x8513

gl_TEXTURE_BINDING_CUBE_MAP :: Num a => a
gl_TEXTURE_BINDING_CUBE_MAP = 0x8514

gl_TEXTURE_CUBE_MAP_POSITIVE_X :: Num a => a
gl_TEXTURE_CUBE_MAP_POSITIVE_X = 0x8515

gl_TEXTURE_CUBE_MAP_NEGATIVE_X :: Num a => a
gl_TEXTURE_CUBE_MAP_NEGATIVE_X = 0x8516

gl_TEXTURE_CUBE_MAP_POSITIVE_Y :: Num a => a
gl_TEXTURE_CUBE_MAP_POSITIVE_Y = 0x8517

gl_TEXTURE_CUBE_MAP_NEGATIVE_Y :: Num a => a
gl_TEXTURE_CUBE_MAP_NEGATIVE_Y = 0x8518

gl_TEXTURE_CUBE_MAP_POSITIVE_Z :: Num a => a
gl_TEXTURE_CUBE_MAP_POSITIVE_Z = 0x8519

gl_TEXTURE_CUBE_MAP_NEGATIVE_Z :: Num a => a
gl_TEXTURE_CUBE_MAP_NEGATIVE_Z = 0x851A

gl_MAX_CUBE_MAP_TEXTURE_SIZE :: Num a => a
gl_MAX_CUBE_MAP_TEXTURE_SIZE = 0x851C

gl_TEXTURE0 :: Num a => a
gl_TEXTURE0 = 0x84C0

gl_TEXTURE1 :: Num a => a
gl_TEXTURE1 = 0x84C1

gl_TEXTURE2 :: Num a => a
gl_TEXTURE2 = 0x84C2

gl_TEXTURE3 :: Num a => a
gl_TEXTURE3 = 0x84C3

gl_TEXTURE4 :: Num a => a
gl_TEXTURE4 = 0x84C4

gl_TEXTURE5 :: Num a => a
gl_TEXTURE5 = 0x84C5

gl_TEXTURE6 :: Num a => a
gl_TEXTURE6 = 0x84C6

gl_TEXTURE7 :: Num a => a
gl_TEXTURE7 = 0x84C7

gl_TEXTURE8 :: Num a => a
gl_TEXTURE8 = 0x84C8

gl_TEXTURE9 :: Num a => a
gl_TEXTURE9 = 0x84C9

gl_TEXTURE10 :: Num a => a
gl_TEXTURE10 = 0x84CA

gl_TEXTURE11 :: Num a => a
gl_TEXTURE11 = 0x84CB

gl_TEXTURE12 :: Num a => a
gl_TEXTURE12 = 0x84CC

gl_TEXTURE13 :: Num a => a
gl_TEXTURE13 = 0x84CD

gl_TEXTURE14 :: Num a => a
gl_TEXTURE14 = 0x84CE

gl_TEXTURE15 :: Num a => a
gl_TEXTURE15 = 0x84CF

gl_TEXTURE16 :: Num a => a
gl_TEXTURE16 = 0x84D0

gl_TEXTURE17 :: Num a => a
gl_TEXTURE17 = 0x84D1

gl_TEXTURE18 :: Num a => a
gl_TEXTURE18 = 0x84D2

gl_TEXTURE19 :: Num a => a
gl_TEXTURE19 = 0x84D3

gl_TEXTURE20 :: Num a => a
gl_TEXTURE20 = 0x84D4

gl_TEXTURE21 :: Num a => a
gl_TEXTURE21 = 0x84D5

gl_TEXTURE22 :: Num a => a
gl_TEXTURE22 = 0x84D6

gl_TEXTURE23 :: Num a => a
gl_TEXTURE23 = 0x84D7

gl_TEXTURE24 :: Num a => a
gl_TEXTURE24 = 0x84D8

gl_TEXTURE25 :: Num a => a
gl_TEXTURE25 = 0x84D9

gl_TEXTURE26 :: Num a => a
gl_TEXTURE26 = 0x84DA

gl_TEXTURE27 :: Num a => a
gl_TEXTURE27 = 0x84DB

gl_TEXTURE28 :: Num a => a
gl_TEXTURE28 = 0x84DC

gl_TEXTURE29 :: Num a => a
gl_TEXTURE29 = 0x84DD

gl_TEXTURE30 :: Num a => a
gl_TEXTURE30 = 0x84DE

gl_TEXTURE31 :: Num a => a
gl_TEXTURE31 = 0x84DF

gl_ACTIVE_TEXTURE :: Num a => a
gl_ACTIVE_TEXTURE = 0x84E0

gl_REPEAT :: Num a => a
gl_REPEAT = 0x2901

gl_CLAMP_TO_EDGE :: Num a => a
gl_CLAMP_TO_EDGE = 0x812F

gl_MIRRORED_REPEAT :: Num a => a
gl_MIRRORED_REPEAT = 0x8370

gl_FLOAT_VEC2 :: Num a => a
gl_FLOAT_VEC2 = 0x8B50

gl_FLOAT_VEC3 :: Num a => a
gl_FLOAT_VEC3 = 0x8B51

gl_FLOAT_VEC4 :: Num a => a
gl_FLOAT_VEC4 = 0x8B52

gl_INT_VEC2 :: Num a => a
gl_INT_VEC2 = 0x8B53

gl_INT_VEC3 :: Num a => a
gl_INT_VEC3 = 0x8B54

gl_INT_VEC4 :: Num a => a
gl_INT_VEC4 = 0x8B55

gl_BOOL :: Num a => a
gl_BOOL = 0x8B56

gl_BOOL_VEC2 :: Num a => a
gl_BOOL_VEC2 = 0x8B57

gl_BOOL_VEC3 :: Num a => a
gl_BOOL_VEC3 = 0x8B58

gl_BOOL_VEC4 :: Num a => a
gl_BOOL_VEC4 = 0x8B59

gl_FLOAT_MAT2 :: Num a => a
gl_FLOAT_MAT2 = 0x8B5A

gl_FLOAT_MAT3 :: Num a => a
gl_FLOAT_MAT3 = 0x8B5B

gl_FLOAT_MAT4 :: Num a => a
gl_FLOAT_MAT4 = 0x8B5C

gl_SAMPLER_2D :: Num a => a
gl_SAMPLER_2D = 0x8B5E

gl_SAMPLER_CUBE :: Num a => a
gl_SAMPLER_CUBE = 0x8B60

gl_VERTEX_ATTRIB_ARRAY_ENABLED :: Num a => a
gl_VERTEX_ATTRIB_ARRAY_ENABLED = 0x8622

gl_VERTEX_ATTRIB_ARRAY_SIZE :: Num a => a
gl_VERTEX_ATTRIB_ARRAY_SIZE = 0x8623

gl_VERTEX_ATTRIB_ARRAY_STRIDE :: Num a => a
gl_VERTEX_ATTRIB_ARRAY_STRIDE = 0x8624

gl_VERTEX_ATTRIB_ARRAY_TYPE :: Num a => a
gl_VERTEX_ATTRIB_ARRAY_TYPE = 0x8625

gl_VERTEX_ATTRIB_ARRAY_NORMALIZED :: Num a => a
gl_VERTEX_ATTRIB_ARRAY_NORMALIZED = 0x886A

gl_VERTEX_ATTRIB_ARRAY_POINTER :: Num a => a
gl_VERTEX_ATTRIB_ARRAY_POINTER = 0x8645

gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: Num a => a
gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = 0x889F

gl_COMPILE_STATUS :: Num a => a
gl_COMPILE_STATUS = 0x8B81

gl_LOW_FLOAT :: Num a => a
gl_LOW_FLOAT = 0x8DF0

gl_MEDIUM_FLOAT :: Num a => a
gl_MEDIUM_FLOAT = 0x8DF1

gl_HIGH_FLOAT :: Num a => a
gl_HIGH_FLOAT = 0x8DF2

gl_LOW_INT :: Num a => a
gl_LOW_INT = 0x8DF3

gl_MEDIUM_INT :: Num a => a
gl_MEDIUM_INT = 0x8DF4

gl_HIGH_INT :: Num a => a
gl_HIGH_INT = 0x8DF5

gl_FRAMEBUFFER :: Num a => a
gl_FRAMEBUFFER = 0x8D40

gl_RENDERBUFFER :: Num a => a
gl_RENDERBUFFER = 0x8D41

gl_RGBA4 :: Num a => a
gl_RGBA4 = 0x8056

gl_RGB5_A1 :: Num a => a
gl_RGB5_A1 = 0x8057

gl_RGB565 :: Num a => a
gl_RGB565 = 0x8D62

gl_DEPTH_COMPONENT16 :: Num a => a
gl_DEPTH_COMPONENT16 = 0x81A5

gl_STENCIL_INDEX :: Num a => a
gl_STENCIL_INDEX = 0x1901

gl_STENCIL_INDEX8 :: Num a => a
gl_STENCIL_INDEX8 = 0x8D48

gl_DEPTH_STENCIL :: Num a => a
gl_DEPTH_STENCIL = 0x84F9

gl_RENDERBUFFER_WIDTH :: Num a => a
gl_RENDERBUFFER_WIDTH = 0x8D42

gl_RENDERBUFFER_HEIGHT :: Num a => a
gl_RENDERBUFFER_HEIGHT = 0x8D43

gl_RENDERBUFFER_INTERNAL_FORMAT :: Num a => a
gl_RENDERBUFFER_INTERNAL_FORMAT = 0x8D44

gl_RENDERBUFFER_RED_SIZE :: Num a => a
gl_RENDERBUFFER_RED_SIZE = 0x8D50

gl_RENDERBUFFER_GREEN_SIZE :: Num a => a
gl_RENDERBUFFER_GREEN_SIZE = 0x8D51

gl_RENDERBUFFER_BLUE_SIZE :: Num a => a
gl_RENDERBUFFER_BLUE_SIZE = 0x8D52

gl_RENDERBUFFER_ALPHA_SIZE :: Num a => a
gl_RENDERBUFFER_ALPHA_SIZE = 0x8D53

gl_RENDERBUFFER_DEPTH_SIZE :: Num a => a
gl_RENDERBUFFER_DEPTH_SIZE = 0x8D54

gl_RENDERBUFFER_STENCIL_SIZE :: Num a => a
gl_RENDERBUFFER_STENCIL_SIZE = 0x8D55

gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: Num a => a
gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = 0x8CD0

gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: Num a => a
gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = 0x8CD1

gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: Num a => a
gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = 0x8CD2

gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: Num a => a
gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = 0x8CD3

gl_COLOR_ATTACHMENT0 :: Num a => a
gl_COLOR_ATTACHMENT0 = 0x8CE0

gl_DEPTH_ATTACHMENT :: Num a => a
gl_DEPTH_ATTACHMENT = 0x8D00

gl_STENCIL_ATTACHMENT :: Num a => a
gl_STENCIL_ATTACHMENT = 0x8D20

gl_DEPTH_STENCIL_ATTACHMENT :: Num a => a
gl_DEPTH_STENCIL_ATTACHMENT = 0x821A

gl_NONE :: Num a => a
gl_NONE = 0

gl_FRAMEBUFFER_COMPLETE :: Num a => a
gl_FRAMEBUFFER_COMPLETE = 0x8CD5

gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: Num a => a
gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = 0x8CD6

gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: Num a => a
gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 0x8CD7

gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: Num a => a
gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = 0x8CD9

gl_FRAMEBUFFER_UNSUPPORTED :: Num a => a
gl_FRAMEBUFFER_UNSUPPORTED = 0x8CDD

gl_FRAMEBUFFER_BINDING :: Num a => a
gl_FRAMEBUFFER_BINDING = 0x8CA6

gl_RENDERBUFFER_BINDING :: Num a => a
gl_RENDERBUFFER_BINDING = 0x8CA7

gl_MAX_RENDERBUFFER_SIZE :: Num a => a
gl_MAX_RENDERBUFFER_SIZE = 0x84E8

gl_INVALID_FRAMEBUFFER_OPERATION :: Num a => a
gl_INVALID_FRAMEBUFFER_OPERATION = 0x0506

gl_UNPACK_FLIP_Y_WEBGL :: Num a => a
gl_UNPACK_FLIP_Y_WEBGL = 0x9240

gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL :: Num a => a
gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL = 0x9241

gl_CONTEXT_LOST_WEBGL :: Num a => a
gl_CONTEXT_LOST_WEBGL = 0x9242

gl_UNPACK_COLORSPACE_CONVERSION_WEBGL :: Num a => a
gl_UNPACK_COLORSPACE_CONVERSION_WEBGL = 0x9243

gl_BROWSER_DEFAULT_WEBGL :: Num a => a
gl_BROWSER_DEFAULT_WEBGL = 0x9244

-- WEBGL_draw_buffers

gl_COLOR_ATTACHMENT0_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT0_WEBGL = 0x8CE0

gl_COLOR_ATTACHMENT1_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT1_WEBGL = 0x8CE1

gl_COLOR_ATTACHMENT2_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT2_WEBGL = 0x8CE2

gl_COLOR_ATTACHMENT3_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT3_WEBGL = 0x8CE3

gl_COLOR_ATTACHMENT4_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT4_WEBGL = 0x8CE4

gl_COLOR_ATTACHMENT5_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT5_WEBGL = 0x8CE5

gl_COLOR_ATTACHMENT6_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT6_WEBGL = 0x8CE6

gl_COLOR_ATTACHMENT7_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT7_WEBGL = 0x8CE7

gl_COLOR_ATTACHMENT8_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT8_WEBGL = 0x8CE8

gl_COLOR_ATTACHMENT9_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT9_WEBGL = 0x8CE9

gl_COLOR_ATTACHMENT10_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT10_WEBGL = 0x8CEA

gl_COLOR_ATTACHMENT11_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT11_WEBGL = 0x8CEB

gl_COLOR_ATTACHMENT12_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT12_WEBGL = 0x8CEC

gl_COLOR_ATTACHMENT13_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT13_WEBGL = 0x8CED

gl_COLOR_ATTACHMENT14_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT14_WEBGL = 0x8CEE

gl_COLOR_ATTACHMENT15_WEBGL :: Num a => a
gl_COLOR_ATTACHMENT15_WEBGL = 0x8CEF


gl_DRAW_BUFFER0_WEBGL :: Num a => a
gl_DRAW_BUFFER0_WEBGL = 0x8825

gl_DRAW_BUFFER1_WEBGL :: Num a => a
gl_DRAW_BUFFER1_WEBGL = 0x8826

gl_DRAW_BUFFER2_WEBGL :: Num a => a
gl_DRAW_BUFFER2_WEBGL = 0x8827

gl_DRAW_BUFFER3_WEBGL :: Num a => a
gl_DRAW_BUFFER3_WEBGL = 0x8828

gl_DRAW_BUFFER4_WEBGL :: Num a => a
gl_DRAW_BUFFER4_WEBGL = 0x8829

gl_DRAW_BUFFER5_WEBGL :: Num a => a
gl_DRAW_BUFFER5_WEBGL = 0x882A

gl_DRAW_BUFFER6_WEBGL :: Num a => a
gl_DRAW_BUFFER6_WEBGL = 0x882B

gl_DRAW_BUFFER7_WEBGL :: Num a => a
gl_DRAW_BUFFER7_WEBGL = 0x882C

gl_DRAW_BUFFER8_WEBGL :: Num a => a
gl_DRAW_BUFFER8_WEBGL = 0x882D

gl_DRAW_BUFFER9_WEBGL :: Num a => a
gl_DRAW_BUFFER9_WEBGL = 0x882E

gl_DRAW_BUFFER10_WEBGL :: Num a => a
gl_DRAW_BUFFER10_WEBGL = 0x882F

gl_DRAW_BUFFER11_WEBGL :: Num a => a
gl_DRAW_BUFFER11_WEBGL = 0x8830

gl_DRAW_BUFFER12_WEBGL :: Num a => a
gl_DRAW_BUFFER12_WEBGL = 0x8831

gl_DRAW_BUFFER13_WEBGL :: Num a => a
gl_DRAW_BUFFER13_WEBGL = 0x8832

gl_DRAW_BUFFER14_WEBGL :: Num a => a
gl_DRAW_BUFFER14_WEBGL = 0x8833

gl_DRAW_BUFFER15_WEBGL :: Num a => a
gl_DRAW_BUFFER15_WEBGL = 0x8834


gl_MAX_COLOR_ATTACHMENTS_WEBGL :: Num a => a
gl_MAX_COLOR_ATTACHMENTS_WEBGL = 0x8CDF

gl_MAX_DRAW_BUFFERS_WEBGL :: Num a => a
gl_MAX_DRAW_BUFFERS_WEBGL = 0x8824


-- WEBGL_color_buffer_float

gl_RGBA32F_EXT :: Num a => a
gl_RGBA32F_EXT = gl_RGBA
-- XXX: ???
-- gl_RGBA32F_EXT = 0x8814

gl_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT :: Num a => a
gl_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT = 0x8211

gl_UNSIGNED_NORMALIZED_EXT :: Num a => a
gl_UNSIGNED_NORMALIZED_EXT = 0x8C17

-- WEBGL_depth_texture

gl_UNSIGNED_INT_24_8_WEBGL :: Num a => a
gl_UNSIGNED_INT_24_8_WEBGL = 0x84FA
