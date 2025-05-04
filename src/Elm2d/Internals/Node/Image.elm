module Elm2d.Internals.Node.Image exposing (Props, toWebGLEntity)

import Elm2d.Color exposing (Color)
import Elm2d.Vector as Vector
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector4 as Vec4 exposing (Vec4)
import WebGL
import WebGL.Settings as Settings
import WebGL.Texture as Texture exposing (Texture)


type alias Props =
    { texture : Texture
    , size : ( Int, Int )
    , offset : ( Int, Int )
    , flipX : Bool
    , flipY : Bool
    , camera : Mat4
    , transform : Mat4
    }


type alias Attributes =
    { position : Vec2
    }


type alias Varyings =
    { v_position : Vec2 }


type alias Uniforms =
    { texture : Texture
    , textureSize : Vec2
    , offset : Vec2
    , size : Vec2
    , flipX : Bool
    , flipY : Bool
    , camera : Mat4
    , transform : Mat4
    }


toWebGLEntity : Props -> WebGL.Entity
toWebGLEntity props =
    WebGL.entityWith [ Settings.sampleAlphaToCoverage ]
        vertexShader
        fragmentShader
        mesh
        { texture = props.texture
        , textureSize = Vector.fromIntTuple (Texture.size props.texture)
        , offset = Vector.fromIntTuple props.offset
        , size = Vector.fromIntTuple props.size
        , flipX = props.flipX
        , flipY = props.flipY
        , camera = props.camera
        , transform = props.transform
        }


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
    attribute vec2 position;

    uniform mat4 camera;
    uniform mat4 transform;
    uniform bool flipX;
    uniform bool flipY;

    varying vec2 v_position;

    void main() {
        gl_Position = camera * transform * vec4(position, 0.0, 1.0);
        
        v_position = position;
        if (flipX) {
            v_position.x = 1.0 - position.x;
        }
        if (flipY) {
            v_position.y = 1.0 - position.y;
        }
    }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
    precision mediump float;

    uniform sampler2D texture;
    uniform vec2 textureSize;
    uniform vec2 offset;
    uniform vec2 size;

    varying vec2 v_position;
    
    void main() {
        vec2 uv = (v_position * size + offset) / textureSize;
        
        // Account for half-pixel offset
        // uv += vec2(0.5 / textureSize.x, 0.5 / textureSize.y);

        vec4 textureColor = texture2D(texture, uv);

        // Helpful for debugging image rendering
        // if (textureColor.a < 0.5) {
        //     gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
        //     return;
        // }

        gl_FragColor = textureColor;
    }
    |]


mesh : WebGL.Mesh Attributes
mesh =
    let
        v0 =
            { position = Vec2.vec2 0 0 }

        v1 =
            { position = Vec2.vec2 1 0 }

        v2 =
            { position = Vec2.vec2 1 1 }

        v3 =
            { position = Vec2.vec2 0 1 }
    in
    WebGL.indexedTriangles [ v0, v1, v2, v3 ] [ ( 0, 1, 2 ), ( 2, 3, 0 ) ]
