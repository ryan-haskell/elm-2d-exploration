module Elm2d.Internals.Node.Rectangle exposing (Props, toWebGLEntity)

import Elm2d.Color exposing (Color)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector4 as Vec4 exposing (Vec4)
import WebGL


type alias Props =
    { fill : Color
    , size : ( Int, Int )
    , camera : Mat4
    , transform : Mat4
    }


type alias Attributes =
    { position : Vec2
    }


type alias Varyings =
    {}


type alias Uniforms =
    { color : Vec4
    , camera : Mat4
    , transform : Mat4
    }


toWebGLEntity : Props -> WebGL.Entity
toWebGLEntity props =
    WebGL.entityWith []
        vertexShader
        fragmentShader
        mesh
        { color = Elm2d.Color.toVector props.fill
        , camera = props.camera
        , transform = props.transform
        }


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
    attribute vec2 position;

    uniform mat4 camera;
    uniform mat4 transform;

    void main() {
        gl_Position = camera * transform * vec4(position, 0.0, 1.0);
    }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
    precision mediump float;

    uniform vec4 color;
    void main() {
        gl_FragColor = color;
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
