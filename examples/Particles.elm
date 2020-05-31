module Particles exposing (main)

import Angle exposing (Angle)
import Block3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Common.Materials as Materials
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Html.Events
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters, meters)
import LuminousFlux
import Pixels exposing (Pixels, pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Entity exposing (translateBy)
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d
import Task
import Time exposing (posixToMillis)
import Vector3d
import Viewpoint3d


type World
    = World


type alias Model =
    { entities : List (Entity World)
    , azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , frames : Int
    , time : Float
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove Float Float
    | Resize (Quantity Float Pixels) (Quantity Float Pixels)
    | AnimationFrame
    | Tick Int


floor : Scene3d.Entity World
floor =
    Scene3d.block Materials.aluminum <|
        Block3d.with
            { x1 = meters -10
            , x2 = meters 10
            , y1 = meters -10
            , y2 = meters 10
            , z1 = meters -2.2
            , z2 = meters -2
            }


aluminumSphereAt : Float -> Float -> Float -> Scene3d.Entity World
aluminumSphereAt x y z =
    Scene3d.sphereWithShadow (Material.uniform Materials.aluminum) <|
        Sphere3d.withRadius (meters 0.5) (Point3d.meters x y z)


sphereAt : Float -> Float -> Float -> Scene3d.Entity World
sphereAt x y z =
    Scene3d.sphereWithShadow (Material.uniform Materials.copper) <|
        Sphere3d.withRadius (meters 0.5) (Point3d.meters x y z)


camera : Model -> Camera3d Meters World
camera model =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = Point3d.meters 0 0 -2
                , groundPlane = SketchPlane3d.xy
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 50
                }
        , verticalFieldOfView = Angle.degrees 35
        }


lightBulb : Light World Bool
lightBulb =
    Light.point (Light.castsShadows True)
        { chromaticity = Light.incandescent
        , intensity = LuminousFlux.lumens 3000000
        , position = Point3d.meters 0 0 5
        }


overheadLighting : Light World Never
overheadLighting =
    Light.overhead
        { upDirection = Direction3d.positiveZ
        , chromaticity = Light.fluorescent
        , intensity = Illuminance.lux 4000
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MouseDown ->
            ( { model | orbiting = True }, Debug.log "Clicked!" Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotation numPixels =
                        Angle.degrees (0.25 * numPixels)

                    newAzimuth =
                        model.azimuth |> Quantity.minus (rotation dx)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (rotation dy)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }, Cmd.none )

            else
                ( model, Cmd.none )

        Resize w h ->
            ( { model | width = w, height = h }, Debug.log "Resizing" Cmd.none )

        AnimationFrame ->
            ( { model | frames = model.frames + 1 }, Cmd.none )

        Tick ts ->
            let
                t =
                    toFloat ts / 1000

                transformer i ent =
                    let
                        v =
                            toFloat (i // maxW)

                        u =
                            toFloat (modBy maxW i)

                        dx =
                            0.05 * sin (t + u / 5.0)

                        dy =
                            0.07 * cos (t + v / 5.0)

                        dz =
                            0.01 * sin t  + 0.01 * sin (3.14     * v / maxW + t)
                    in
                    translateBy (Vector3d.meters dx dy dz) ent

                newEnts =
                    model.entities
                        |> List.indexedMap transformer
            in
            ( { model | time = t, entities = newEnts }, Cmd.none )


init : () -> ( Model, Cmd Msg )
init () =
    ( { entities = allEntities
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 40
      , orbiting = False
      , width = pixels 0
      , height = pixels 0
      , frames = 0
      , time = 0
      }
    , Task.perform
        (\{ viewport } ->
            Resize (pixels viewport.width) (pixels viewport.height)
        )
        Browser.Dom.getViewport
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ Html.Events.onMouseDown MouseDown ]
            [ Scene3d.custom
                { lights = Scene3d.twoLights lightBulb overheadLighting
                , camera = camera model
                , clipDepth = meters 0.1
                , dimensions = ( model.width, model.height )
                , antialiasing = Scene3d.multisampling
                , exposure = Scene3d.exposureValue 12
                , toneMapping = Scene3d.hableFilmicToneMapping
                , whiteBalance = Light.fluorescent
                , background = Scene3d.backgroundColor (Color.rgba 0 0 0 1.0)
                , entities = floor :: model.entities
                }
            ]
        ]


maxW =
    5


maxH =
    5


allEntities : List (Entity World)
allEntities =
    List.range 0 (maxW * maxH - 1)
        |> List.map (\i -> sphereAt (2 * toFloat (modBy maxW i) - toFloat maxW / 2) (2 * toFloat i / maxW - toFloat maxH / 2) 3)


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            if model.orbiting then
                [ Browser.Events.onMouseMove decodeMouseMove
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

            else
                [ Browser.Events.onResize
                    (\width height ->
                        Resize (pixels (toFloat width)) (pixels (toFloat height))
                    )
                ]
    in
    Sub.batch <| Time.every (1000 / 60) (\x -> Tick <| posixToMillis x) :: subs


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
