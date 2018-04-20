module Colour exposing (Lab, Rgb, labLerp, toCss)


type alias Lab =
    { l : Float
    , a : Float
    , b : Float
    }


type alias Xyz =
    { x : Float
    , y : Float
    , z : Float
    }


type alias RgbLinear =
    { r : Float
    , g : Float
    , b : Float
    }


type alias Rgb =
    { r : Int
    , g : Int
    , b : Int
    }


labToRgb : Lab -> Rgb
labToRgb =
    labToXyz >> xyzToRgbLinear >> rgbLinearToRgb


labF : Float -> Float
labF t =
    let
        t3 =
            t * t * t
    in
    if t3 > 0.008856 then
        t3
    else
        (t - 0.13793103448) / 7.787


d65 : Xyz
d65 =
    Xyz 95.047 100.0 108.883


labToXyz : Lab -> Xyz
labToXyz { l, a, b } =
    let
        yy =
            (l + 16.0) / 116.0

        xx =
            yy + (a / 500.0)

        zz =
            yy - (b / 200.0)
    in
    Xyz (d65.x * labF xx) (d65.y * labF yy) (d65.z * labF zz)


xyzToRgbLinear : Xyz -> RgbLinear
xyzToRgbLinear { x, y, z } =
    let
        xx =
            x / 100.0

        yy =
            y / 100.0

        zz =
            z / 100.0

        r =
            3.2406 * xx - 1.5372 * yy - 0.4986 * zz

        g =
            -0.9689 * xx + 1.8758 * yy + 0.0415 * zz

        b =
            0.0557 * xx - 0.204 * yy + 1.057 * zz
    in
    RgbLinear r g b


gamma : Float -> Float
gamma x =
    if x > 0.0031308 then
        1.055 * x ^ (1.0 / 2.4) - 0.055
    else
        12.92 * x


rgbLinearToRgb : RgbLinear -> Rgb
rgbLinearToRgb { r, g, b } =
    let
        f =
            gamma >> (*) 256 >> round >> max 0 >> min 255
    in
    Rgb (f r) (f g) (f b)


lerp : a -> a -> Float -> (a -> Float) -> Float
lerp a b t proj =
    let
        pa =
            proj a

        pb =
            proj b
    in
    (pb - pa) * t + pa


labLerp : Lab -> Lab -> Int -> Int -> Int -> Rgb
labLerp c1 c2 xa xb x =
    let
        xx =
            max xa x |> min xb

        t =
            toFloat (xx - xa) / toFloat (xb - xa)

        lerp_ =
            lerp c1 c2 t
    in
    Lab (lerp_ .l) (lerp_ .a) (lerp_ .b) |> labToRgb


toCss : Rgb -> String
toCss { r, g, b } =
    "rgb("
        ++ toString r
        ++ ","
        ++ toString g
        ++ ","
        ++ toString b
        ++ ")"
