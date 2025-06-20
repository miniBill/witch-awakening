module Svg.Attributes.Extra exposing (cx, cy, x, y)

import Svg exposing (Attribute)
import Svg.Attributes


x : Float -> Attribute msg
x v =
    Svg.Attributes.x (String.fromFloat v)


y : Float -> Attribute msg
y v =
    Svg.Attributes.y (String.fromFloat v)


cx : Float -> Attribute msg
cx v =
    Svg.Attributes.cx (String.fromFloat v)


cy : Float -> Attribute msg
cy v =
    Svg.Attributes.cy (String.fromFloat v)
