module Element.Events exposing
    ( onClick, onDoubleClick, onMouseDown, onMouseUp, onMouseEnter, onMouseLeave, onMouseMove
    , onFocus, onLoseFocus
    -- , onClickCoords
    -- , onClickPageCoords
    -- , onClickScreenCoords
    -- , onMouseCoords
    -- , onMousePageCoords
    -- , onMouseScreenCoords
    )

{-|


## Mouse Events

@docs onClick, onDoubleClick, onMouseDown, onMouseUp, onMouseEnter, onMouseLeave, onMouseMove


## Focus Events

@docs onFocus, onLoseFocus

-}

import Element exposing (Attribute)
import Html.Events
import Internal.Model as Internal
import Json.Decode as Json



-- MOUSE EVENTS


{-| -}
onMouseDown : msg -> Attribute msg
onMouseDown =
    Internal.Attr << Html.Events.onMouseDown


{-| -}
onMouseUp : msg -> Attribute msg
onMouseUp =
    Internal.Attr << Html.Events.onMouseUp


{-| -}
onClick : msg -> Attribute msg
onClick =
    Internal.Attr << Html.Events.onClick


{-| -}
onDoubleClick : msg -> Attribute msg
onDoubleClick =
    Internal.Attr << Html.Events.onDoubleClick


{-| -}
onMouseEnter : msg -> Attribute msg
onMouseEnter =
    Internal.Attr << Html.Events.onMouseEnter


{-| -}
onMouseLeave : msg -> Attribute msg
onMouseLeave =
    Internal.Attr << Html.Events.onMouseLeave


{-| -}
onMouseMove : msg -> Attribute msg
onMouseMove msg =
    on "mousemove" (Json.succeed msg)



-- onClickWith
--     { button = primary
--     , send = localCoords Button
--     }
-- type alias Click =
--     { button : Button
--     , send : Track
--     }
-- type Button = Primary | Secondary
-- type Track
--     = ElementCoords
--     | PageCoords
--     | ScreenCoords
-- |
-- FOCUS EVENTS


{-| -}
onLoseFocus : msg -> Attribute msg
onLoseFocus =
    Internal.Attr << Html.Events.onBlur


{-| -}
onFocus : msg -> Attribute msg
onFocus =
    Internal.Attr << Html.Events.onFocus



-- CUSTOM EVENTS


{-| Create a custom event listener. Normally this will not be necessary, but
you have the power! Here is how `onClick` is defined for example:

    import Json.Decode as Json

    onClick : msg -> Attribute msg
    onClick message =
        on "click" (Json.succeed message)

The first argument is the event name in the same format as with JavaScript's
[`addEventListener`][aEL] function.
The second argument is a JSON decoder. Read more about these [here][decoder].
When an event occurs, the decoder tries to turn the event object into an Elm
value. If successful, the value is routed to your `update` function. In the
case of `onClick` we always just succeed with the given `message`.
If this is confusing, work through the [Elm Architecture Tutorial][tutorial].
It really does help!
[aEL]: <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener>
[decoder]: <http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode>
[tutorial]: <https://github.com/evancz/elm-architecture-tutorial/>

-}
on : String -> Json.Decoder msg -> Attribute msg
on event decode =
    Internal.Attr <| Html.Events.on event decode



-- {-| Same as `on` but you can set a few options.
-- -}
-- onWithOptions : String -> Html.Events.Options -> Json.Decoder msg -> Attribute msg
-- onWithOptions event options decode =
--     Internal.Attr <| Html.Events.onWithOptions event options decode
-- COMMON DECODERS
