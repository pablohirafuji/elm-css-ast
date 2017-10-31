module CssAst.Animations
    exposing
        ( Animation(..)
        , declarations
        , SingleAnimation
        , SingleAnimationDirection(..)
        , SingleAnimationFillMode(..)
        , SingleAnimationIterationCount(..)
        , AnimationName(..)
        , SingleAnimationPlayState(..)
        , SingleTimingFunction(..)
        , CubicBezierTimingFunction(..)
        , StepTimingFunction(..)
        , StepPos(..)
        )

{-| [CSS Animations](https://drafts.csswg.org/css-animations-1/)

@docs Animation, SingleAnimation, SingleAnimationDirection, SingleAnimationFillMode, SingleAnimationIterationCount, AnimationName, SingleAnimationPlayState, SingleTimingFunction, CubicBezierTimingFunction, StepTimingFunction, StepPos, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, map2)
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe)
import CssAst.Values exposing (Time, identifier, time, number, integer)


{-| The Animation type.

[Source](https://drafts.csswg.org/css-animations-1/#property-index).

-}
type Animation
    = Animation SingleAnimation (List SingleAnimation)
    | AnimationDelay Time (List Time)
    | AnimationDirection SingleAnimationDirection (List SingleAnimationDirection)
    | AnimationDuration Time (List Time)
    | AnimationFillMode SingleAnimationFillMode (List SingleAnimationFillMode)
    | AnimationIterationCount SingleAnimationIterationCount (List SingleAnimationIterationCount)
    | AnimationName AnimationName (List AnimationName)
    | AnimationPlayState SingleAnimationPlayState (List SingleAnimationPlayState)
    | AnimationTimingFunction SingleTimingFunction (List SingleTimingFunction)


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Animation )
declarations =
    [ ( "animation"
      , oneOrMoreCommaList Animation singleAnimation
      )
    , ( "animation-delay"
      , oneOrMoreCommaList AnimationDelay time
      )
    , ( "animation-direction"
      , oneOrMoreCommaList AnimationDirection singleAnimationDirection
      )
    , ( "animation-duration"
      , oneOrMoreCommaList AnimationDuration time
      )
    , ( "animation-fill-mode"
      , oneOrMoreCommaList AnimationFillMode singleAnimationFillMode
      )
    , ( "animation-iteration-count"
      , oneOrMoreCommaList AnimationIterationCount singleAnimationIterationCount
      )
    , ( "animation-name"
      , oneOrMoreCommaList AnimationName animationName
      )
    , ( "animation-play-state"
      , oneOrMoreCommaList AnimationPlayState singleAnimationPlayState
      )
    , ( "animation-timing-function"
      , oneOrMoreCommaList AnimationTimingFunction singleTimingFunction
      )
    ]


{-| The [`<single-animation>`](https://drafts.csswg.org/css-animations-1/#typedef-single-animation) data type.

Value definition syntax:
`<time> || <single-timing-function> || <time> || <single-animation-iteration-count> || <single-animation-direction> || <single-animation-fill-mode> || <single-animation-play-state> || [ none | <keyframes-name> ]`

-}
type alias SingleAnimation =
    { duration : Maybe Time
    , timingFunction : Maybe SingleTimingFunction
    , delay : Maybe Time
    , iterationCount : Maybe SingleAnimationIterationCount
    , direction : Maybe SingleAnimationDirection
    , fillMode : Maybe SingleAnimationFillMode
    , playState : Maybe SingleAnimationPlayState
    , name : Maybe AnimationName
    }


singleAnimation : Parser SingleAnimation
singleAnimation =
    singleAnimationHelp [] singleAnimationList emptySingleAnimation


singleAnimationHelp : List (SingleAnimation -> Parser SingleAnimation) -> List (SingleAnimation -> Parser SingleAnimation) -> SingleAnimation -> Parser SingleAnimation
singleAnimationHelp nsHead nsTail n =
    case nsTail of
        head :: tail ->
            oneOf
                [ succeed identity
                    |= head n
                    |. whitespace
                    |> andThen (singleAnimationHelp [] (nsHead ++ tail))
                , singleAnimationHelp (nsHead ++ [ head ]) tail n
                ]

        [] ->
            if n.duration == Nothing && n.timingFunction == Nothing && n.delay == Nothing && n.iterationCount == Nothing && n.direction == Nothing && n.fillMode == Nothing && n.playState == Nothing && n.name == Nothing then
                fail "Empty `animation` property value."
            else
                succeed n


singleAnimationList : List (SingleAnimation -> Parser SingleAnimation)
singleAnimationList =
    [ \r -> map (\n -> { r | duration = Just n }) time
    , \r -> map (\n -> { r | timingFunction = Just n }) singleTimingFunction
    , \r -> map (\n -> { r | delay = Just n }) time
    , \r -> map (\n -> { r | iterationCount = Just n }) singleAnimationIterationCount
    , \r -> map (\n -> { r | direction = Just n }) singleAnimationDirection
    , \r -> map (\n -> { r | fillMode = Just n }) singleAnimationFillMode
    , \r -> map (\n -> { r | playState = Just n }) singleAnimationPlayState
    , \r -> map (\n -> { r | name = Just n }) animationName
    ]


emptySingleAnimation : SingleAnimation
emptySingleAnimation =
    { duration = Nothing
    , timingFunction = Nothing
    , delay = Nothing
    , iterationCount = Nothing
    , direction = Nothing
    , fillMode = Nothing
    , playState = Nothing
    , name = Nothing
    }


{-| The possible values for the [`<single-animation-direction>`](https://drafts.csswg.org/css-animations-1/#typedef-single-animation-direction) data type.

Value definition syntax:
`normal | reverse | alternate | alternate-reverse`

-}
type SingleAnimationDirection
    = Normal
    | Reverse
    | Alternate
    | AlternateReverse


singleAnimationDirection : Parser SingleAnimationDirection
singleAnimationDirection =
    keywordsToType
        [ ( "normal", Normal )
        , ( "reverse", Reverse )
        , ( "alternate", Alternate )
        , ( "alternate-reverse", AlternateReverse )
        ]


{-| The possible values for the [`<single-animation-fill-mode>`](https://drafts.csswg.org/css-animations-1/#typedef-single-animation-fill-mode) data type.

Value definition syntax:
`none | forwards | backwards | both`

-}
type SingleAnimationFillMode
    = NoAnimationFillMode
    | Forwards
    | Backwards
    | Both


singleAnimationFillMode : Parser SingleAnimationFillMode
singleAnimationFillMode =
    keywordsToType
        [ ( "none", NoAnimationFillMode )
        , ( "forwards", Forwards )
        , ( "backwards", Backwards )
        , ( "both", Both )
        ]


{-| The possible values for the [`<single-animation-iteration-count>`](https://drafts.csswg.org/css-animations-1/#typedef-single-animation-iteration-count) data type.

Value definition syntax:
`infinite | <number>`

-}
type SingleAnimationIterationCount
    = Infinite
    | Number Float


singleAnimationIterationCount : Parser SingleAnimationIterationCount
singleAnimationIterationCount =
    oneOf
        [ keyword "infinite" |> map (always Infinite)
        , number |> map Number
        ]


{-| The possible values for the [`animation-name`](https://drafts.csswg.org/css-animations-1/#propdef-animation-name) property.

Value definition syntax:
`[ none | <custom-identifier> | <string> ]#`

TODO: <string>

-}
type AnimationName
    = NoAnimationName
    | KeyframeName String


animationName : Parser AnimationName
animationName =
    oneOf
        [ keyword "none" |> map (always NoAnimationName)
        , identifier |> map KeyframeName
        ]


{-| The possible values for the [`single-animation-play-state`](https://drafts.csswg.org/css-animations-1/#typedef-single-animation-play-state) data type.

Value definition syntax:
`running | paused`

-}
type SingleAnimationPlayState
    = Running
    | Paused


singleAnimationPlayState : Parser SingleAnimationPlayState
singleAnimationPlayState =
    keywordsToType
        [ ( "running", Running )
        , ( "paused", Paused )
        ]


{-| The possible values for the [`single-timing-function`](https://drafts.csswg.org/css-timing-1/#typedef-single-timing-function) data type.

Value definition syntax:
`linear | <cubic-bezier-timing-function> | <step-timing-function> | <frames-timing-function>`

-}
type SingleTimingFunction
    = Linear
    | CubicBezierTimingFunction CubicBezierTimingFunction
    | StepTimingFunction StepTimingFunction
    | FramesTimingFunction Int


singleTimingFunction : Parser SingleTimingFunction
singleTimingFunction =
    oneOf
        [ keyword "linear" |> map (always Linear)
        , cubicBezierTimingFunction |> map CubicBezierTimingFunction
        , stepTimingFunction |> map StepTimingFunction
        , succeed FramesTimingFunction
            |. keyword "frames("
            |. whitespace
            |= integer
            |. whitespace
            |. symbol ")"
        ]


{-| The possible values for the [`<cubic-bezier-timing-function>`](https://drafts.csswg.org/css-timing-1/#typedef-cubic-bezier-timing-function) data type.

Value definition syntax:
`ease | ease-in | ease-out | ease-in-out | cubic-bezier(<number>, <number>, <number>, <number>)`

-}
type CubicBezierTimingFunction
    = Ease
    | EaseIn
    | EaseOut
    | EaseInOut
    | CubicBezier Float Float Float Float


cubicBezierTimingFunction : Parser CubicBezierTimingFunction
cubicBezierTimingFunction =
    oneOf
        [ keyword "ease" |> map (always Ease)
        , keyword "ease-in" |> map (always EaseIn)
        , keyword "ease-out" |> map (always EaseOut)
        , keyword "ease-in-out" |> map (always EaseInOut)
        , succeed CubicBezier
            |. keyword "cubic-bezier("
            |. whitespace
            |= number
            |. whitespace
            |. symbol ","
            |. whitespace
            |= number
            |. whitespace
            |. symbol ","
            |. whitespace
            |= number
            |. whitespace
            |. symbol ","
            |. whitespace
            |= number
            |. whitespace
            |. symbol ")"
        ]


{-| The possible values for the [`<step-timing-function>`](https://drafts.csswg.org/css-timing-1/#typedef-step-timing-function) data type.

Value definition syntax:
`step-start | step-end | steps(<integer>[, [ start | end ] ]?)`

-}
type StepTimingFunction
    = StepStart
    | StepEnd
    | Steps Int StepPos


stepTimingFunction : Parser StepTimingFunction
stepTimingFunction =
    oneOf
        [ keyword "step-start" |> map (always StepStart)
        , keyword "step-end" |> map (always StepEnd)
        , succeed Steps
            |. keyword "steps("
            |. whitespace
            |= integer
            |. whitespace
            |= oneOf
                [ Parser.delayedCommit
                    (symbol "," |. whitespace)
                    (oneOf
                        [ keyword "start" |> map (always Start)
                        , keyword "end" |> map (always End)
                        ]
                    )
                , succeed NoStepPos
                ]
            |. whitespace
            |. symbol ")"
        ]


{-| Possible values for the [`steps`](https://drafts.csswg.org/css-timing-1/#typedef-step-timing-function) function.

Value definition syntax:
`[ start | end ]?`

-}
type StepPos
    = NoStepPos
    | Start
    | End
