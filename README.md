# Elm CSS Ast

A parser for CSS code.

Work in Progress.

## About the AST design

### Selector API

To justify selectors API, from the specs:

    .my-class == *.my-class == *[class~="my-class"]
    #my-id == *#my-id == *[id="my-id"]

### Impossible states

The AST types try to make impossible CSS states impossible. But, for readability/usage reasons, sometimes is possible to obtain impossible CSS states.

For example, the [`<position>`](https://drafts.csswg.org/css-values-4/#typedef-position) data type has this value definition syntax:
```
[
  [ left | center | right ] || [ top | center | bottom ]
|
  [ left | center | right | <length-percentage> ]
  [ top | center | bottom | <length-percentage> ]?
|
  [ [ left | right ] <length-percentage> ] &&
  [ [ top | bottom ] <length-percentage> ]
]
```

The representation of an impossible state type, that I can think of, would be:
```elm
type Position
	= OnlyLeftCenterRight LeftCenterRight
	| OnlyTopCenterBottom TopCenterBottom
	| LeftCenterRightAndTopCenterBottom LeftCenterRight TopCenterBottom
	| OrLengthPercentage LeftCenterRightLengthPercentage (Maybe TopCenterBottomLengthPercentage)
	| WithLengthPercentage ( LeftRight, LengthPercentage ) ( TopBottom, LengthPercentage )


type LeftCenterRightLengthPercentage
	= LeftCenterRight LeftCenterRight
	| LCRLengthPercentage LengthPercentage


type TopCenterBottomLengthPercentage
	= TopCenterBottom TopCenterBottom
	| TCBLengthPercentage LengthPercentage


type LeftCenterRight
	= LCRCenter
	| LeftRight LeftRight


type TopCenterBottom
	= TCBCenter
	| TopBottom TopBottom


type LeftRight
	= Left
	| Right


type TopBottom
	= Top
	| Bottom
```

Instead, it is:
```elm
type alias Position =
    ( PositionValue, Maybe PositionValue )


type PositionValue
    = CenterPosition
    | SidePosition Side (Maybe LengthPercentage)
    | LengthPercentagePosition LengthPercentage


type Side
    = Top
    | Right
    | Bottom
    | Left
```
And there is a function that validates the state.

To think: Represent the AST with default values?