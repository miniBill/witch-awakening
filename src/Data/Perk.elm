module Data.Perk exposing (Content(..), hybridizeCost)


type Content
    = Single Int String
    | WithChoices String (List ( String, Int )) String
    | WithCosts (List Int) String


hybridizeCost : number
hybridizeCost =
    6
