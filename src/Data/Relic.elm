module Data.Relic exposing (Content(..))


type Content
    = Single Int String
    | WithChoices (List Int) String
    | CosmicPearlContent Int String
