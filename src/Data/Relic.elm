module Data.Relic exposing (Content(..), intro)


type Content
    = Single Int String
    | WithChoices (List Int) String
    | CosmicPearlContent Int String


intro : String
intro =
    """
    {center} Relics are like perks, but are external boons in the form of magical artifacts, as a rest they aren’t things inherent to yourself, but are things you can acquire over time.

    {center} "Let’s see if we can detect any Relics in your future, they sometimes show up in these tests..."

    {center} {choice *Relics cost REWARD points obtained from Quests, as shown, however you can buy them with POWER instead, if you so choose.*}
    """
