module Data.Class exposing (Details, intro, title)

import Generated.Types exposing (Class)


type alias Details =
    { name : Class
    , dlc : Maybe String
    , content : String
    }


title : String
title =
    "# True Form - Class"


intro : String
intro =
    """
    "Ahh, yes... Oh, _wow_! You have an incredible amount of untapped power waiting. First things first: You’ll need your true form! We used to simply wait for it to emerge, but these days we can poke and prod the right places to provoke a controlled early awakening. Most witches have multiple potential true forms and one gets locked in when they finally awaken, but with a controlled environment we can force one of the others. Your options don’t represent all possible outcomes, but let’s see what you have available. First up is what type of witch you are, you can think of it like a “Class” of witch."

    Your witch type determines your method by which you _can naturally progress over time_ towards a power cap. _You_ will have the same power cap and starting power regardless of type "and you’re lucky! You’ve got more than most witches, and it looks like you might be capable of using Rank 5 magic, the _average_ witch only reaches rank 3", You can pre-spend up to your power cap to confirm you have the potential for something to unlock someday, if you wish. It’s up to you how well adapted you are to your starting abilities, perhaps you want to study them for some time before you have a full grasp on them?

    {center}{choice _*Choose one.*_}
    """
