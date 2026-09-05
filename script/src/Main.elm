module Main exposing (run)

import BackendTask exposing (BackendTask)
import Build
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Path.Posix as Path exposing (Path)
import WitchAwakeningBuildfile


run : Script
run =
    Script.withoutCliOptions toTask


toTask : BackendTask FatalError ()
toTask =
    case Path.parseRelativeDirectory "../media" of
        Nothing ->
            BackendTask.fail (FatalError.fromString "Failed to parse `../media` as a path")

        Just inputDirectory ->
            let
                input :
                    { inputDirectory : Path Path.Relative Path.Directory
                    , buildPath : String
                    , debug : Bool
                    }
                input =
                    { inputDirectory = inputDirectory
                    , buildPath = "../build/elm-build"
                    , debug = False
                    }

                config : Build.Config { inputPath : String }
                config =
                    { debug = False
                    , inputPath = "../media"
                    , buildPath = "../build/elm-build"
                    , outputName = "../build/dist"
                    , removeStale = True
                    , jobs = Nothing
                    , hashKind = Build.fastHash
                    , check = False
                    , keepFailed = False
                    }
            in
            Build.toTask WitchAwakeningBuildfile.buildFile config


orDie : Maybe (Path Path.Relative kind) -> Path Path.Relative kind
orDie path =
    case path of
        Just p ->
            p

        Nothing ->
            let
                _ =
                    modBy 0 0
            in
            orDie path
