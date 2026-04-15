module Main exposing (run)

import BackendTask exposing (BackendTask)
import Build
import FatalError exposing (FatalError)
import Hash
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import WitchAwakeningBuildfile


run : Script
run =
    Script.withoutCliOptions toTask


toTask : BackendTask FatalError ()
toTask =
    let
        input : { inputDirectory : Path }
        input =
            { inputDirectory = Path.path "../media" }
    in
    Build.toTask
        { getInputs = WitchAwakeningBuildfile.getInputs input
        , buildAction = WitchAwakeningBuildfile.buildAction input
        , debug = False
        , buildDirectory = Path.path "../build"
        , outputName = Path.path "../dist"
        , removeStale = True
        , jobs = Nothing
        , hashKind = Hash.Fast
        }
