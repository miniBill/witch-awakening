module ReviewConfig exposing (config)

import Docs.ReviewAtDocs
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import ReviewPipelineStyles
import ReviewPipelineStyles.Premade
import Simplify


config : List Rule
config =
    [ Docs.ReviewAtDocs.rule
        |> Rule.ignoreErrorsForDirectories [ "generated" ]
    , NoConfusingPrefixOperator.rule
    , NoDebug.Log.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
        |> Rule.ignoreErrorsForDirectories [ "generated" ]
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForDirectories [ "generated" ]
        |> Rule.ignoreErrorsForFiles [ "src/Data/Costs/Monad.elm" ]
    , NoUnused.Parameters.rule
        |> Rule.ignoreErrorsForDirectories [ "generated" ]
    , NoUnused.Patterns.rule
        |> Rule.ignoreErrorsForDirectories [ "generated" ]
    , NoUnused.Variables.rule
        |> Rule.ignoreErrorsForDirectories [ "generated" ]
    , Simplify.rule Simplify.defaults
        |> Rule.ignoreErrorsForDirectories [ "generated" ]
    , ReviewPipelineStyles.rule pipelineConfig
        |> Rule.ignoreErrorsForDirectories [ "generated" ]
    ]
        |> List.map (Rule.ignoreErrorsForDirectories [ "vendored" ])


pipelineConfig : List (ReviewPipelineStyles.PipelineRule ())
pipelineConfig =
    List.concat
        [ -- ReviewPipelineStyles.Premade.noSingleLineRightPizza
          -- , ReviewPipelineStyles.Premade.noMultilineLeftPizza
          -- , ReviewPipelineStyles.Premade.noPipelinesWithSimpleInputs
          --,
          ReviewPipelineStyles.Premade.noRepeatedParentheticalApplication
        , ReviewPipelineStyles.Premade.noPipelinesWithConfusingNonCommutativeFunctions
        , ReviewPipelineStyles.Premade.noSemanticallyInfixFunctionsInLeftPipelines
        ]
