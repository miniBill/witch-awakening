generated/Images.elm: build/sizes codegen/Generate.elm codegen/Gen/Basics.elm
	yarn elm-codegen run --flags-from build/sizes

build/sizes:
	identify public/*.png public/*.jpg > $@

codegen/Gen/Basics.elm: codegen/elm.codegen.json
	yarn elm-codegen install
