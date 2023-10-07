generated/Images.elm: codegen/Generate.elm codegen/Gen/Basics.elm build/elm-codegen-flags/sizes build/elm-codegen-flags/title_gradient.ppm
	yarn elm-codegen run --flags-from build/elm-codegen-flags

build/elm-codegen-flags/sizes:
	mkdir -p build/elm-codegen-flags
	identify public/*.png public/*.jpg > $@

codegen/Gen/Basics.elm: codegen/elm.codegen.json
	yarn elm-codegen install

build/elm-codegen-flags/%.ppm: extract/%.png
	mkdir -p build/elm-codegen-flags
	convert $^ -compress none $@
