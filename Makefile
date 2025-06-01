.PHONY: all
all: generated/Images.elm

GRADIENT_SRC = $(wildcard public/gradients/*.png)
GRADIENT_OUT = $(patsubst public/gradients/%.png,build/elm-codegen-flags/%.ppm,$(GRADIENT_SRC))
DLCS_SRC = $(wildcard DLCs/*.md)
DLCS_OUT = $(patsubst DLCs/%.md,build/elm-codegen-flags/%.md,$(DLCS_SRC))

generated/Images.elm: codegen/Generate.elm codegen/Data.elm codegen/Gen/Basics.elm build/elm-codegen-flags/sizes $(GRADIENT_OUT) $(DLCS_OUT)
	yarn elm-codegen run --flags-from build/elm-codegen-flags
	elm-format --yes generated

build/elm-codegen-flags/sizes: $(wildcard public/*.png) $(wildcard public/*.jpg) $(wildcard public/*.webp)
	mkdir -p build/elm-codegen-flags
	identify $^ > $@

build/elm-codegen-flags/%.md: DLCs/%.md
	mkdir -p build/elm-codegen-flags
	cp $^ $@

codegen/Gen/Basics.elm: codegen/elm.codegen.json
	yarn elm-codegen install

build/elm-codegen-flags/%.ppm: public/gradients/%.png
	mkdir -p build/elm-codegen-flags
	magick $^ -compress none $@

.PHONY: build
build:
	rm -rf out
	mkdir -p out
	mkdir -p out/public
	mkdir -p out/build
	yarn elm-watch make --optimize
	cp index.html favicon.ico out
	cp public/*.* out/public
	cp build/main.js out/build

.PHONY: run
run: generated/Images.elm
	yarn elm-watch hot

.PHONY: test
test: generated/Images.elm
	yarn elm-test-rs --watch --compiler $(which lamdera)
