.PHONY: all
all: generated/Images.elm

GRADIENT_SRC = $(wildcard public/gradients/*.png)
GRADIENT_OUT = $(patsubst public/gradients/%.png,build/elm-codegen-flags/%.ppm,$(GRADIENT_SRC))
DLCS_SRC = $(wildcard DLCs/**/*.md)
DLCS_OUT = $(patsubst DLCs/%.md,build/elm-codegen-flags/%.md,$(DLCS_SRC))

generated/Images.elm: $(wildcard codegen/*.elm) $(wildcard codegen/Generate/*.elm) codegen/Gen/Basics.elm build/elm-codegen-flags/sizes $(GRADIENT_OUT) $(DLCS_OUT)
	yarn elm-codegen run --flags-from build/elm-codegen-flags
	yarn elm-format --yes generated

build/elm-codegen-flags/sizes: $(wildcard public/*.png) $(wildcard public/*.webp) $(wildcard public/[A-Z]*/*.webp) $(wildcard public/[A-Z]*/*.png)
	mkdir -p build/elm-codegen-flags
	identify $^ > $@

build/elm-codegen-flags/%.md: DLCs/%.md
	mkdir -p $(dir $@)
	cp $^ $@

codegen/Gen/Basics.elm: codegen/elm.codegen.json
	yarn elm-codegen install

build/elm-codegen-flags/%.ppm: public/gradients/%.png
	mkdir -p build/elm-codegen-flags
	magick $^ -compress none $@

out/build/main.js: generated/Images.elm $(wildcard src/**/*.elm) $(wildcard src/*.elm)
	rm -rf out
	mkdir -p out
	mkdir -p out/public
	mkdir -p out/build
	yarn elm-watch make --optimize
	cp -r index.html favicon.ico favicon out
	cp public/*.* out/public
	cp build/main.js out/build

# Utils
.PHONY: build
build: out/build/main.js

.PHONY: run
run: generated/Images.elm
	yarn elm-watch hot

.PHONY: test
test: generated/Images.elm
	yarn elm-test-rs --watch --compiler $(which lamdera)

.PHONY: deploy
deploy:
	yarn netlify deploy --prod
