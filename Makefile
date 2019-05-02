ELM_FILES = $(shell find . -path ./elm.json -prune -o -type f -name '*.elm' ! -wholename './src/Main.elm' ! -wholename './src/Rule.elm')

all: build $(ELM_FILES) build/js/Rule.js build/js/Main.js

.PHONY: clean build

build/js/%.js: src/%.elm
	elm make $< --optimize --output $@

build:
	mkdir -p build
	cp -r static/* build/
	cp README.md build/index.md

clean:
	rm -rf build
