.DEFAULT_GOAL = all

version := $(shell date +"%Y-%m-%d").$(shell git rev-list --count HEAD)

.PHONY: all
all: build

.PHONY: build
build:
	gxc cli.ss

.PHONY: test
test: build
	gerbil test

.PHONY: example
example:
	gxc -V -exe -o example ./example.ss

.PHONY: tag
tag:
	git tag v$(version)
