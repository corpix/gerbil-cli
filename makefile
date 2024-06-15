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

.PHONY: tag
tag:
	git tag v$(version)
