.PHONY: build quick full

build:
	mix deps.get
	mix deps.compile

quick: build
	mix run bench/bench.exs quick

full: build
	mix run bench/bench.exs full
