REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

ifeq ($(wildcard rebar3),rebar3)
	REBAR3 = $(CURDIR)/rebar3
endif

ifdef RUNNING_ON_CI
REBAR3 = ./rebar3
else
REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")
endif

ifeq ($(REBAR3),)
	REBAR3 = $(CURDIR)/rebar3
endif

TEST_PROFILE ?= test

.PHONY: all build clean check dialyzer xref test cover benchmark doc publish

all: build

build: $(REBAR3)
	@$(REBAR3) compile

$(REBAR3):
	wget $(REBAR3_URL) || curl -Lo rebar3 $(REBAR3_URL)
	@chmod a+x rebar3

clean: $(REBAR3)
	@$(REBAR3) clean

check: dialyzer xref

dialyzer: $(REBAR3)
	@$(REBAR3) dialyzer

xref: $(REBAR3)
	@$(REBAR3) xref

test: $(REBAR3)
	@$(REBAR3) as $(TEST_PROFILE) eunit

cover: test
	@$(REBAR3) as $(TEST_PROFILE) cover

benchmark:
	make -C steady_vector_bench/ run

doc: build
	./scripts/hackish_make_docs.sh

publish: $(REBAR3)
	@$(REBAR3) as publication hex publish
