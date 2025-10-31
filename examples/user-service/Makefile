.PHONY: all clean generate test validate compile

all: clean generate test compile

generate:
	# 
	# Generate Erlang client from Smithy model
	#
	smithy build
	tree src/generated

test:
	#
	# Run the tests
	#
	rebar3 eunit

clean:
	rm -rf build _build src/generated/*.erl rebar3.crashdump

validate:
	#
	# Validate the Smithy model
	#
	smithy validate model/

compile:
	#
	# Compile the code and analyze with Dialyzer
	#
	rebar3 lock
	rebar3 compile
	rebar3 dialyzer
