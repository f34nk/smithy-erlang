.PHONY: all clean generate test validate

all: clean generate test

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
	rm -rf build _build src/generated/*.erl

validate:
	#
	# Validate the Smithy model
	#
	smithy validate model/
