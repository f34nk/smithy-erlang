.PHONY: all clean generate test validate

all: copy-model clean generate test

copy-model:
	#
	# Copy the model from the JAVA test
	#
	cp ../../src/test/resources/models/aws-test-s3.smithy model/

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
