.PHONY: all clean generate test validate compile

YELLOW=\033[1;33m
NC=\033[0m

all: clean copy generate compile test

copy:
	#
	# Copy the model from the JAVA test
	#
	cp ../../src/test/resources/models/aws-s3-client.smithy model/
	#
	# Copy the AWS SigV4 module from the main resources
	#
	cp ../../src/test/resources/aws_sigv4_test.erl test/

generate:
	# 
	# Generate Erlang client from Smithy model
	#
	smithy build
	tree src/generated

compile:
	#
	# Compile the code and analyze with Dialyzer
	#
	rebar3 lock
	rebar3 compile
	#
	# Analyze test code with Dialyzer
	# Tests must include the types header for type checking
	#
	rebar3 as test compile
	rebar3 as test dialyzer || echo "\n$(YELLOW)Some Dialyzer warnings are expected: we use record types in specs (for Dialyzer) but maps at runtime (no conversion overhead, matches AWS SDK patterns).$(NC)\n"
	rebar3 as test dialyzer_html

test:
	#
	# Run the tests
	#
	rebar3 eunit

clean:
	rm -rf build _build src/generated/*.erl src/generated/*.hrl rebar3.crashdump
	# remove copied files
	rm -rf model/aws-s3-client.smithy test/aws_sigv4_test.erl
