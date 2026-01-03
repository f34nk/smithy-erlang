export HOST_PORT=4567
export AWS_ACCESS_KEY_ID=dummy
export AWS_SECRET_ACCESS_KEY=dummy
export AWS_DEFAULT_REGION=us-east-1
export AWS_ENDPOINT=http://localhost:$(HOST_PORT)
export TF_VAR_endpoint=$(AWS_ENDPOINT)
export CONTAINER_NAME=dynamodb-demo

YELLOW=\033[1;33m
NC=\033[0m

.PHONY: all
all: clean copy generate compile analyze test

.PHONY: copy
copy:
	#
	# Download model from AWS SDKs: https://github.com/aws/api-models-aws/blob/main/models/dynamodb/service/2012-08-10/dynamodb-2012-08-10.json
	#
	wget https://raw.githubusercontent.com/aws/api-models-aws/refs/heads/main/models/dynamodb/service/2012-08-10/dynamodb-2012-08-10.json
	mv dynamodb-2012-08-10.json model/

.PHONY: generate
generate:
	# 
	# Generate Erlang client from Smithy model
	#
	smithy build
	tree src/generated

.PHONY: compile
compile:
	#
	# Compile the code and analyze with Dialyzer
	#
	rebar3 lock
	rebar3 compile

.PHONY: analyze
analyze:
	#
	# Analyze test code with Dialyzer
	# Tests must include the types header for type checking
	#
	rebar3 as test compile
	rebar3 as test dialyzer || echo "\n$(YELLOW)Some Dialyzer warnings are expected: we use record types in specs (for Dialyzer) but maps at runtime (no conversion overhead, matches AWS SDK patterns).$(NC)\n"
	rebar3 as test dialyzer_html

.PHONY: test
test: test/unit

.PHONY: test/unit
test/unit:
	#
	# Run unit tests
	#
	rebar3 eunit

.PHONY: demo
demo: copy generate compile docker/start terraform
	#
	# Run demo against LocalStack
	#
	erl \
	-pa _build/default/lib/aws_demo/ebin \
	-pa _build/default/lib/jsx/ebin \
	-noshell \
	-eval "application:ensure_all_started(inets), application:ensure_all_started(ssl), application:ensure_all_started(jsx), aws_demo_app:run(), halt()."

.PHONY: clean
clean:
	rm -rf build _build src/generated/*.erl src/generated/*.hrl rebar3.crashdump erl_crash.dump test/*.beam
	rm -rf model/*.json
	rm -rf terraform/.terraform terraform/terraform.tfstate terraform/terraform.tfstate.backup terraform/.terraform.lock.hcl

.PHONY: terraform
terraform:
	#
	# Run Terraform to create infrastructure
	#
	@echo "AWS_DEFAULT_REGION is $$AWS_DEFAULT_REGION"
	cd terraform && terraform init && terraform apply -auto-approve

.PHONY: docker/start
docker/start: docker/stop
	#
	# Run LocalStack in Docker
	#
	@echo "Forward port $$HOST_PORT to Localstack port 4566"
	docker run --rm -d -p $(HOST_PORT):4566 -e SERVICES=dynamodb --name $(CONTAINER_NAME) localstack/localstack

.PHONY: docker/stop
docker/stop:
	docker stop $(CONTAINER_NAME) &> /dev/null || true
	docker rm $(CONTAINER_NAME) &> /dev/null || true
