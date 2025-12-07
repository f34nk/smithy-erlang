X:=$(shell find output -type d -not -name output -maxdepth 1 -exec basename {} \;)
SDKS:=$(foreach x,$(X),output/$(x)/)

.PHONY: all
all:
	@echo "Usage:"
	@echo "  make setup SDKS=s3,dynamodb,lambda"
	@echo "  make build"

.PHONY: clean
clean:
	#
	# Clear the output directory
	#
	rm -rf output

.PHONY: setup
setup:
	#
	# Setup projects
	#
	SDKS=$(SDKS) ./setup.sh

.PHONY: build
build: build/clean
	#
	# Build all: $(SDKS)
	#
	@for x in $(SDKS); do \
		sdk=`echo $$x|sed 's/\/$$//g'` ; \
		make $$sdk ; \
		if [ $$? -ne 0 ]; then \
			echo "Error building $$sdk" ; \
			exit 1 ; \
		fi ; \
		rm -rf $$x/build; \
		tree $$x ; \
	done

.PHONY: $(SDKS)
output/%: $(SDKS)
	#
	# Build $@
	#
	cd $@ && \
	rm -f smithy-build.log && \
	smithy build > smithy-build.log 2>&1
	[ -s smithy-build.log ] || rm -rf smithy-build.log
	grep SUCCESS $@/*.log || false


.PHONY: build/clean
build/clean:
	#
	# Clean all: $(SDKS)
	#
	@for x in $(SDKS); do \
		cd $$x ; \
		rm -f smithy-build.log; \
		rm -rf src/*; \
		rm -rf build; \
		cd - ; \
	done

.PHONY: list-sdks
list-sdks:
	#
	# List all SDK IDs
	#
	find api-models-aws-main/models -maxdepth 1 -type d|awk -F'/' '{print $$NF}'|sort > sdks.txt
	wc -l sdks.txt
