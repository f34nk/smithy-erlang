
X:=$(shell find output -type d -not -name output -maxdepth 1 -exec basename {} \;)
SDKS:=$(foreach x,$(X),output/$(x)/)

.PHONY: all
all: clean setup build

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
	./setup.sh

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
	smithy build 2>smithy-build.log
	[ -s smithy-build.log ] || rm -rf smithy-build.log
	grep SUCCESS output/s3/*.log || false

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
