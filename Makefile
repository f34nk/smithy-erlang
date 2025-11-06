X:=$(shell find examples -type d -not -name examples -maxdepth 1 -exec basename {} \;)
EXAMPLES:=$(foreach x,$(X),examples/$(x)/)

.PHONY: all
all: clean build test

.PHONY: build
build:
	#
	# Build and publish the generator
	#
	rm -rf build-errors.log
	./gradlew clean build publishToMavenLocal 2>build-errors.log
	[ -s build-errors.log ] || rm -rf build-errors.log
	tree build/libs
	tree ~/.m2/repository/io/smithy/erlang/smithy-erlang

.PHONY: test
test:
	#
	# Run the tests
	#
	rm -rf test-errors.log
	./gradlew test --info 2>test-errors.log
	[ -s test-errors.log ] || rm -rf test-errors.log

.PHONY: clean
clean:
	#
	# Clear the build
	#
	rm -rf build bin test-errors.log build-errors.log
	rm -rf ~/.m2/repository/io/smithy/erlang/smithy-erlang

# Usage: make examples
.PHONY: examples
examples:
	#
	# Build $(EXAMPLES)
	#
	@for x in $(EXAMPLES); do \
		example=`echo $$x|sed 's/\/$$//g'` ; \
		make $$example ; \
	done

# Usage: make examples/user-service
.PHONY: $(EXAMPLES)
examples/%: $(EXAMPLES)
	#
	# Build $@
	#
	cd $@ && make

# Usage: make examples/clean
examples/clean:
	#
	# Build $(EXAMPLES)
	#
	@for x in $(EXAMPLES); do \
		cd $$x ; \
		make clean ; \
		cd - ; \
	done
