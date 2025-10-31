.PHONY: all clean build test

all: clean build test

build:
	#
	# Build and publish the generator
	#
	rm -rf build-errors.log
	./gradlew clean build publishToMavenLocal 2>build-errors.log
	[ -s build-errors.log ] || rm -rf build-errors.log
	tree build/libs
	tree ~/.m2/repository/io/smithy/erlang/smithy-erlang

test:
	#
	# Run the tests
	#
	rm -rf test-errors.log
	./gradlew test --info 2>test-errors.log
	[ -s test-errors.log ] || rm -rf test-errors.log

clean:
	#
	# Clear the build
	#
	rm -rf build bin test-errors.log build-errors.log
	rm -rf ~/.m2/repository/io/smithy/erlang/smithy-erlang
