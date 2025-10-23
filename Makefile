.PHONY: all clean build test

all: clean build test

build:
	#
	# Build and publish the generator
	#
	./gradlew clean build publishToMavenLocal
	tree build/libs
	tree ~/.m2/repository/io/smithy/erlang/smithy-erlang

test:
	#
	# Run the tests
	#
	./gradlew test --info

clean:
	#
	# Clear the build
	#
	rm -rf build bin
	rm -rf ~/.m2/repository/io/smithy/erlang/smithy-erlang
