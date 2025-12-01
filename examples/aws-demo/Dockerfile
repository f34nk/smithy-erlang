FROM erlang:27.0

# Avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    curl \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install rebar3
RUN curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3

# Set up library path for lexbor
ENV LD_LIBRARY_PATH=/usr/local/lib

# Create working directory
WORKDIR /app

# Copy project files
COPY src/ /app/src/
COPY rebar.config /app/rebar.config
COPY rebar.lock /app/rebar.lock

# Compile the application
RUN rebar3 compile

# Verify s3_client_app.beam was compiled
RUN ls -la _build/default/lib/aws_demo/ebin/ && \
    test -f _build/default/lib/aws_demo/ebin/aws_demo_app.beam || (echo "ERROR: aws_demo_app.beam not found!" && exit 1)

# Run the S3 client application with all dependencies started
CMD ["erl", \
     "-pa", "_build/default/lib/aws_demo/ebin", \
     "-pa", "_build/default/lib/jsx/ebin", \
     "-noshell", \
     "-eval", "application:ensure_all_started(inets), application:ensure_all_started(ssl), application:ensure_all_started(jsx), aws_demo_app:run(), halt()."]
