# Use Alpine Linux as base image
FROM alpine:latest

# Install libpq and gmp dependencies (dynamic libraries required by the project)
RUN apk update && apk add libpq gmp

# Copy the prebuilt binary from stack-work into the container
COPY .stack-work/docker/_home/.local/bin/pauvre /usr/local/bin/pauvre

# Run the binary on container start
CMD ["pauvre"]