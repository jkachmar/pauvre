# pauvre

A small playground for trying to minimize container sizes for practical 
Haskell projects. A Servant API using Persistant as the driver for a Postgres
database was arbitrarily chosen as a sufficiently complex project to test.

## Development

To avoid entering the build container during development, ensure all `stack` 
invocations begin with `stack --no-docker`.

## Build for Deployment

Images are tagged with `-t <tag>` for clarity of the example, you should tag
them in the most helpful manner to your deployment.

To create the Alpine build environment: `docker build -f BaseImage -t alpine-ghc/base .`

To compile the binary: `stack --docker-image=alpine-ghc/base install`

To build the container: `docker build -t pauvre .`

To run the container locally: `docker run -i -t -p 8081:8081 pauvre`
