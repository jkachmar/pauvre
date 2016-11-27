# pauvre

An example Servant application using Persistant as the database driver and
Postgres as the backend data store.

For maximum portability, the project can be built into a minimal Docker image
(~30 MB at the time of writing) using the steps below.

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
