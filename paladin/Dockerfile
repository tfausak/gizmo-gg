FROM debian:8.7

# Install dependencies.
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install --assume-yes curl libpq-dev

# Install Stack.
RUN curl --location https://get.haskellstack.org | sh

# Install GHC.
WORKDIR /project
COPY stack.yaml /project
RUN stack setup

# Install dependencies.
COPY package.yaml /project
RUN stack build --only-dependencies

# Build project.
COPY . /project
RUN stack build --copy-bins --local-bin-path /usr/local/bin

# Run project.
EXPOSE 8080
CMD /usr/local/bin/paladin
