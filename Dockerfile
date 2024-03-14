# syntax=docker/dockerfile:1

FROM haskell:9.6.4 AS builder
WORKDIR /app
COPY . /app
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libtinfo-dev && \
    rm -rf /var/lib/apt/lists/*
RUN stack setup
RUN stack build --only-dependencies
RUN stack build
FROM debian:buster-slim
WORKDIR /app
COPY --from=builder /app/.stack-work/install/x86_64-linux/*/9.6.4/bin/University-Helper-exe /app/
COPY --from=builder /usr/local/bin/stack /usr/local/bin/stack
COPY sql /docker-entrypoint-initdb.d/sql
EXPOSE 5432
CMD ["stack", "run"]
