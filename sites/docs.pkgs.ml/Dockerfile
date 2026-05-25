FROM docker.io/cloudflare/sandbox:0.8.4 AS sandbox-base

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV HOME=/root
ENV PATH=/root/.riot/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    git \
    libpcre2-dev \
    libssl-dev \
    libzstd-dev \
    pkg-config \
    tar \
    uuid-dev \
    xz-utils \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

COPY --from=sandbox-base /container-server/sandbox /sandbox

RUN curl -sSL https://get.riot.ml | sh - \
    && /root/.riot/bin/riot version \
    && mkdir -p /tmp/riot-toolchain-workspace \
    && printf "[workspace]\nmembers = []\n" > /tmp/riot-toolchain-workspace/riot.toml \
    && printf "[toolchain]\nversion = \"5.5.0-riot.3\"\ntargets = [\"x86_64-unknown-linux-gnu\"]\n" > /tmp/riot-toolchain-workspace/ocaml-toolchain.toml \
    && cd /tmp/riot-toolchain-workspace \
    && /root/.riot/bin/riot toolchain install \
    && rm -rf /tmp/riot-toolchain-workspace

RUN mkdir -p /workspace

WORKDIR /workspace

EXPOSE 3000

ENTRYPOINT ["/sandbox"]
