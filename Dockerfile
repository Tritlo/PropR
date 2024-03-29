ARG HASKELL_VERSION=8.10.7
ARG PROPR_VERSION=1.0.0
ARG RUN_TESTS
# ==========================================
# =         Stage 1: Build & Test          =
# ==========================================
FROM haskell:${HASKELL_VERSION} as builder

LABEL builder=true 
LABEL maintainer="L.H.Applis@tu-delft.nl"

WORKDIR /builder
COPY ./PropR.cabal /builder/
RUN cabal update
RUN cabal build --dependencies-only PropR

COPY ./src /builder/src
COPY ./tests /builder/tests

#TODO: Can we freeze the cabal update here somehow? 

WORKDIR /builder
RUN cabal build
# Run tests if arg has any value
RUN if [[ -n "${RUN_TESTS}"]] ; then cabal test; else echo "Skipping Tests"; fi

# ==========================================
# =       Stage 2: Runnable Container      =
# ==========================================
FROM haskell:${HASKELL_VERSION} as runnable

# Args need to be re-passed in a new Stage (See https://stackoverflow.com/questions/53681522/share-variable-in-multi-stage-dockerfile-arg-before-from-not-substituted)
ARG HASKELL_VERSION
ARG PROPR_VERSION

LABEL builder=false 
LABEL maintainer="L.H.Applis@tu-delft.nl"
LABEL name="tritlo/propr"
LABEL url="https://github.com/Tritlo/PropR"
LABEL vcs="https://github.com/Tritlo/PropR"

# Copy the Executable from Builder-Container
COPY --from=builder /builder/dist-newstyle/build/x86_64-linux/ghc-${HASKELL_VERSION}/PropR-${PROPR_VERSION}/x/propr/build/propr /app/
RUN chmod +x /app/propr

# Install the Helpers
RUN cabal update
COPY ./check-helpers /app/check-helpers
WORKDIR /app/check-helpers
RUN cabal install --lib check-helpers random QuickCheck

ENV LOG_LEVEL=INFO
ENV REPAIR_TARGET=/input
ENV CONFIG_FILE="/config/config.json"
ENV LOG_FILE="/output/docker-propr.log"

# We create the default here, but if you mount it with docker compose
# It will be created if not existing.
RUN mkdir ${REPAIR_TARGET}

# Copy the Entrypoint
COPY ./entrypoint.sh /app/
WORKDIR /app
RUN chmod +x /app/entrypoint.sh

ENTRYPOINT ["/app/entrypoint.sh"]
