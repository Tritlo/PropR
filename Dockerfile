FROM haskell:8.10

WORKDIR /app

COPY ./src /app/src
COPY ./tests /app/tests
COPY ./Endemic.cabal /app/
COPY ./entrypoint.sh /app/
RUN chmod +x /app/entrypoint.sh

#TODO: Can we freeze the cabal update here somehow? 
RUN cabal update
RUN cabal install --lib QuickCheck tasty tasty-hunit
# After Commit acd14d5e7e1cd3d88f072dcda40f0a00ae605daa
#RUN cabal install check-helpers

RUN cabal build

ENV LOG_LEVEL=INFO
ENV REPAIR_TARGET=/input
ENV CONFIG_FILE="/config/config.json"
ENV LOG_FILE="/output/docker-endemic.log"

# We create the default here, but if you mount it with docker compose
# It will be created if not existing.
RUN mkdir ${REPAIR_TARGET}


ENTRYPOINT ["/app/entrypoint.sh"]