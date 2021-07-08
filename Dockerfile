FROM haskell:8.10

WORKDIR /app

COPY ./src /app/src
COPY ./tests /app/tests
COPY ./Endemic.cabal /app/
COPY ./entrypoint.sh /app/
RUN chmod +x /app/entrypoint.sh

#TODO: Can we freeze the cabal update here? 
RUN cabal update
RUN cabal install --lib QuickCheck 

RUN cabal build

ENV LOG_LEVEL=INFO
ENV REPAIR_TARGET=/input

# We create the default here, but if you mount it with docker compose
# It will be created if not existing.
RUN mkdir ${REPAIR_TARGET}


ENTRYPOINT ["/app/entrypoint.sh"]