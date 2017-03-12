FROM haskell:8

RUN mkdir -p /app
RUN mkdir -p /wiki
COPY ./example-users /wiki/users
WORKDIR /app

COPY ./gitwiki.cabal /app/gitwiki.cabal
COPY ./stack.yaml /app/stack.yaml
RUN stack install --only-dependencies

COPY . /app
RUN stack install

ENV PATH /root/.local/bin:$PATH

EXPOSE 3000

CMD ["gitwiki", "/wiki"]
