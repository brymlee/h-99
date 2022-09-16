FROM docker.io/haskell:slim-buster
RUN mkdir /root/.local/bin -p
RUN mkdir haskell-ninety-nine-problems
WORKDIR haskell-ninety-nine-problems
COPY stack.yaml stack.yaml
COPY haskell-ninety-nine-problems.cabal haskell-ninety-nine-problems.cabal 
RUN stack setup 9.0.2
RUN stack install --only-dependencies
COPY LICENSE LICENSE
COPY src src
RUN stack install
CMD ["/root/.local/bin/haskell-ninety-nine-problems"]
