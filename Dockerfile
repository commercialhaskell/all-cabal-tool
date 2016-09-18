FROM fpco/stack-run:lts-6

COPY run.sh /usr/local/bin/run.sh
COPY run-inner.sh /usr/local/bin/run-inner.sh

RUN curl https://s3.amazonaws.com/stackage-travis/hackage-mirror/hackage-watcher.bz2 | bunzip2 > /usr/local/bin/hackage-watcher && \
    curl https://s3.amazonaws.com/stackage-travis/all-cabal-hashes-tool/all-cabal-hashes-tool.bz2 | bunzip2 > /usr/local/bin/all-cabal-hashes-tool && \
    curl https://s3.amazonaws.com/stackage-travis/all-cabal-hashes-tool/all-cabal-hashes-tool.bz2 | bunzip2 > /usr/local/bin/all-cabal-hashes-tool && \
    curl https://s3.amazonaws.com/stackage-travis/all-cabal-metadata-tool/all-cabal-metadata-tool.bz2 | bunzip2 > /usr/local/bin/all-cabal-metadata-tool && \
    chmod +x \
        /usr/local/bin/run.sh \
        /usr/local/bin/run-inner.sh
        /usr/local/bin/all-cabal-metadata-tool \
        /usr/local/bin/all-cabal-hashes-tool \
        /usr/local/bin/hackage-watcher \
        /usr/local/bin/all-cabal-hashes-tool
