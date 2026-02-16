FROM ubuntu:noble

RUN chsh -s /bin/bash
SHELL ["/bin/bash", "-c"]

RUN apt-get update &&\
    apt-get install -y build-essential curl git libgmp10 libgmp-dev locales pkg-config rsync zlib1g zlib1g-dev &&\
    rm -rf /var/lib/apt/lists/* &&\
    localedef -i en_CA -c -f UTF-8 -A /usr/share/locale/locale.alias en_CA.UTF-8 &&\
    apt-get clean

ENV LANG en_CA.utf8

RUN \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_VERBOSE=1 \
    BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN echo "PATH=/root/.ghcup/bin/:$PATH\n" >> /root/.bashrc
RUN /root/.ghcup/bin/ghcup install ghc 9.8.4 && /root/.ghcup/bin/ghcup set ghc 9.8.4