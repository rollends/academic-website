FROM ubuntu:noble

RUN apt-get update &&\
    apt-get install -y build-essential git haskell-stack locales pkg-config zlib1g zlib1g-dev &&\
    rm -rf /var/lib/apt/lists/* &&\
    localedef -i en_CA -c -f UTF-8 -A /usr/share/locale/locale.alias en_CA.UTF-8 &&\
    apt-get clean

ENV LANG en_CA.utf8
