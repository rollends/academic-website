FROM ubuntu:noble

RUN chsh -s /bin/bash
SHELL ["/bin/bash", "-c"]

RUN apt-get update &&\
    apt-get install -y build-essential curl git libffi-dev libffi8 libgmp10 libgmp-dev libncurses-dev locales pkg-config rsync zlib1g zlib1g-dev &&\
    rm -rf /var/lib/apt/lists/* &&\
    localedef -i en_CA -c -f UTF-8 -A /usr/share/locale/locale.alias en_CA.UTF-8 &&\
    apt-get clean

ENV LANG=en_CA.utf8    
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_VERBOSE=1
ENV BOOTSTRAP_HASKELL_ADJUST_BASHRC=1
ENV BOOTSTRAP_HASKELL_INSTALL_HLS=1
ENV BOOTSTRAP_HASKELL_GHC_VERSION="9.8.4"
ENV BOOTSTRAP_HASKELL_STACK_VERSION="recommended"
ENV BOOTSTRAP_HASKELL_HLS_VERSION="latest"

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN echo "source /root/.ghcup/env" >> /root/.bashrc

# SSH Configuration.
RUN mkdir -p /root/.ssh/
RUN chmod 700 /root/.ssh/
COPY ssh_config.private deploykey.private /root/.ssh/
RUN mv /root/.ssh/ssh_config.private /root/.ssh/config