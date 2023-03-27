FROM rocker/geospatial

ARG USER_UID="1000"
ARG USER_GID="1000"
ARG QUARTO_VERSION=latest


RUN install2.r --error --skipinstalled \
	plotly \
    shiny \
    leaflet \
    leaflet.extras \
    htmltools\
    stringr \
    classInt 

RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends software-properties-common curl wget libssl-dev libxml2-dev libsodium-dev imagemagick libmagick++-dev libgit2-dev libssh2-1-dev zlib1g-dev librsvg2-dev libudunits2-dev libcurl4-openssl-dev python3-pip pandoc libzip-dev libfreetype6-dev libfontconfig1-dev tk libpq5 libxt6 openssh-client openssh-server \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/*

RUN apt-get update && \
    apt-get install -y zsh curl git && \
    curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh -o install.sh && \
    sh install.sh --unattended && \
    rm install.sh && \
    chsh -s /bin/zsh

CMD ["/bin/zsh"]
# install R packages needed for VSCode interaction and package management
RUN install2.r --error --skipinstalled --ncpus -4 languageserver renv remotes httpgd

# install radian via python and pip3
RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends python3-setuptools

# RUN pip3 install radian

# [Optional] Uncomment this section to add Quarto to the container
RUN /rocker_scripts/install_quarto.sh

# USER $USERNAME

