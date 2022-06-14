FROM rocker/tidyverse:4.1.3

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/covidestim/webworker" \
      org.label-schema.vendor="Covidestim" \
      maintainer="Marcus Russi <marcus.russi@yale.edu>"

ENV PATH /opt/webworker/scripts:$PATH

# Install MsgPack and sf
RUN install2.r --error --deps TRUE RcppMsgPack sf

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    postgresql-client \
    jq \
    libudunits2-dev \
    libgeos-dev \
    libproj-dev \
    libgdal-dev \
    git-lfs \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  && rm -rf /var/lib/apt/lists/* \
  && wget -c "http://archive.ubuntu.com/ubuntu/pool/main/m/make-dfsg/make_4.3-4ubuntu1_amd64.deb" \
  && sudo apt-get install ./make_4.3-4ubuntu1_amd64.deb \
  && rm ./make_4.3-4ubuntu1_amd64.deb

# Copy over the GitHub repo
COPY . /opt/webworker

# Make all scripts executable
RUN chmod a+rx /opt/webworker/scripts/*

# Initialize and update the `vaccineAdjust` R package, which is embedded in
# this repository as a submodule
RUN cd /opt/webworker && git submodule init && git submodule update

# Install vaccineAdjust
# RUN installGithub.r covidestim/vaccineAdjust
RUN Rscript -e "devtools::install('/opt/webworker/vaccineAdjust/')"

