FROM rocker/tidyverse:latest

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/covidestim/webworker" \
      org.label-schema.vendor="Covidestim" \
      maintainer="Marcus Russi <marcus.russi@yale.edu>"

ENV PATH /opt/webworker/scripts:$PATH

# Install MsgPack
RUN install2.r --error --deps TRUE RcppMsgPack

RUN apt-get update \
  && apt-get install -y --no-install-recommends postgresql-client \
  && echo 'deb http://archive.ubuntu.com/ubuntu groovy main restricted' >> /etc/apt/sources.list \
  && apt-get update \
  && apt-get install make \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  && rm -rf /var/lib/apt/lists/*

# Copy over the GitHub repo
COPY . /opt/webworker

# Make all scripts executable
RUN chmod a+rx /opt/webworker/scripts/*

# Initialize and update the `vaccineAdjust` R package, which is embedded in
# this repository as a submodule
RUN cd /opt/webworker && git submodule init && git submodule update

# Install vaccineAdjust
# RUN installGithub.r covidestim/vaccineAdjust
RUN cd /opt/webworker/ && R CMD INSTALL vaccineAdjust

