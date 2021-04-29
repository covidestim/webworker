FROM rocker/tidyverse:latest

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/covidestim/webworker" \
      org.label-schema.vendor="Covidestim" \
      maintainer="Marcus Russi <marcus.russi@yale.edu>"

ENV PATH /opt/webworker/scripts:$PATH

# Install MsgPack
RUN install2.r --error --deps TRUE RcppMsgPack

# Install all specialty packages needed for the data cleaning process
RUN install2.r --error --deps TRUE sf splines imputeTS tempdisagg

# Copy over the GitHub repo
COPY . /opt/webworker

RUN chmod a+rx /opt/webworker/scripts/*

RUN apt-get update \
  && apt-get install -y --no-install-recommends postgresql-client \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  && rm -rf /var/lib/apt/lists/*
