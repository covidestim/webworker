FROM rocker/tidyverse:latest

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/covidestim/webworker" \
      org.label-schema.vendor="Covidestim" \
      maintainer="Marcus Russi <marcus.russi@yale.edu>"

ENV PATH /opt/webworker/scripts:$PATH

# Install MsgPack
RUN install2.r --error --deps TRUE RcppMsgPack

# Copy over the GitHub repo
COPY . /opt/webworker

RUN chmod a+rx /opt/webworker/scripts/*
