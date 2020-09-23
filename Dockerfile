FROM rocker/tidyverse:latest

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/covidestim/webworker" \
      org.label-schema.vendor="Covidestim" \
      maintainer="Marcus Russi <marcus.russi@yale.edu>"

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  openssh-client

# All future commands are run as 'rstudio' user
USER rstudio

ENV PATH /home/rstudio/scripts:$PATH

# Install MsgPack
RUN install2.r --error --deps TRUE RcppMsgPack

# Copy over the GitHub repo
COPY --chown=rstudio . /home/rstudio

CMD ["R"]
