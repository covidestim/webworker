FROM rocker/tidyverse:latest

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/covidestim/webworker" \
      org.label-schema.vendor="Covidestim" \
      maintainer="Marcus Russi <marcus.russi@yale.edu>"

# All future commands are run as 'rstudio' user
USER rstudio

# Copy over the GitHub repo
COPY --chown=rstudio . /home/rstudio

# Now install MsgPack
RUN install2.r --error \
    --deps TRUE \
    RcppMsgPack \

CMD ["R"]
