# Webworker

This is a container for doing various specialized operations on AWS Batch and YCRC. Specifically, this container performs:

- Reformatting of certain model outputs as per-capita estimates
- Serialization of model output as gzipped WebPack for use on the website
- Gzipping model summary files (`summary.csv`) for distribution through AWS S3

It is based on the `rocker/tidyverse` Dockern image with a single additional
dependency on `RcppMsgPack`. The Docker image is converted to a Singularity 
`.sif` file when run on YCRC. This image is auto-built on Docker Hub off of
`master`.
