FROM rocker/tidyverse:4.2.2
WORKDIR /home/rstudio
RUN apt update && apt install -y libv8-dev cmake
RUN R -e "install.packages('renv', version='0.16.0')"

USER 1000:1000
ENV RENV_VERSION 0.16.0
# ENV RENV_PATHS_LIBRARY /home/rstudio/renv/library
COPY renv.lock _renv.lock
RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.dcf renv/settings.dcf
RUN R -e "renv::restore(lockfile = '_renv.lock')"
USER root
EXPOSE 8787
