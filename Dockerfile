FROM rocker/tidyverse:4.2.2
WORKDIR /home/rstudio
USER rstudio
RUN R -e "install.packages('renv', version='0.16.0')"
COPY renv.lock renv.lock
RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.dcf renv/settings.dcf
RUN R -e "renv::restore()"
USER root
EXPOSE 8787
