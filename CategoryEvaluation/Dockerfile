ARG R_VERSION
FROM productionevaluation:${R_VERSION}

## instead of this we could save this from an api call
COPY renv.lock renv.lock

ENV RENV_VERSION 0.9.2-24
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN R -e 'renv::restore()'

COPY predictionmodelquery.R predictionmodelquery.R

ARG CATEGORYID
ENV cat_id=${CATEGORYID}
ENV env="staging"
ENV product="fe"
ENV status="active"
ENV max_models=100

# RUN R -e 'source("predictionmodelquery.R")'
