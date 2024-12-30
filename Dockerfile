FROM rocker/verse:4.4.0
RUN apt-get update && apt-get install -y   && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.2.1")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.26")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.46")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.1.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1.1")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.3.0")'
RUN Rscript -e 'remotes::install_version("writexl",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.2")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.5.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.33")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.15.4")'
RUN Rscript -e 'remotes::install_github("ggrlab/restrictedROC@85541606de75c118e3793e80f6b224e9d5ecd0f7")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 9207
# CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(golem.rROC);golem.rROC::run_app()"
CMD R -e "golem.rROC::run_app(options = list(port = 9207, host='0.0.0.0'))"
# CMD R -e "options('shiny.port'=8080,shiny.host='0.0.0.0');shiny::runGitHub('shiny-examples', 'rstudio', subdir = '001-hello');"
