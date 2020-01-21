FROM debian:stretch
LABEL org.label-schema.vcs-url="https://sitfs1/DataAnalytics" \
    org.label-schema.vendor="Sandhills Global" \
    maintainer="Jordan Brown <jordan-brown@sandhills.com>"

## https://hub.docker.com/r/rocker/r-ver
ARG R_VERSION
ARG BUILD_DATE
ENV BUILD_DATE ${BUILD_DATE:-2019-03-11}
ENV R_VERSION=${R_VERSION:-3.5.2} \
    LC_ALL=en_US.UTF-8 \
    LANG=en_US.UTF-8 \
    TERM=xterm

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    bash-completion \
    ca-certificates \
    file \
    fonts-texgyre \
    g++ \
    gfortran \
    gsfonts \
    libblas-dev \
    libbz2-1.0 \
    libcurl3 \
    libicu57 \
    libjpeg62-turbo \
    libopenblas-dev \
    libpangocairo-1.0-0 \
    libpcre3 \
    libpng16-16 \
    libreadline7 \
    libtiff5 \
    liblzma5 \
    locales \
    make \
    unzip \
    zip \
    zlib1g \
    && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8 \
    && BUILDDEPS="curl \
    default-jdk \
    libbz2-dev \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libpango1.0-dev \
    libjpeg-dev \
    libicu-dev \
    libpcre3-dev \
    libpng-dev \
    libreadline-dev \
    libtiff5-dev \
    liblzma-dev \
    libx11-dev \
    libxt-dev \
    perl \
    tcl8.6-dev \
    tk8.6-dev \
    texinfo \
    texlive-extra-utils \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-recommended \
    x11proto-core-dev \
    xauth \
    xfonts-base \
    xvfb \
    zlib1g-dev" \
    && apt-get install -y --no-install-recommends $BUILDDEPS \
    && cd tmp/ \
    ## Download source code
    && curl -O https://cran.r-project.org/src/base/R-3/R-${R_VERSION}.tar.gz \
    ## Extract source code
    && tar -xf R-${R_VERSION}.tar.gz \
    && cd R-${R_VERSION} \
    ## Set compiler flags
    && R_PAPERSIZE=letter \
    R_BATCHSAVE="--no-save --no-restore" \
    R_BROWSER=xdg-open \
    PAGER=/usr/bin/pager \
    PERL=/usr/bin/perl \
    R_UNZIPCMD=/usr/bin/unzip \
    R_ZIPCMD=/usr/bin/zip \
    R_PRINTCMD=/usr/bin/lpr \
    LIBnn=lib \
    AWK=/usr/bin/awk \
    CFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g" \
    CXXFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g" \
    ## Configure options
    ./configure --enable-R-shlib \
    --enable-memory-profiling \
    --with-readline \
    --with-blas \
    --with-tcltk \
    --disable-nls \
    --with-recommended-packages \
    ## Build and install
    && make \
    && make install \
    ## Add a default CRAN mirror
    && echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site \
    ## Add a library directory (for user-installed packages)
    && mkdir -p /usr/local/lib/R/site-library \
    && chown root:staff /usr/local/lib/R/site-library \
    && chmod g+wx /usr/local/lib/R/site-library \
    ## Fix library path
    && echo "R_LIBS_USER='/usr/local/lib/R/site-library'" >> /usr/local/lib/R/etc/Renviron \
    && echo "R_LIBS=\${R_LIBS-'/usr/local/lib/R/site-library:/usr/local/lib/R/library:/usr/lib/R/library'}" >> /usr/local/lib/R/etc/Renviron \
    ## install packages from date-locked MRAN snapshot of CRAN
    && [ -z "$BUILD_DATE" ] && BUILD_DATE=$(TZ="America/Los_Angeles" date -I) || true \
    && MRAN=https://mran.microsoft.com/snapshot/${BUILD_DATE} \
    && echo MRAN=$MRAN >> /etc/environment \
    && export MRAN=$MRAN \
    && echo "options(repos = c(CRAN='$MRAN'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site \
    ## Use littler installation scripts
    && Rscript -e "install.packages(c('littler', 'docopt'), repo = '$MRAN')" \
    && ln -s /usr/local/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
    && ln -s /usr/local/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
    && ln -s /usr/local/lib/R/site-library/littler/bin/r /usr/local/bin/r 

    ## Any plumber dependencies
RUN apt-get update -qq \
    &&PLUMBERDEPS=" git-core \
    libssl-dev \
    libcurl4-gnutls-dev" \
    && apt-get install -y  $PLUMBERDEPS

    ## Clean up from R source install
RUN command cd / \
    && rm -rf /tmp/* \
    && apt-get remove --purge -y $BUILDDEPS \
    && apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/* \
    ## install plumber
    && install2.r plumber

    ## Install odbc dependencies
    RUN apt-get update \
        && apt-get install --yes --no-install-recommends \
                apt-transport-https \
                curl \
                gnupg \
                unixodbc-dev \
        && curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
        && curl https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list \
        && apt-get update \
        && ACCEPT_EULA=Y apt-get install --yes --no-install-recommends msodbcsql17 \
        && install2.r odbc \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/* \
        && rm -rf /tmp/*


##Need to determine if this is needed when working with HAPROXY
# EXPOSE 8000
# ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R'); pr$run(host='0.0.0.0', port=8000)"]
# CMD ["R"]