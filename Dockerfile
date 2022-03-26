FROM public.ecr.aws/lambda/provided

ARG AWS_ACCESS_KEY_ID
ARG AWS_SECRET_ACCESS_KEY
ARG AWS_DEFAULT_REGION

ENV AWS_ACCESS_KEY_ID ${AWS_ACCESS_KEY_ID}
ENV AWS_SECRET_ACCESS_KEY ${AWS_SECRET_ACCESS_KEY}
ENV AWS_DEFAULT_REGION ${AWS_DEFAULT_REGION}

RUN echo $AWS_ACCESS_KEY_ID
RUN echo $AWS_SECRET_ACCESS_KEY
RUN echo $AWS_DEFAULT_REGION

ENV R_VERSION=4.0.3

RUN yum -y install wget

RUN yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm \
&& wget https://cdn.rstudio.com/r/centos-7/pkgs/R-${R_VERSION}-1-1.x86_64.rpm \
&& yum -y install R-${R_VERSION}-1-1.x86_64.rpm \
&& rm R-${R_VERSION}-1-1.x86_64.rpm

ENV PATH="${PATH}:/opt/R/${R_VERSION}/bin/"

# System requirements for R packages
RUN yum -y install openssl-devel libxml2-devel
       
#RUN yum install -y wget java-1.8.0-openjdk java-1.8.0-openjdk-devel zip unzip
#RUN echo "JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")" | tee -a /etc/profile && source /etc/profile && echo $JAVA_HOME

#RUN export LD_LIBRARY_PATH=/usr/lib/jvm/jre/lib/amd64/server
#RUN R CMD javareconf

# Find a way to list the packages better
RUN Rscript -e "install.packages(c('httr', 'jsonlite', 'logger', 'readxl', 'aws.s3', 'dplyr', 'tidyr', 'stringr', 'purrr', 'lubridate', 'DBI', 'openxlsx'), repos = 'https://cloud.r-project.org/')"

COPY *.R ${LAMBDA_TASK_ROOT}/
RUN mkdir ${LAMBDA_TASK_ROOT}/generated/
RUN chmod 755 -R ${LAMBDA_TASK_ROOT}/
  
RUN printf '#!/bin/sh\ncd $LAMBDA_TASK_ROOT\nRscript runtime.R' > /var/runtime/bootstrap && chmod +x /var/runtime/bootstrap