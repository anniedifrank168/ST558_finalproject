# Use the official R base image
FROM r-base:latest

# Install system/package dependencies
RUN apt-get update && apt-get install -y \
libcurl4-openssl-dev \
libxml2-dev \
libssl-dev \
libpng-dev \
pandoc \
&& apt-get clean \
&& rm -rf /var/lib/apt/lists/*
    
ENV http_proxy=http://proxyaddress:port
ENV https_proxy=http://proxyaddress:port
# Install necessary R packages
RUN R -e "install.packages('tidymodels')"
RUN R -e "install.packages('rpart')"
RUN R -e "install.packages('rsample')"
RUN R -e "install.packages('yardstick')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('confusionMatrix')"
RUN R -e "install.packages('remotes')"
RUN R -e "install.packages('plumber')"

#RUN mkdir -p ~/app
WORKDIR /app

# Copy API script into the container
COPY API.R /app/API.R

# Copy dataset into the container (for model fitting)
COPY diabetes_binary_health_indicators_BRFSS2015.csv /app/diabetes_binary_health_indicators_BRFSS2015.csv

# Expose port for API will run
EXPOSE 8000

# Run the plumber API
#ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('API.R'); pr$run(host = '0.0.0.0', port = 8000)"]
ENTRYPOINT ["R", "-e", "print(iris)"]