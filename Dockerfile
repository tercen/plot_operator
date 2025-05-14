FROM tercen/runtime-r44:4.4.3-7

COPY . /operator
WORKDIR /operator

RUN R -e "renv::consent(provided = TRUE); renv::restore(confirm = FALSE)"

ENV TERCEN_SERVICE_URI https://tercen.com
ENV OPENBLAS_NUM_THREADS=1

ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
CMD ["--taskId", "someid", "--serviceUri", "https://tercen.com", "--token", "sometoken"]
