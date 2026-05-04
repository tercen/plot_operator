FROM tercen/runtime-r44-minimal-plot:4.4.3-1 AS builder

# Operator-specific CRAN packages installed on top of the
# tercen/teRcenHttp/mtercen/teRcenApi/teRcen + Cairo stack baked
# into the base image.
RUN installr -d ggplot2 dplyr svglite jsonlite scales tidyr zip

FROM tercen/runtime-r44-minimal-plot:4.4.3-1

COPY --from=builder /usr/local/lib/R/library /usr/local/lib/R/library
COPY main.R utils.R utils_colors.R palettes.json /operator/
WORKDIR /operator

ENV TERCEN_SERVICE_URI=https://tercen.com
ENV OPENBLAS_NUM_THREADS=1

ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
CMD ["--taskId", "someid", "--serviceUri", "https://tercen.com", "--token", "sometoken"]
