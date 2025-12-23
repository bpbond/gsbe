# Convert the ERA5 grib files into data frames ready for model prediction
# BBL December 2025

library(readr)
nms <- read_csv("ERA5_short_names.csv", col_types = "cc")

library(tibble)
library(terra)
library(arrow)

message("Reading ERA5 grib data and transforming to data frames ready for model prediction...")
for(yr in 1985:2024) {
    message(yr)
    x <- rast(paste0("data-ERA5/ERA5_", yr, ".grib"))
    y <- as.matrix(x)

    # This is returned as 6483600 (=grid cells) rows * 84 (=7 variables * 12 months) columns
    nvars <- ncol(y) / 12
    stopifnot(nvars %% 1 == 0) # should be cleanly divisible by 12

    # Compute annual means
    out <- tibble(.rows = nrow(y))
    for(i in seq_len(nvars)) {
        out[colnames(y)[i]] <- rowMeans(y[,icols <- i + 0:11 * nvars])
    }

    # Use sensible short names that match analysis data
    colnames(out) <- nms$short_name

    # This is same for every grid -- don't save with the climate data
    # out$area <- as.matrix(cellSize(x))[,1]

    write_parquet(out, paste0("data-ERA5/ERA5_", yr, ".parquet")) # 66 MB
}

message("All done!")
