# Convert the ERA5 grib files into data frames ready for model prediction
# BBL December 2025

library(readr)
nms <- read_csv("ERA5_short_names.csv", col_types = "cc")

library(tibble)
library(terra)
yr <- 1985
x <- rast(paste0("data-ERA5/ERA5_", yr, ".grib"))
y <- as.matrix(x)

# This is returned as 6483600 (=grid cells) rows * 84 (=7 variables * 12 months) columns
nvars <- ncol(y) / 12
stopifnot(nvars %% 1 == 0) # should be cleanly divisible by 12

out <- tibble(.rows = nrow(y))
for(i in seq_len(nvars)) {
    out[colnames(y)[i]] <- rowMeans(y[,icols <- i + 0:11 * nvars])
}

colnames(out) <- nms$short_name

out$area <- as.matrix(cellSize(x))[,1]

library(arrow)
write_parquet(out, paste0("out_", yr, ".parquet")) # 66 MB
#write_csv(out, "out.csv") # 482 MB
