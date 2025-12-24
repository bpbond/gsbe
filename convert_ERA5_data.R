# Convert the ERA5 grib files into data frames ready for model prediction
# BBL December 2025

library(readr)
nms <- read_csv("ERA5_short_names.csv", col_types = "cc")

library(tibble)
library(terra)
library(arrow)
library(dplyr)
library(tidyr)

# ========= ERA5 data

message("Transforming ERA5 grib data to data frames for model prediction...")
for(yr in 1985:2024) {
    message("\t", yr)
    x <- rast(paste0("data-ERA5/ERA5_", yr, ".grib"))
    y <- as.matrix(x)

    # This is returned as 6483600 (=grid cells) rows * 84 (=7 variables * 12 months) columns
    # (but we are resampling to 0.25 degrees, same as SPEI data = 1038240)
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

# ========= Soil and vegetation ERA5 data

lc <- rast("data-ERA5/ERA5_soil_type.grib") %>% as_tibble()
names(lc)[1] <- "Soil_type_number"
write_parquet(lc, "data-ERA5/ERA5_soil_type.parquet")


# ========= SPEI data

message("Assembling SPEI data...")

ERA5_raster <- rast("data-ERA5/ERA5_1985.grib")

load_spei <- function(window, year) {
    message("\tLoad ", window, " ", year)
    files <- list.files("data-spei/",
                        pattern = paste0("SPEI", window, ".*", year, "[0-9]{2}.nc$"),
                        full.names = TRUE)
    stopifnot(length(files) == 12)

    suppressWarnings({
        dat <- lapply(files, function(f) {
            x <- terra::rast(f)
            # Resample to the ERA5 0.1x0.1 grid
            as.matrix(resample(x, ERA5_raster))
        })
    })
    suppressMessages({
        dat <- dplyr::bind_cols(dat, .name_repair = "unique")
    })
    out <- tibble(SPEI = rowMeans(dat))
    out$SPEI[out$SPEI == -9999] <- NaN
    names(out)[1] <- paste0("SPEI", window)
    out
}

# Need these for the first loop iteration
spei12_lastyear <- load_spei(12, 1984) %>% rename(SPEI12_y1 = SPEI12)
spei24_lastyear <- load_spei(24, 1984) %>% rename(SPEI24_y1 = SPEI24)

for(yr in 1985:2024) {
    message(yr)
    spei12 <- load_spei(12, yr)

    out <- bind_cols(spei12, spei12_lastyear, spei24_lastyear)
    arrow::write_parquet(out, paste0("data-spei/spei", yr, ".parquet"))

    spei12_lastyear <- spei12 %>% rename(SPEI12_y1 = SPEI12)
    spei24_lastyear <- load_spei(24, yr) %>% rename(SPEI24_y1 = SPEI24)
}

message("All done!")
