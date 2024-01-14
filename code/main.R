
library(chirps)
library(terra)
library(lubridate)
library(data.table)
library(ggplot2)

# utility function to extract the last day for a given month and year
get_last_day_of_month <- function(yy, mm) {
  ym <- paste(yy, mm, 1, sep = "-")
  as.character(ceiling_date(ymd(ym), "month") - days(1))
}

# function to fetch chirps data for a given month and year
chirps_fetch_for_month <- function(yy, mm, sv) {
  message(sprintf("getting data for year: %s, month %s...", yy, mm))

  # get month range
  from <- as.Date(paste(yy, mm, 1, sep = "-"))
  to <- get_last_day_of_month(yy, mm)

  # fetch chirps data
  get_chirps(sv, dates = c(from, to), server = "CHC", resolution = 0.05)
}


# function to fetch chirps data for a specific month across multiple years
# return: a list of SpatRaster objects of lenght equal to the number of years
chirps_fetch_month_over_years <- function(yy_range, mm, sv) {
  .fetch_month <- function(yy, mm, sv) {
    chirps_fetch_for_month(yy, mm = mm, sv = sv)
  }

  lapply(yy_range, .fetch_month, mm = mm, sv = sv)
}


# function to compute number of rainy days for a list of spatial rasters
# return:  list of SpatRaster objects containing sums of rainy days
compute_nb_of_rainy_days <- function(daily_mm_of_rain) {
  lapply(daily_mm_of_rain, function(xx) sum(xx > 0))
}


# function to compute the average number of rainy days for a given
# month across multiple years; for a specific area of interest
get_avg_rainy_days_per_month <- function(mm, yy_range, sv) {
  ### fetch chirps data for a given month over years
  daily_mm_of_rain <- chirps_fetch_month_over_years(yy_range, mm, sv)

  ### compute rounded average number of rainy days
  # sum up rainy days
  nb_rainy_days <- compute_nb_of_rainy_days(daily_mm_of_rain)
  # flatten list
  nb_rainy_days <- do.call(c, nb_rainy_days)
  # get avg
  avg_rainy_days <- round(mean(nb_rainy_days, na.rm = TRUE))

  ### apply mask
  mask(avg_rainy_days, sv)
}



###

yy_range <- 2020:2022

# build spacial vector from shape file
sv <- vect("data/aoi.shp")

# compute avg rainy days for month and aoi, across years
avg_rainy_days <- lapply(1:12,
  get_avg_rainy_days_per_month, yy_range = yy_range, sv = sv
)
avg_rainy_days <- do.call(c, avg_rainy_days) # aggregate

# avg_rainy_days <- terra::wrap(avg_rainy_days)
# saveRDS(avg_rainy_days, file = "avg_rainy_days.rds")
# avg_rainy_days <- readRDS("avg_rainy_days.rds")
# avg_rainy_days <- terra::unwrap(avg_rainy_days)

# example01 <- terra::wrap(example01)
# saveRDS(example01, file = "example01.rds")
# example01 <- readRDS("example01.rds")
# example01 <- terra::unwrap(example01)

# prepare data for plotting
names(avg_rainy_days) <- month.name[1:dim(avg_rainy_days)[3]]
df <- as.data.frame(avg_rainy_days, xy = TRUE)
df <- melt(as.data.table(df), id.vars = c("x", "y"))
limits <- c(min(df$value), max(df$value))

ggplot(data = df, aes(x = x, y = y, fill = value)) +
  geom_raster() + 
  scale_fill_gradient(low="white", high = "red", limits=limits) +
  coord_quickmap() + 
  theme_minimal () +
  labs(title = "Average number of rainy days", fill = "avg days") +
  xlab("latitude") +
  ylab("longitude") +
  facet_wrap(~variable, ncol=3)


### generate report
library("rmarkdown")
render("code/report.rmd")
