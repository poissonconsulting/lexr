# ensure required packages are loaded
library(lubridate)
library(dplyr)
library(magrittr)
library(ggplot2)
library(klexdatr)
library(lexr)

# for additional information on a function type: ?function_name

# set plot theme and font size
theme_set(theme_bw(base_size = 10))

# input hourly data from klexdatr package
klex <- input_lex_data("klexdatr")

# select only those capture with a fork length greater than or equal to 500 mm
capture <- filter(klex$capture, Length >= 500)

# combine hourly data into 6 hour intervals
kdetect <- make_detect_data(klex, capture = capture, start_date = as.Date("2008-04-01"),
                            end_date = as.Date("2013-12-31"), hourly_interval = 6L)

# plot Kootenay Lake by color-coded section
plot_detect_section(kdetect)

# plot percent receiver coverage by color-coded section
plot_detect_coverage(kdetect)

# plot detections by fish, species, date and color-coded section.
plot_detect_overview(kdetect)

# select only Rainbow Trout
capture <-  filter(kdetect$capture, Species == "Rainbow Trout")

# group six hour intervals into monthly periods
interval_period <- mutate(kdetect$interval, Month = month(Month, label = TRUE),
                          Period = paste(Year, Month))$Period
interval_period %<>% factor(levels = unique(.))

# create monthly data ready for analysis
kanalysis <- make_analysis_data(
  kdetect, capture = capture, interval_period = interval_period,
  growth = growth_vb, linf = 1000, k = 0.19
)

# save all plots to folders in the working directory
if (FALSE) { # FALSE if so not run automatically
  plot_data_dir(klex, all = TRUE)
  plot_data_dir(kdetect, all = TRUE)
  plot_data_dir(kanalysis, all = TRUE)
}
