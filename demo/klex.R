# for additional information on a function type: ?function_name

# ensure required packages are loaded
library(ggplot2)
library(klexdatr)
library(lexr)

# set theme and font size
theme_set(theme_bw(base_size = 10))

# input, check and process data
klex <- input_lex_data("klexdatr")
check_lex_data(klex)
kdetect <- make_detect_data(klex, hourly_interval = 6L, end_date = as.Date("2014-12-31"))
summary(kdetect)

kanalysis <- make_analysis_data(kdetect, interval_period = make_difftime(60 * 60 * 24 * 7 * 4))

plot_data_dir(klex, all = TRUE)
plot_data_dir(kdetect, all = TRUE)
plot_data_dir(kanalysis, all = TRUE)
