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

# create directory for plots
dir.create("klex", showWarnings = FALSE)

# plot base data
setwd("klex")
png("klex%03d.png", width = 6, height = 6, units = "in", res = 300)
plot(klex, all = TRUE)
dev.off()
setwd("..")

# plot processed data
setwd("klex")
png("kdetect%03d.png", width = 6, height = 6, units = "in", res = 300)
plot(kdetect, all = TRUE)
dev.off()
setwd("..")
