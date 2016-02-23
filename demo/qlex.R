# for additional information on a function type: ?function_name

# ensure required packages are loaded
library(ggplot2)
library(qlexdatr)
library(lexr)

# set theme and font size
theme_set(theme_bw(base_size = 10))

# input, check and process data
qlex <- input_lex_data("qlexdatr")
check_lex_data(qlex)
qdetect <- make_detect_data(qlex, hourly_interval = 6L, end_date = as.Date("2015-09-26"))
summary(qdetect)

# create directory for plots
dir.create("qlex", showWarnings = FALSE)

# plot base data
setwd("qlex")
png("qlex%03d.png", width = 6, height = 6, units = "in", res = 300)
plot(qlex, all = TRUE)
dev.off()
setwd("..")

# plot processed data
setwd("qlex")
png("qdetect%03d.png", width = 6, height = 6, units = "in", res = 300)
plot(qdetect, all = TRUE)
dev.off()
setwd("..")
