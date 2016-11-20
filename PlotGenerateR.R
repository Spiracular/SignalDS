#Copied code for bike chart
#comes from the Kaggle competition, code by Ben Hamner

#Pretty good illustration of the virtues of jitter plots

library(ggplot2)
library(lubridate)
library(scales)

# Tweak these to show something else on the axes
x_axis <- "jitter_times"
y_axis <- "count"
color  <- "temp_f" # for example swap this to "humidity"

# The competition data is stored in the ../input directory
train <- read.csv("KaggleBikeSharing/train.csv")
test  <- read.csv("KaggleBikeSharing/test.csv") 

# Write some basic stats to the log
cat("Number of training rows ", nrow(train), "\n")
cat("Number of test rows ", nrow(test), "\n")
head(train)

train$hour  <- hour(ymd_hms(train$datetime))
train$times <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$jitter_times <- train$times+minutes(round(runif(nrow(train),min=0,max=59)))
train$day <- wday(ymd_hms(train$datetime), label=TRUE)
train$temp_f <- train$temp*9/5+32

p <- ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Temp (°F)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("On workdays, most bikes are rented on warm mornings and evenings\n") +
  theme(plot.title=element_text(size=18))

ggsave("bike_rentals_by_time_and_temperature.png", p)