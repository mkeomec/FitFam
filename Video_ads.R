#Compare Video ad conversions to non-video (static) ads

#Import raw facebook data
#Video ad Data
video_data <- read.csv(choose.files(default = "", caption = "Select Video ads data",multi = FALSE, filters = Filters, index = nrow(Filters)))

#non-video ad data
static_data <- read.csv(choose.files(default = "", caption = "Select static ads data",multi = FALSE, filters = Filters, index = nrow(Filters)))

# Calculate conversion rates. Conversion / reach
video_conv_rate <- sum(video_data$Website.Conversions, na.rm = TRUE)/sum(video_data$Reach, na.rm=TRUE)
static_conv_rate <- sum(static_data$Website.Conversions, na.rm = TRUE)/sum(static_data$Reach, na.rm=TRUE)
conv_rate <- c(video_conv_rate,static_conv_rate)

# Calculate mean cost per conversion
video_cost_conv <- mean(video_data$Cost.per.Website.Conversion..USD., na.rm=TRUE)
static_cost_conv <- mean(static_data$Cost.per.Website.Conversion..USD., na.rm=TRUE) 
cost_conv <- c(video_cost_conv,static_cost_conv)

#plot conversion rates
barplot(conv_rate, main='Conversions: Video vs static',names.arg=c('Video','Static'))
barplot(cost_conv, main='Cost per conversion: Video vs static',names.arg=c('Video','Static'))
