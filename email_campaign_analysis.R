library(dplyr)
library(plyr)
library(ggplot2)
library(scales)
setwd('C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/Fitfam')

# Load in email campaign data
email_data <-read.csv(file.choose(),header=TRUE)

#Explore data
labels(email_data)

#Open rate and click rate are factors. Convert to numeric

email_data$Open.Rate <- as.numeric(sub("%", "",email_data$Open.Rate,fixed=TRUE))/100
email_data$Click.Rate <- as.numeric(sub("%", "",email_data$Click.Rate,fixed=TRUE))/100

email_data$List_cat=0

# Stratify by list

for (i in 1:nrow(email_data)){
    if (email_data$List[i]=='FitFam Email List'){
        email_data$List_cat[i]='FitFam Email List'
        print(i)
    }else if (email_data$List[i]=='Fun Runners'){
        email_data$List_cat[i]='Fun Runners'
    }else if (email_data$List[i]=='Virtual Runners'){
        email_data$List_cat[i]='Virtual Runners'
    }else email_data$List_cat[i]=0
}

# Calculate mean, standard deviation, standard error for Open Rate

attach(email_data)
OpenRate.list.mean <- aggregate(Open.Rate,by=list(List_cat),FUN=mean,na.rm=TRUE)
OpenRate.list.sd <- aggregate(Open.Rate,by=list(List_cat),FUN=sd,na.rm=TRUE)
OpenRate.list.length <- aggregate(Open.Rate,by=list(List_cat),FUN=length)

OpenRate.list.se <- NULL
for (i in 1:nrow(OpenRate.list.mean['x'])){
    OpenRate.list.se[i] <- OpenRate.list.sd['x'][[i,1]]/sqrt(OpenRate.list.length['x'][[i,1]])
}
OpenRate.list <- data.frame(c(OpenRate.list.mean,OpenRate.list.sd['x']),data.frame(OpenRate.list.se))
colnames(OpenRate.list) <- c('Name','Mean','StDev','StErr')
OpenRate.list
# Calculate mean, standard deviation, standard error for Click rate

ClickRate.list.mean <- aggregate(Click.Rate,by=list(List_cat),FUN=mean,na.rm=TRUE)
ClickRate.list.sd <- aggregate(Click.Rate,by=list(List_cat),FUN=sd,na.rm=TRUE)
ClickRate.list.length <- aggregate(Click.Rate,by=list(List_cat),FUN=length)

ClickRate.list.se <- NULL
for (i in 1:nrow(ClickRate.list.mean['x'])){
    ClickRate.list.se[i] <- ClickRate.list.sd['x'][[i,1]]/sqrt(ClickRate.list.length['x'][[i,1]])
}
ClickRate.list <- data.frame(c(ClickRate.list.mean,ClickRate.list.sd['x']),data.frame(ClickRate.list.se))
colnames(ClickRate.list) <- c('Name','Mean','StDev','StErr')
ClickRate.list
# Find best day of Open Rate

OpenRate.day.mean <- aggregate(Open.Rate,by=list(Send.Weekday),FUN=mean,na.rm=TRUE)
OpenRate.day.sd <- aggregate(Open.Rate,by=list(Send.Weekday),FUN=sd,na.rm=TRUE)
OpenRate.day.length <- aggregate(Open.Rate,by=list(Send.Weekday),FUN=length)
OpenRate.day.se <- NULL
for (i in 1:nrow(OpenRate.day.mean['x'])){
    OpenRate.day.se[i] <- OpenRate.day.sd['x'][[i,1]]/sqrt(OpenRate.day.length['x'][[i,1]])
}
OpenRate.day <- data.frame(c(OpenRate.day.mean,OpenRate.day.sd['x']),data.frame(OpenRate.day.se))
colnames(OpenRate.day) <- c('Name','Mean','StDev','StErr')
OpenRate.day
# Find best day of  Click Rate
ClickRate.day.mean <- aggregate(Click.Rate,by=list(Send.Weekday),FUN=mean,na.rm=TRUE)
ClickRate.day.sd <- aggregate(Click.Rate,by=list(Send.Weekday),FUN=sd,na.rm=TRUE)
ClickRate.day.length <- aggregate(Click.Rate,by=list(Send.Weekday),FUN=length)
ClickRate.day.se <- NULL
for (i in 1:nrow(ClickRate.day.mean['x'])){
    ClickRate.day.se[i] <- ClickRate.day.sd['x'][[i,1]]/sqrt(ClickRate.day.length['x'][[i,1]])
}
ClickRate.day <- data.frame(c(ClickRate.day.mean,ClickRate.day.sd['x']),data.frame(ClickRate.day.se))
colnames(ClickRate.day) <- c('Name','Mean','StDev','StErr')

ClickRate.day
# Calculate Open and click rate by time of day

# Extract time of day from Send.date column

email_data$Send.Time <- strftime(strptime(email_data$Send.Date, "%b %d,%Y %I:%M %p"),format="%H:%M:%S")

# Plot click rate vs time of day
ggplot(data=email_data, aes(x=as.POSIXct(email_data$Send.Time,format="%H:%M:%S"),y=email_data$Click.Rate)) + geom_point()+ scale_x_datetime( breaks = date_breaks("2 hours"),labels=date_format("%H:%M",tz="US/Pacific"))

# Plot open rate vs time of day
ggplot(data=email_data, aes(x=as.POSIXct(email_data$Send.Time,format="%H:%M:%S"),y=email_data$Open.Rate)) + geom_point()+ scale_x_datetime( breaks = date_breaks("2 hours"),labels=date_format("%H:%M",tz="US/Pacific"))

# Create new category for unsubscribe rate
email_data$unsub.rate <- email_data$Unsubscribes/email_data$Total.Recipients
ggplot(data=email_data, aes(x=as.POSIXct(email_data$Send.Date,format="%b %d,%Y %I:%M %p"),y=email_data$unsub.rate))+geom_point(aes(colour = factor(email_data$List_cat)))


