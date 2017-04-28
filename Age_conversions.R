# FitFam 
# Question: What ages has converted best?
# To address: Plot histogram of conversions grouped by age categories

# Import raw data file that was exported from Facebook ads manager
age_data <- read.csv(file= "C:/Users/cwbishop/Documents/GitHub/FitFam/FitFam-All-Campaigns-Purchases.csv",head=TRUE,sep=",")
#age_data <- read.csv(file.choose(), header=TRUE)

#Remove non-campaign data by eliminating campaigns that do not have website purchases
age_data <- age_data[complete.cases(age_data[,'Website.Purchases']),]

#Aggregate data by age
attach(age_data)

purchase <-aggregate(age_data$Website.Purchases, by=list(Age), 
                    FUN=sum, na.rm=TRUE)

cost_purchase <- aggregate(age_data$Cost.per.Website.Purchase..USD., by=list(Age), 
                           FUN=mean, na.rm=TRUE)

#plot conversions grouped by age
barplot(purchase[,2], main='Purchases by age',names.arg=c('18-24','25-34','35-44','45-54','55-64','65+'))

#plot Cost per conversion
barplot(cost_purchase[,2], main='$ Cost per purchase by age',names.arg=c('18-24','25-34','35-44','45-54','55-64','65+'))