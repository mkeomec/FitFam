# FitFam 
# Question: What ages has converted best?
# To address: Plot histogram of conversions grouped by age categories

# Import raw data file that was exported from Facebook ads manager
age_data <- read.csv(file.choose(), header=TRUE)

#sort by age asecending order
age_data <- age_data[order(age_data$Age),]
barplot(age_data$Website.Conversions, names.arg=c('13-17','18-24','25-34','35-44','45-54','55-64','65+','Unknown'))
