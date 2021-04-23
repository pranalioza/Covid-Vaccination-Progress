library(dplyr)
library(tidyverse)
library(tidyr)

#exploring data

df <- read.csv("C:/Users/prana/Desktop/R Mini project/country_vaccinations.csv")
view(df)

#drop unnecessary columns - source name and source website
df = subset(df, select = -c(source_name, source_website))
view(df)

str(df)
head(df)
tail(df)
dim(df)
nrow(df)
ncol(df)
summary(df) 


#preprocessing

view(df)

names(which(colSums(is.na(df)) > 0)) #columns with null values

sum(is.na(df)) #check sum of NA values in dataset

#replacing null values with mean of column
for(column in names(df)){
  df[[column]][is.na(df[[column]])]<-mean(df[[column]],na.rm=TRUE)
}

sum(is.na(df)) #check sum of NA values in dataset, should be 0

view(df)


#statistics of dataset
mean(df$total_vaccinations)
mean(df$people_fully_vaccinated)

median(df$total_vaccinations)
quantile(df$total_vaccinations)
var(df$total_vaccinations)

df%>%distinct(country) 

df%>%distinct(country)%>%count()  #number of countries included in the dataset

#exploratory analysis 

less_than_million <- df[df$daily_vaccinations_per_million < 1, ]
less_than_million  #show entries of countries with daily vaccinations less than 1 million

# Number of countries with daily vaccinations less than 1 million
count(less_than_million)

# Adding category of grade column  - is it to be done? can it be done? 


#Data visualization

library(ggplot2)

vaccine_names = subset(df, select = c(vaccines))

vaccine_names

pfizer_moderna_count = vaccine_names %>% filter(
  vaccines %in% c("Moderna, Oxford/AstraZeneca", "Pfizer/BioNTech" )
)

pfizer_moderna_count

ggplot(df, aes(x=pfizer_moderna_count))+ geom_bar()
