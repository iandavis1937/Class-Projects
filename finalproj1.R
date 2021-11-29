library(dplyr)
library(psych)
library(effects)
library(tidyr)
library(ggplot2)
library(purrr)
library(rmarkdown)

lib<-c("lme4", "readr", "tidyr", "effects", "ggplot2", "psych", "MASS",
       "Rmisc","plyr", "dplyr", "lmerTest", "ggthemes", "lsmeans", "pastecs",
       "sjstats", "car","irr", "reshape2", "purrr", "broom", "rmarkdown",
       "shiny")
lapply(lib,require,character.only=TRUE)
lapply(lib, old.packages, character.only = TRUE)


setwd("C:/Users/Ian Davis/Desktop/Intro Data Sci/Final Project")
df_counties2 <- read.csv("voteMortalityDemographics.csv")
df_counties <- read.csv("votingDataForMapping.csv")
df_individual <- read.csv("cces2016.csv")

### Descriptives ####

head(df_counties)
#not used###
summarystats <- df_counties %>%
  summarise(across(
    (c(totalMortality2014, rShift, selfChg1980to2014, popChg15)), 
    list(min = min, mean = mean, median = median, max = max, sd = sd), na.rm = TRUE,
    .names = "{.col}.{.fn}"))

#used###
my_sum <- function(v){
  if(!any(is.na(v))){
    res <- c(summary(v),"NA's"=0)
  } else{
    res <- summary(v)
  }
  return(res)
}

#Variable vectors
totalMortality2014_v <- df_counties$totalMortality2014
rShift_v <- df_counties$rShift
selfChg1980to2014_v <- df_counties$selfChg1980to2014
popChg15_v <- df_counties$popChg15
personal_econ_worse_v <- df_individual$personal_econ_worse
race_attitudes_v <- df_individual$race_attitudes

#NA's out of totals
options(scipen = 10)
a <- c(sum(!is.na(totalMortality2014_v)), sum(is.na(totalMortality2014_v)), 
  (sum(is.na(totalMortality2014_v))/sum(!is.na(totalMortality2014_v))))

b <- c(sum(!is.na(rShift_v)), sum(is.na(rShift_v)), 
  (sum(is.na(rShift_v))/sum(!is.na(rShift_v))))

c <- c(sum(!is.na(selfChg1980to2014_v)), sum(is.na(selfChg1980to2014_v)), 
  (sum(is.na(selfChg1980to2014_v))/sum(!is.na(selfChg1980to2014_v))))

d <- c(sum(!is.na(popChg15_v)), sum(is.na(popChg15_v)), 
  (sum(is.na(popChg15_v))/sum(!is.na(popChg15_v))))

e <- c(sum(!is.na(personal_econ_worse_v)), sum(is.na(personal_econ_worse_v)), 
  (sum(is.na(personal_econ_worse_v))/sum(!is.na(personal_econ_worse_v))))

f <- c(sum(!is.na(race_attitudes_v)), sum(is.na(race_attitudes_v)), 
  (sum(is.na(race_attitudes_v))/sum(!is.na(race_attitudes_v))))
f
g <- as.data.frame(a) %>% mutate(b, c, d, e, f)
                          
g <- t(g)

#More descriptives
summarystatsA <- as.vector(as.numeric(as.character(my_sum(df_counties$totalMortality2014))))
summarystatsB <- as.vector(as.numeric(as.character(my_sum(df_counties$rShift))))
summarystatsC <- as.vector(as.numeric(as.character(my_sum(df_counties$selfChg1980to2014))))
summarystatsD <- as.vector(as.numeric(as.character(my_sum(df_counties$popChg15))))
summarystatsE <- as.vector(as.numeric(as.character(my_sum(df_individual$personal_econ_worse))))
summarystatsF <- as.vector(as.numeric(as.character(my_sum(df_individual$race_attitudes))))

summarystats <- data.frame(summarystatsA, summarystatsB, summarystatsC,
                           summarystatsD, summarystatsE, summarystatsF,
                           row.names = c("Min.", "1st Qu.", "Median", "Mean", 
                                         "3rd Qu.", "Max.", "NA's"))
colnames(summarystats) <- c("totalMortality2014", "rShift", "selfChg1980to2014", "popChg15",
                            "personal_econ_worse", "race_attitudes")

write.csv(summarystats, "C:/Users/Ian Davis/Desktop/Intro Data Sci/Final Project/summarystats.csv")

#Correlation matrix ####
df_counties_num <- df_counties %>% select(where(is.numeric))

cor_table <- cor(df_counties_num)
#filter out NAs? #
cor_table <- cor_table %>% as.data.frame(cor_table) %>% 
  select(where(!is.na(cor_table)))
  
  filter(across(.cols = everything(), !is.na()))

  
# Plots ####
  
df_counties %>% ggplot(mapping = aes(x = drugChg1980to2014, y = rShift)) +
                  geom_point() +
                  geom_smooth()
    
df_counties %>% ggplot(mapping = aes(x = selfChg1980to2014, y = rShift)) +
    geom_point() +
    geom_smooth()
  