install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("XML")
install.packages("plyr")

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(XML)
library(dplyr)
library(lubridate)
library(plyr)

#make a dataframe from original data in xml
longline2000 <- xmlToDataFrame("LONGLINE_00.xml")
print(longline2000)

#convert to csv
write.csv(longline2000, file = "longline2000.csv")

#make a dataframe from csv
longline2000_csv <- read.csv(file = "longline2000.csv")

#clear the "null" data
longline2000_csv_cleared <- filter(longline2000_csv, hhooks != 0,!is.null(hhooks))

#remove column of fishing grounds (lat, long) for CpUE calculation
#longline2000_csv_cpue is data without information on fishing ground
longline2000_csv_cpue <- select(longline2000_csv_cleared, yy, mm, hhooks, alb_c, alb_n, yft_c, yft_n, bet_c, bet_n, mls_c, mls_n, blm_c, blm_n, bum_c, bum_n, swo_c, swo_n, oth_c, oth_n)

#add column "total_n", "total_c", "cpue_c" and "cpue_n"
#longline2000_csv_cpue1 is data with total in catch (kg) and n (number of idv tuna)
longline2000_csv_cpue1 <- longline2000_csv_cpue %>% 
  mutate(total_c_kg = (alb_c+yft_c+bet_c+mls_c+blm_c+bum_c+swo_c+oth_c)*1000) %>% 
  mutate(total_n = alb_n+yft_n+bet_n+mls_n+blm_n+bum_n+swo_n+oth_n)

#merge the rows from same same month and year
longline2000_csv_cpue2 <- ddply(longline2000_csv_cpue1, .(yy, mm), summarize, hhooks = sum(hhooks), alb_c = sum(alb_c), alb_n = sum(alb_n), yft_c = sum(yft_c), yft_n = sum(yft_n), 
      bet_c = sum(bet_c), bet_n = sum(bet_n), mls_c = sum(mls_c), mls_n = sum(mls_n), blm_c = sum(blm_c), blm_n = sum(blm_n), bum_c = sum(bum_c), bum_n = sum(bum_n), 
      swo_c = sum(swo_c), swo_n = sum(swo_n), oth_c = sum(oth_c), oth_n = sum(oth_n), total_c_kg = sum(total_c_kg), total_n = sum(total_n))

#create column of cpue, both in c (kg/hhooks) and n (idv/hhooks), and average of weight of each species (kg/n), and date
longline2000_csv_cpue3 <- longline2000_csv_cpue2 %>% mutate(cpue_c = (total_c_kg/hhooks)*100) %>% mutate(cpue_n = (total_n/hhooks)*100) %>% 
  mutate(alb_idv = (alb_c*1000)/alb_n) %>% mutate(yft_idv = (yft_c*1000)/yft_n) %>% mutate(bet_idv = (bet_c*1000)/bet_n) %>% mutate(mls_idv = (mls_c*1000)/mls_n) %>% 
  mutate(blm_idv = (blm_c*1000)/blm_n) %>% mutate(bum_idv = (bum_c*1000)/bum_n) %>% mutate(swo_idv = (swo_c*1000)/swo_n) %>% mutate(oth_idv = (oth_c*1000)/oth_n)

#add column of date from "month (mm)" and year "yy"
longline2000_csv_cpue4 <- within(longline2000_csv_cpue3, Date <- sprintf("%d-%02d", yy, mm))

#NOW, WE HAVE longline2000_csv_cpue3 AS THE DATA THAT READY TO ANALYZE
#We are going to get output:
#1. Fluctuation of Albacore, both in weight (ton) and number of individual.
#2. Correlation between month and both cpue_c (weight) and cpue_n (number of individual)


#correlation between: month and cpue_c
cor(longline2000_csv_cpue3$mm, longline2000_csv_cpue3$cpue_c)

#time series
plot(longline2000_csv_cpue3: alb_idv)