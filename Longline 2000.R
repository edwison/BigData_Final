install.packages("tidyverse")
install.packages("ggplot2")
install.packages("XML")
install.packages("plyr")

library(tidyverse)
library(ggplot2)
library(XML)
library(plyr)
library(gridExtra)
library(dplyr)
library(lubridate)

#make a dataframe from original data (xml format)
longline2000 <- xmlToDataFrame("LONGLINE_00.xml")
print(longline2000)

#convert the original data into csv in order to make it available to processed in R
write.csv(longline2000, file = "longline2000.csv")

#make a dataframe from file that we just converted into csv
longline2000_csv <- read.csv(file = "longline2000.csv")

#clear the "null" data
longline2000_csv_cleared <- filter(longline2000_csv, hhooks != 0,!is.null(hhooks))

#remove column of fishing grounds (lat, long) for CpUE calculation and merging the catch from same month and year
#longline2000_csv_cpue is data without information on fishing ground
longline2000_csv_cpue <- select(longline2000_csv_cleared, yy, mm, hhooks, alb_c, alb_n, yft_c, yft_n, bet_c, bet_n, mls_c, mls_n, blm_c, blm_n, bum_c, bum_n, swo_c, swo_n, oth_c, oth_n)

#add column "total_n", "total_c", "cpue_c" and "cpue_n"
#longline2000_csv_cpue1 is data with total in catch (kg) and n (number of idv tuna)
longline2000_csv_cpue1 <- longline2000_csv_cpue %>% 
  mutate(total_c_ton = alb_c+yft_c+bet_c+mls_c+blm_c+bum_c+swo_c+oth_c) %>% 
  mutate(total_c_kg = (alb_c+yft_c+bet_c+mls_c+blm_c+bum_c+swo_c+oth_c)*1000) %>% 
  mutate(total_n = alb_n+yft_n+bet_n+mls_n+blm_n+bum_n+swo_n+oth_n)

#merge the rows from same same month and year
longline2000_csv_cpue2 <- ddply(longline2000_csv_cpue1, .(yy, mm), summarize, hhooks = sum(hhooks), alb_c = sum(alb_c), alb_n = sum(alb_n), yft_c = sum(yft_c), yft_n = sum(yft_n), 
      bet_c = sum(bet_c), bet_n = sum(bet_n), mls_c = sum(mls_c), mls_n = sum(mls_n), blm_c = sum(blm_c), blm_n = sum(blm_n), bum_c = sum(bum_c), bum_n = sum(bum_n), 
      swo_c = sum(swo_c), swo_n = sum(swo_n), oth_c = sum(oth_c), oth_n = sum(oth_n), total_c_ton = sum(total_c_ton), total_c_kg = sum(total_c_kg), total_n = sum(total_n))

#create column of cpue, both in c (kg/hhooks) and n (idv/hhooks), and average of individual weight of each species (kg/n), and date
longline2000_csv_cpue3 <- longline2000_csv_cpue2 %>% mutate(cpue_c = (total_c_kg/hhooks)*100) %>% mutate(cpue_n = (total_n/hhooks)*100) %>% 
  mutate(alb_idv = (alb_c*1000)/alb_n) %>% mutate(yft_idv = (yft_c*1000)/yft_n) %>% mutate(bet_idv = (bet_c*1000)/bet_n) %>% mutate(mls_idv = (mls_c*1000)/mls_n) %>% 
  mutate(blm_idv = (blm_c*1000)/blm_n) %>% mutate(bum_idv = (bum_c*1000)/bum_n) %>% mutate(swo_idv = (swo_c*1000)/swo_n) %>% mutate(oth_idv = (oth_c*1000)/oth_n)

#add column of date from "month (mm)" and year "yy"
longline2000_csv_cpue4 <- within(longline2000_csv_cpue3, Date <- sprintf("%d-%02d", yy, mm))

#convert the final dataframe into csv. just in case we need.
write.csv(longline2000_csv_cpue4, file = "longline2000_csv_cpue4.csv")

#make new dataframes for each species
#add column on Catch per Unit Effort (CpUE) both in weight (kg/100 hooks) and number of individual (n/100 hooks).
#alb (Albacore Tuna)
albplot <- longline2000_csv_cpue4 %>% select(yy, mm, hhooks, alb_c, alb_n, alb_idv) %>% group_by(yy,mm) %>% 
  mutate(alb_c_kg = alb_c*1000) %>% mutate(cpue_alb_n = (alb_n/hhooks)*100) %>%
  mutate(cpue_alb_c_kg = (alb_c_kg/hhooks)*100) %>% mutate(cpue_alb_c_ton = (alb_c/hhooks)*100)

#yft (Yellowfin Tuna)
yftplot <- longline2000_csv_cpue4 %>% select(yy, mm, hhooks, yft_c, yft_n, yft_idv) %>% group_by(yy,mm) %>% 
  mutate(yft_c_kg = yft_c*1000) %>%
  mutate(cpue_yft_n = (yft_n/hhooks)*100) %>% mutate(cpue_yft_c_kg = (yft_c_kg/hhooks)*100) %>% mutate(cpue_yft_c_ton = (yft_c/hhooks)*100)

#bet (Bigeye Tuna)
betplot <- longline2000_csv_cpue4 %>% select(yy, mm, hhooks, bet_c, bet_n, bet_idv) %>% group_by(yy,mm) %>% 
  mutate(bet_c_kg = bet_c*1000) %>%
  mutate(cpue_bet_n = (bet_n/hhooks)*100) %>% mutate(cpue_bet_c_kg = (bet_c_kg/hhooks)*100) %>% mutate(cpue_bet_c_ton = (bet_c/hhooks)*100)

#mls (Stripped Marlin)
mlsplot <- longline2000_csv_cpue4 %>% select(yy, mm, hhooks, mls_c, mls_n, mls_idv) %>% group_by(yy,mm) %>% 
  mutate(mls_c_kg = mls_c*1000) %>%
  mutate(cpue_mls_n = (mls_n/hhooks)*100) %>% mutate(cpue_mls_c_kg = (mls_c_kg/hhooks)*100) %>% mutate(cpue_mls_c_ton = (mls_c/hhooks)*100)

#blm (Black Marlin)
blmplot <- longline2000_csv_cpue4 %>% select(yy, mm, hhooks, blm_c, blm_n, blm_idv) %>% group_by(yy,mm) %>% 
  mutate(blm_c_kg = blm_c*1000) %>%
  mutate(cpue_blm_n = (blm_n/hhooks)*100) %>% mutate(cpue_blm_c_kg = (blm_c_kg/hhooks)*100) %>% mutate(cpue_blm_c_ton = (blm_c/hhooks)*100)

#bum (Blue Marlin)
bumplot <- longline2000_csv_cpue4 %>% select(yy, mm, hhooks, bum_c, bum_n, bum_idv) %>% group_by(yy,mm) %>% 
  mutate(bum_c_kg = bum_c*1000) %>%
  mutate(cpue_bum_n = (bum_n/hhooks)*100) %>% mutate(cpue_bum_c_kg = (bum_c_kg/hhooks)*100) %>% mutate(cpue_bum_c_ton = (bum_c/hhooks)*100)

#swo (Swordfish)
swoplot <- longline2000_csv_cpue4 %>% select(yy, mm, hhooks, swo_c, swo_n, swo_idv) %>% group_by(yy,mm) %>% 
  mutate(swo_c_kg = swo_c*1000) %>%
  mutate(cpue_swo_n = (swo_n/hhooks)*100) %>% mutate(cpue_swo_c_kg = (swo_c_kg/hhooks)*100) %>% mutate(cpue_swo_c_ton = (swo_c/hhooks)*100)


#Now, we have "longline2000_csv_cpue4" and "dataframe of each species" as the data that ready to analyze.
#We are going to get output: Fluctuation of tunas (alb, yft, bet) both in average individual weight (kg) and number of individual.

#alb number
scatter.smooth(x=albplot$mm, y=albplot$alb_n, main="Albacore Tuna - number of idv")
#alb catch (kg)
scatter.smooth(x=albplot$mm, y=albplot$alb_idv, main="Albacore Tuna - average weight of idv")

#yft number
scatter.smooth(x=yftplot$mm, y=yftplot$yft_n, main="Yellowfin Tuna - number of idv")
#yft catch (kg)
scatter.smooth(x=yftplot$mm, y=yftplot$yft_idv, main="Yellowfin Tuna - average weight of idv")

#bet number
scatter.smooth(x=betplot$mm, y=betplot$bet_n, main="Bigeye Tuna - number of idv")
#bet catch (kg)
scatter.smooth(x=betplot$mm, y=betplot$bet_idv, main="Bigeye Tuna - average weight of idv")

#mls number
scatter.smooth(x=mlsplot$mm, y=mlsplot$mls_n, main="Stripped Marlin - number of idv")
#mls catch (kg)
scatter.smooth(x=mlsplot$mm, y=mlsplot$mls_idv, main="Stripped Marlin - average weight of idv")

#blm number
scatter.smooth(x=blmplot$mm, y=blmplot$blm_n, main="Black Marlin - number of idv")
#blm catch (kg)
scatter.smooth(x=blmplot$mm, y=blmplot$blm_idv, main="Black Marlin - average weight of idv")

#bum number
scatter.smooth(x=bumplot$mm, y=bumplot$bum_n, main="Blue Marlin - number of idv")
#bum catch (kg)
scatter.smooth(x=bumplot$mm, y=bumplot$bum_idv, main="Blue Marlin - average weight of idv")

#swo number
scatter.smooth(x=swoplot$mm, y=swoplot$swo_n, main="Swordfish - number of idv")
#swo catch (kg)
scatter.smooth(x=swoplot$mm, y=swoplot$swo_idv, main="Swordfish - average weight of idv")

#total number
scatter.smooth(x=longline2000_csv_cpue4$mm, y=longline2000_csv_cpue4$total_n, main="Total - number of idv")
#total catch (ton)
scatter.smooth(x=longline2000_csv_cpue4$mm, y=longline2000_csv_cpue4$total_c_ton, main="Total - weight of idv")




