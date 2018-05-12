install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("XML")

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(XML)
library(dplyr)
library(lubridate)

#make a dataframe of longline
longline2000 <- xmlToDataFrame("LONGLINE_00.xml")
print(longline2000)

#clear the "null" data
longline2000_catch <- filter(longline2000, hhooks != 0,!is.null(hhooks))

#remove column of fishing grounds (lat, long)
longline2000_catch_cpue <- select(longline2000_catch, yy, mm, hhooks, alb_c, alb_n, yft_c, yft_n, bet_c, bet_n, mls_c, mls_n, blm_c, blm_n, bum_c, bum_n, swo_c, swo_n, oth_c, oth_n)

longline2000_catch_cpue %>%
  mutate(Catch_n_total = alb_n+yft_n+bet_n+mls_n+blm_n+bum_n+swo_n+oth_n)

bet_n <- as.numeric(as.character( longline2000_catch_cpue[, 9] ))
bet_c <- as.numeric(as.character( longline2000_catch_cpue[, 8] ))
bet_c_kg <- (bet_c * 1000)
hhooks <- as.numeric(as.character( longline2000_catch_cpue[, 3] ))

longline2000_catch_cpue %>%
  mutate(bet_c_cpue = bet_c_kg / hhooks)
