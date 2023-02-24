library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)

info <- read.csv("Info_Data.csv")
do_temp <- read.csv("Interpolated_Data.csv")

head(info)
head(do_temp)

length(do_temp$row_id)

lg_tempdo <- filter(do_temp, lake_id == 55)

summary(lg_tempdo$depth)
head(lg_tempdo)
str(lg_tempdo) #date shows up as a char
lg_tempdo$date <- as.Date(lg_tempdo$date) #overwrite date as Date format
head(lg_tempdo)

lg_surf_tempdo <- filter(lg_tempdo, depth <= 10)
head(lg_surf_tempdo)
lg_surf_mean <- lg_surf_tempdo %>% 
  group_by(date) %>% 
  summarise(surf_temp = mean(temp), surf_do_con = mean(do_con))
head(lg_surf_mean)
lg_surf_mean$month <- month(lg_surf_mean$date)
head(lg_surf_mean)
lg_surf_mean_summ <- filter(lg_surf_mean, month == 7 | month == 8)
lg_surf_mean_summ$year <- year(lg_surf_mean_summ$date)
head(lg_surf_mean_summ)
lg_surf_mean_summ$ n <- 1
lg_annual_tempdo <- lg_surf_mean_summ %>% 
  group_by(year) %>%
  summarise(surf_temp = mean(surf_temp, na.rm = TRUE),
            surf_do_con = mean(surf_do_con, na.rm = TRUE),
            n = sum(n))

# Graph
summary(lg_annual_tempdo$year)
head(lg_annual_tempdo)
ggplot(data = lg_annual_tempdo)+
  geom_point(aes(x = year, y = surf_temp), color = "red")+
  ylab("Temp °C")+
  xlab("")+
  theme_classic()
# Lin Reg
st_lm <- lm(lg_annual_tempdo$surf_temp ~ lg_annual_tempdo$year)
summary(st_lm)
ggplot(data = lg_annual_tempdo)+
  geom_point(aes(x = year, y = surf_temp, color = "Surface <= 2m"))+
  geom_smooth(aes(x = year, y = surf_temp), method = lm, se = FALSE, color = "red")+
  ylab("Temp °C")+
  xlab("")+
  ylim(15,30)+
  theme_classic()+
  scale_colour_manual(name="", values=c("blue","red"))
ggsave("lake_annie_temp.jpg")

sdo_lm <- lm(lg_annual_tempdo$surf_do_con ~ lg_annual_tempdo$year)
summary(sdo_lm)
ggplot(data = lg_annual_tempdo)+
  geom_point(aes(x = year, y = surf_do_con, color = "Surface <= 2m"))+
  ylab("DO mg/L")+
  xlab("")+
  theme_classic()+
  scale_colour_manual(name="", values=c("blue","red"))
ggsave("lake_annie_o2.jpg")
