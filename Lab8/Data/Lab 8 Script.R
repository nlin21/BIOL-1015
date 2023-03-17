
library(dplyr)
library(ggplot2)

#read in air temperature data
global_temp.df  <- read.csv("global_temp_cleaned.csv")

# co2 dat set
loa_co2.df <- read.csv("loa_co2_cleaned.csv")

# Number 4
# you should clean up figure
ggplot(data = global_temp.df, aes(x=year, 
  y = annual_temp_c,))+
  geom_point(size = 0.75)+
  theme_classic()+
  geom_smooth(se = FALSE, method = lm, color = "blue")+
  xlab("Year")+
  ylab("Degrees in Celsius")
ggsave("climate_change_graph.jpg")

options(scipen = 999)
regression <- lm(global_temp.df$annual_temp_c ~ 
                   global_temp.df$year)
summary(regression)

# 4 a-c review output and where the answers came from

# 4a equation
# slope is 0.007
# y = mx+b
# y = 0.007*x+ -14.82

# 4 b r2 is the multiple r2 which is 0.75

# 4 c rate fo change
#for every 1 unit increase in x y increases 
# by 0.007

# in other words each year air temp is increasing by 0.007 degreees c

# filter some year in 1950s
library(dplyr)
# new plot and stats
# then google drive
head(global_temp.df)
recent <- filter(global_temp.df, year >= 1955)
ggplot(data = recent, aes(x=year, y = annual_temp_c))+
  geom_point(size = 0.75)+
  theme_classic()+
  geom_smooth(se = FALSE, method = lm, color = "blue")+
  xlab("Year")+
  ylab("Degrees in Celsius")
ggsave("1955_climate_change.jpg")
recent_regression <- lm(recent$annual_temp_c ~ recent$year)
summary(recent_regression)

# CO2
head(loa_co2.df)
ggplot(data = loa_co2.df, aes(x=year, y = annual_co2_ppm))+
  geom_point(size = 1)+
  theme_classic()+
  geom_smooth(se = FALSE, method = lm, color = "blue")+
  xlab("Year")+
  ylab("CO2 in Parts per Million")
ggsave("co2_change.jpg")
co2_regression <- lm(loa_co2.df$annual_co2_ppm ~ loa_co2.df$year)
summary(co2_regression)

# use the combo data set for part B
# combined data set for last part
combo <- full_join(global_temp.df, loa_co2.df)
combo <- na.omit(combo)
head(combo)
ggplot(data = combo, aes(x=annual_co2_ppm, y = annual_temp_c))+
  geom_point(size = 1)+
  theme_classic()+
  geom_smooth(se = FALSE, method = lm, color = "blue")+
  xlab("CO2 in Parts per Million")+
  ylab("Degrees in Celsius")
ggsave("combo_change.jpg")
combo_regression <- lm(combo$annual_co2_ppm ~ combo$annual_temp_c)
summary(combo_regression)
