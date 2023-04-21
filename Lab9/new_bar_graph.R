# means comment
# 20
dat <- read.csv("crustacean.txt")
# 24
dat <- read.csv("crustacean.txt", header = TRUE, sep = "\t")
# 27
class(dat)
# 29
dat$lake.name
# 30
class(dat$lake.name)
# 31
class(dat$sum_mg_ww_L)
# 34
head(dat)
# 36
str(dat)
# 37
dat$round <- round(dat$sum_mg_ww_L, digits = 2)
# 40
head(dat)
# 42 important line of code
summary(dat)
# 48
avg <- mean(dat$round)
avg
# 54
dat$away_from_avg <- dat$round - avg
# 56 remove scientific notation
options(scipen = 999)

# 60
install.packages("tidyverse")
# 61
library(dplyr)
# 62
head(dat)
# 64
all_avg <- dat %>% group_by(lake.name) %>% summarise(avg = mean(round))
# 66
ann_avg <- dat %>% group_by(lake.name, Year) %>% summarise(avg = mean(round), stdev = sd(round))
# 71
bgm <- filter(dat, lake.name == "Big Moose")
# 75
two_lakes <- filter(dat, lake.name == "Big Moose" | lake.name == "G")
# 78
large <- filter(two_lakes, round > 2)
# 80
small <- filter(two_lakes, round <= 2)
# 83
write.csv(ann_avg, "annual_average_per_lake_.csv", row.names = FALSE)

# 93
library(ggplot2)
# 94
head(bgm)
# 95-96
ggplot(data = bgm, aes(x = Year, y = round))+
  geom_point()
# 103-106
head(ann_avg)
ggplot(data = ann_avg, aes(x = Year, y = avg))+
  geom_point()+
  facet_wrap(~lake.name)
# 108-113
ggplot(data = ann_avg, aes(x = Year, y = avg))+
  geom_point(color = "blue")+
  facet_wrap(~lake.name)+
  ylab("Animal Density (mg / L)")+
  xlab("")+
  theme_classic()
# 118-124 add a trend line
ggplot(data = bgm, aes(x = Year, y = round))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")+
  theme_classic()+
  ylab("Crustacean Density (mg ww / L)")+
  xlab("")+
  scale_x_continuous(breaks = seq(1990, 2015, by = 5))
# 127
ggsave("scatter.jpeg")
# 130
head(two_lakes)
# 133
last_years <- filter(two_lakes, Year >= 2010)
# 134
head(last_years)
# 139
mean_2_lakes <- last_years %>% group_by(lake.name) %>% summarise(mean = mean(round), sd = sd(round))
# 142
head(mean_2_lakes)
# 145
ggplot(data = mean_2_lakes, aes(x = lake.name, y = mean, fill = lake.name))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2)+
  theme_classic()+
  ggtitle("Annual Average of Crustaceans of Lake Big Moose and Lake G")+
  scale_fill_discrete(name = "Lake Name")+
  scale_y_continuous(limits = c(0, 1))+
  xlab("Lake Name")+
  ylab("Average (mg/L)")
  
# 150
ggsave("bar.jpeg")

