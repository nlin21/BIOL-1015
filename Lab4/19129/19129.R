library(ggplot2)
library(dplyr)

area <- read.csv("areas.csv")
head(area)
ggplot(data = area, aes(x=Area))+
  geom_histogram(fill="pink")+
  xlab("Areas")+
  ylab("Count")+
  theme_classic()
ggsave("19129_area_graph.jpeg")

length <- read.csv("lengths.csv")
head(length)
ggplot(data = length, aes(x=Length))+
  geom_histogram(fill="pink")+
  xlab("Lengths")+
  ylab("Count")+
  theme_classic()
ggsave("19129_length_graph.jpeg")

summary(area)
sdA <- area$Area
sd(sdA)

summary(length)
sdL <- length$Length
sd(sdL)
