dat <- read.csv("Apply Data needs more arranging.csv")
head(dat)
str(dat)
library(tidyverse)
avg <- dat %>% group_by(Species, Treatment) %>%
  summarise(mean=mean(Abs), sd=sd(Abs))
unique(avg$Species)
avg$Species <- factor(avg$Species,
                      levels = c('bkg', 'A. hyd', 'B. sub', 'C.fre',
                                 'E.col', 'P.flu', 'S. epi', 'S. mar'))

library(ggplot2)
ggplot(data=avg, aes(Species, y=mean, fill=Treatment))+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position="dodge")+
  theme_classic()+
  ylab("Bacteria Proxy (Abs)")
ggsave("other.jpeg")

res.aov2 <- aov(Abs ~ Treatment + Species, data=dat)
summary(res.aov2)  
res.aov3 <- aov(Abs ~ Treatment * Species, data=dat) 
summary(res.aov3)
TukeyHSD(res.aov3)
