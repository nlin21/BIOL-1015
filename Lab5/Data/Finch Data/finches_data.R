library(dplyr)
library(ggplot2)

finches <- read.csv("finches_data_for_lab.csv")
head(finches)

# Calculating Descriptive Statistics 
body_mass <- finches %>% 
  group_by(drought_survival) %>% 
  summarise(mean_body_mass = mean(body_mass_g), sd_body_mass = sd(body_mass_g))
head(body_mass)

wing_length <- finches %>% 
  group_by(drought_survival) %>% 
  summarise(mean_wing_length = mean(wing_length_mm), sd_wing_length = sd(wing_length_mm))
head(wing_length)

tarsus_length <- finches %>% 
  group_by(drought_survival) %>% 
  summarise(mean_tarsus_length = mean(tarsus_length_mm), sd_tarsus_length = sd(tarsus_length_mm))
head(tarsus_length)

beak_depth <- finches %>% 
  group_by(drought_survival) %>% 
  summarise(mean_beak_depth = mean(beak_depth_mm), sd_beak_depth = sd(beak_depth_mm))
head(beak_depth)


# Graphing the Data
ggplot(data = body_mass, aes(x = drought_survival, y = mean_body_mass, fill = drought_survival))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean_body_mass-sd_body_mass, ymax=mean_body_mass+sd_body_mass), width=.2)+
  theme_classic()+
  ggtitle("Body Mass Bar Graph")+
  xlab("Drought Survivorship")+
  ylab("Body Mass (g.)")
ggsave("body_mass_bar_graph.jpeg")

ggplot(data = wing_length, aes(x = drought_survival, y = mean_wing_length, fill = drought_survival))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean_wing_length-sd_wing_length, ymax=mean_wing_length+sd_wing_length), width=.2)+
  theme_classic()+
  ggtitle("Wing Length Bar Graph")+
  xlab("Drought Survivorship")+
  ylab("Wing Length (mm.)")
ggsave("wing_length_bar_graph.jpeg")

ggplot(data = tarsus_length, aes(x = drought_survival, y = mean_tarsus_length, fill = drought_survival))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean_tarsus_length-sd_tarsus_length, ymax=mean_tarsus_length+sd_tarsus_length), width=.2)+
  theme_classic()+
  ggtitle("Tarsus Length Bar Graph")+
  xlab("Drought Survivorship")+
  ylab("Tarsus Length (mm.)")
ggsave("tarsus_length_bar_graph.jpeg")        

ggplot(data = beak_depth, aes(x = drought_survival, y = mean_beak_depth, fill = drought_survival))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean_beak_depth-sd_beak_depth, ymax=mean_beak_depth+sd_beak_depth), width=.2)+
  theme_classic()+
  ggtitle("Beak Depth Bar Graph")+
  xlab("Drought Survivorship")+
  ylab("Beak Depth (mm.)")
ggsave("beak_depth_bar_graph.jpeg")  


# Calculating T-Test Statistics
head(finches)
t.test(finches$body_mass_g ~ finches$drought_survival)
t.test(finches$wing_length_mm ~ finches$drought_survival)
t.test(finches$tarsus_length_mm ~ finches$drought_survival)
t.test(finches$beak_depth_mm ~ finches$drought_survival)

# Wing Length vs. Beak Depth
head(finches)

survivors <- filter(finches, drought_survival == "survivor")
ggplot(data = survivors, aes(x = beak_depth_mm, y = wing_length_mm))+
  geom_point(size=3)+
  theme_classic()+
  ggtitle("Beak Depth vs. Wing Length in Survivors")+
  xlab("Beak Depth (mm.)")+
  ylab("Wing Length (mm.)")
ggsave("wing_length_vs_beak_depth.jpeg")

reg <- lm(survivors$wing_length_mm ~ survivors$beak_depth_mm)
summary(reg)
