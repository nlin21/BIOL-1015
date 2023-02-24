
# call package

library(dplyr)
library(ggplot2)

# read in data make sure working directory is correct!

pengs_clean <- read.csv("penguins.csv")

# is there a difference in weight between males and females?
# we will pick one penguin species
gentoo <- filter(pengs_clean, species == "Gentoo")
# first get avg and sd using %>% and group_by
head(gentoo)
weight_sum <- gentoo %>% group_by(sex) %>% summarise(weight = mean(body_mass_g), sd = sd(body_mass_g))
head(weight_sum)

# bar plot with sd bars like lab 2
library(ggplot2)
head(weight_sum)
ggplot(data=weight_sum, aes(x=sex, y=weight, fill = sex)) +
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=weight-sd, ymax=weight+sd), width=.2) +
  theme_classic()+
  scale_fill_manual(values=c('blue','orange'))

# you max fix axis labels axis limits etc...
# we will use a t-test
# please note that we use the entire data set for this
# y variable ~ x variable
t.test(gentoo$body_mass_g ~ gentoo$sex)
# p value is probability that these two groups came from the same distribution
# in other words the prob that these two distributions are the same
# if this portability is low then we say they are statistically significant and different
# low is anything <= 0.05
#             p-value < 2.2e-16
# interpretation: male penguins are heavier than female penguins

# next part
# Are two variables related?
# bill length and bill depth
# start with a scatter plot
# think about independent vs dependent x and y

head(gentoo)
ggplot(data = gentoo, aes(x=bill_length_mm,y=bill_depth_mm ))+
geom_point(size =3)

# looks like a positive relationship
# we will use a linear regression
# y~x
# create a regression object
reg <- lm(gentoo$bill_depth_mm ~ gentoo$bill_length_mm)
# get summary of regression object
summary(reg)
# p-value: 7.337e-16 significant
# use the multiple r2 of 0.43
# in other words 43% of the bill depth can be explained by the bill length

# since this is significant regression we will add trend line with geom_smooth
# there are different methods today we will use lm for linear model
ggplot(data = gentoo, aes(x=bill_length_mm,y=bill_depth_mm ))+
  geom_point(size =3)+
  geom_smooth(method="lm", se = FALSE, color = "red", linetype = "dashed")

