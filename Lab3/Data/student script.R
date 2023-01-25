# this is code for you
library(ggplot2)
library(dplyr)
# check working directory!
# read in your data set as a csv
# this is an example data set you need to create your own spread sheet from the 
# student data sheet and load that in instead!
dat <- read.csv("student_data_sheet.csv")
head(dat)

# this is tricky to figure out so here is the bare bones code go ahead and modify it make it look nice for your data
# this 'maps' the location for each sequence
# first idnetify data
ggplot(data = dat)+
  # next we want ot make a segment from the start to the end wiht the sequnce # being the y axis
  geom_segment(aes(x = start_site, y = Sequence, xend = end_site, yend = Sequence), size=5, color="blue")+
  # just make a nice theme
  theme_classic()+
  # change x axis label
  xlab("Location (1-5000)")+
  ylab("Sequence # (1-20)")
# you should add axis labels change colors etc
# save plot and add to word document with caption and 
# bullet point take away
ggsave("segment.jpeg")
  


# Create box and whisker plot for GC content and length 
# hint use geom_boxplot()
# leave x blank and make y = to your variable of choice
# modify graphs to make them look nice
# paste into word document

# length box plot
ggplot(data = dat)+
  geom_boxplot(aes(y = length), color = "blue", fill = "pink")+
  theme_classic()
ggsave("length_box_plot.jpeg")

# GC content box plot
ggplot(data = dat)+
  geom_boxplot(aes(y = GC_content), color = "blue", fill = "pink")+
  theme_classic()
ggsave("gc_content_box_plot.jpeg")

# calculate summary statistics for 
# gc content and length
# find the mean median min max
# put in a table add to word doc 
summary(dat)


