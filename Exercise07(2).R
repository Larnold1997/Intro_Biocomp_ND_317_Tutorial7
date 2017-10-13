#######Problem 2
library(ggplot2)
library(grid)
library(gridExtra)

#read the data file
data2 <- read.table(file = "winsAndHRs.txt", header = TRUE, sep="\t")

#plot the proportion of wins as a function of HRs
a = ggplot(data = data2, aes(x = HR , y = Win))
a + geom_point() + coord_cartesian() + theme_bw() + xlab("Homeruns") + ylab("Win Percentage")

#adding in the trend line
a + geom_point() + coord_cartesian() + theme_bw() + xlab("Homeruns") + ylab("Win Percentage") + stat_smooth(method = "lm")
