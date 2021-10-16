'''
title: "Homework 3"
author: "Simran Bhatia, Naren Mohan, Srujana Gaddam"
date: "10/12/2021"
'''

####Problem 1 (20 points) The following are historical data on staff salaries (dollars per pupil) for 30 schools sampled in the eastern part of the United States in the early 1970s.
#####(a) Compute the sample mean and sample standard deviation.

sal <- c(3.79, 2.99, 2.45, 2.14, 3.36, 2.05, 3.14, 3.54,
         2.77, 2.91, 3.10, 1.84, 2.52, 3.22, 2.67, 2.52, 2.71,
         2.75, 3.57, 3.85, 2.89, 2.83, 3.13, 2.44, 2.10, 3.71, 
         2.37, 2.68, 3.51, 3.37)

mean_sal <- mean(sal)
std_dev_sal <- sd(sal)

paste("Mean =", round(mean_sal, 2))
paste("Standard Deviation =", round(std_dev_sal, 2))

#####(b) Construct a relative frequency histogram of the data.

library(reshape2)
tab <- melt(table(cut(sal, breaks=c(1.5, 2.0, 2.5, 3.0, 3.5, 4.0))))
tab["ratio"] <- tab["value"] / sum(tab["value"])
ggplot(tab, aes(x=Var1, y=ratio,  color='yellow')) +
  geom_bar(stat="identity")+
  labs(title="Relative frequency histogram of the salaries data",
       x="Salaries",
       y="Relative Frequency")+
  theme(legend.title = element_blank(), 
        legend.position = "none")


#####(c) Construct a stem-and-leaf display of the data

stem(sal, scale=3)

#####(d) Construct a box plot of the data

med <- median(sal)
five <- data.frame(x = rep(1,5), five = fivenum(sal))
df_sal <- as.data.frame(sal)
ggplot(df_sal, aes(y=sal))+
  geom_boxplot(width=0.2)+
  labs(title="Box plot of the salaries per pupil",
       y="Salaries per pupil")+
  scale_x_discrete(breaks = NULL)+
  geom_text(data = five, aes(x=0, y= five, label = five), nudge_x = 0.2)
