library(datasets)
library(ggplot2)
library(dplyr)

data("ToothGrowth")

smDose <- ToothGrowth %>%
  filter(dose == .5)

mdDose <- ToothGrowth %>%
  filter(dose == 1.0)

lgDose <- ToothGrowth %>%
  filter(dose == 2.0)

ojData <- ToothGrowth %>%
  filter(supp == 'OJ')
#  group_by(factor(dose)) %>%
#  summarize("mean" = mean(len), "variance" = var(len), "sd" = sd(len))

stdData <- ToothGrowth %>%
  filter(supp == 'VC')
#  group_by(factor(dose)) %>%
#  summarize("mean" = mean(len), "variance" = var(len), "sd" = sd(len))

ojT <- t.test(ToothGrowth$len)


ojPlot <- ToothGrowth %>%
  ggplot(aes(x=factor(dose), y=len, group=supp, color=supp)) +
  ggtitle("Odontoblast Length as a Function of Dose\n") +
  labs(x = "Dosage (mg)", y = "Odontoblasts Length (microns)", color="Supplement") +
  geom_point() +
  stat_summary(aes(group=supp, color=supp), fun.y=mean, geom="line") +
  theme_bw(base_size = 10)

stdPlot <- ToothGrowth %>%
  filter(supp == 'VC') %>%
  ggplot(aes(factor(dose), len)) +
  geom_violin()
  #stat_summary(fun.y=mean, geom="line", aes(group=1))

grid.arrange(ojPlot, stdPlot)


ggplot(ToothGrowth, aes(x=len)) + 
  geom_histogram(aes(y = ..density..), col = "azure4", fill = "cornflowerblue", alpha = .5, binwidth = 2) +
  geom_density(col = "black", fill = "cornflowerblue", alpha = .25)

data.frame(
  "Orange Juice",
  "Ascorbic Acid",
  paste("[", round(t4$conf.int[1],4), ", ", round(t4$conf.int[2],4), "]", sep=""),
  round(unname(t4$parameter), 2),
  qt(.975, unname(t4$parameter)),
  4,
  5)





