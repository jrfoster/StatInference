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





