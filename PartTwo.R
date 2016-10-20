suppressMessages(library(datasets))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(knitr))
data("ToothGrowth")


str(ToothGrowth)
paste("Complete Cases:", sum(complete.cases(ToothGrowth)))


ToothGrowth %>%
  ggplot(aes(x=factor(dose), y=len, group=supp, color=supp)) +
  ggtitle("Odontoblast Length as a Function of Dose\n") +
  labs(x = "Dosage (mg)", y = "Odontoblasts Length (microns)", color="Supplement") +
  geom_point() +
  stat_summary(aes(group=supp, color=supp), fun.y=mean, geom="line") +
  theme_bw(base_size = 10)


t1 <- t.test(sOJ$len, sVC$len, conf.level=.975, paired=FALSE, var.equal=FALSE)
kable(
  data.frame(
    "Ascorbic Acid", 
    "Orange Juice", 
    paste("[", round(t1$conf.int[1],4), ", ", round(t1$conf.int[2],4), "]", sep=""), 
    round(unname(t1$parameter), 2),  
    qt(.9875, unname(t1$parameter)), 
    unname(t1$statistic), t1$p.value), 
  col.names = c("Group 1 Supp", "Group 2 Supp", "Conf Int", 
                "Deg F", "Tabulated t-value", "t-statistic", "p-value"))


t2 <- t.test(d1$len, d05$len, conf.level=.975, paired=FALSE, var.equal=FALSE)
t3 <- t.test(d2$len, d05$len, conf.level=.975, paired=FALSE, var.equal=FALSE)
t4 <- t.test(d2$len, d1$len, conf.level=.975, paired=FALSE, var.equal=FALSE)
g1dose <- c("1", "2", "2")
g2dose <- c(".5", ".5", "1")
ci <- c(paste("[", round(t2$conf.int[1], 4), ", ", round(t2$conf.int[2], 4), "]", sep=""), 
        paste("[", round(t3$conf.int[1], 4), ", ", round(t3$conf.int[2], 4), "]", sep=""), 
        paste("[", round(t4$conf.int[1], 4), ", ", round(t4$conf.int[2], 4), "]", sep=""))
df <- c(unname(t2$parameter),
        unname(t3$parameter),
        unname(t4$parameter))
tT <- c(qt(.9875, unname(t2$parameter)),
        qt(.9875, unname(t3$parameter)),
        qt(.9875, unname(t4$parameter)))
cT <- c(unname(t2$statistic),
        unname(t3$statistic),
        unname(t4$statistic))
pval <- c(t2$p.value, t3$p.value, t4$p.value)
kable(data.frame(g1dose, g2dose, ci, df, tT, cT, pval), 
      col.names = c("Group 1 Dose","Group 2 Dose","Conf Int", "Deg F", 
                    "Tabulated t-value", "t-statistic", "p-value"))


allP <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value)
g1 <- c("Ascorbic Acid", "1", "2", "2")
g2 <- c("Orange Juice", ".5", ".5", "1")
adjP <- p.adjust(allP, method="bonferroni")
origConclusion <- allP > .025
adjConclusion <- adjP > .025
kable(data.frame(g1, g2, allP, origConclusion, adjP, adjConclusion), 
      col.names = c("Group1", "Group 2", "Original p-value", "> alpha?", 

                    
                    adjP <- p.adjust(allP, method="BH")
adjConclusion <- adjP > .025
kable(data.frame(g1, g2, allP, origConclusion, adjP, adjConclusion), 
      col.names = c("Group1", "Group 2", "Original p-value", "> alpha?", 
                    "Adjusted p-value", "> alpha?"))
