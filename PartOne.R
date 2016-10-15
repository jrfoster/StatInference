library(ggplot2)
library(gridExtra)
library(matrixStats)

windowsFonts(Avenir = windowsFont("Avenir Medium"))

ex1 <- data.frame(val = rexp(1000, 1.33))
ex1Plot <-  ggplot(ex1, aes(x=val)) + 
  geom_density(col = "black", fill = "cornflowerblue", alpha = .25) +
  ggtitle(expression(paste("Exponential distribution with ", lambda, " = 1.33"))) +
  labs(x = "X", y = "Density") +
  theme_bw(base_family = "Avenir", base_size = 12)

ex2 <- data.frame(val = rexp(1000, 1))
ex2Plot <-  ggplot(ex2, aes(x=val)) + 
  geom_density(col = "black", fill = "cornflowerblue", alpha = .25) +
  ggtitle(expression(paste("Exponential distribution with ", lambda, " = 1.00"))) +
  labs(x = "X", y = "Density") +
  theme_bw(base_family = "Avenir", base_size = 12)

ex3 <- data.frame(val = rexp(1000, .80))
ex3Plot <-  ggplot(ex3, aes(x=val)) + 
  geom_density(col = "black", fill = "cornflowerblue", alpha = .25) +
  ggtitle(expression(paste("Exponential distribution with ", lambda, " = 0.80"))) +
  labs(x = "X", y = "Density") +
  theme_bw(base_family = "Avenir", base_size = 12)

ex4 <- data.frame(val = rexp(1000, .67))
ex4Plot <-  ggplot(ex4, aes(x=val)) + 
  geom_density(col = "black", fill = "cornflowerblue", alpha = .25) +
  ggtitle(expression(paste("Exponential distribution with ", lambda, " = 0.67"))) +
  labs(x = "X", y = "Density") +
  theme_bw(base_family = "Avenir", base_size = 12)

grid.arrange(ex1Plot, ex2Plot, ex3Plot, ex4Plot)

simulations <- data.frame(t(replicate(1000, rexp(40, .2)))) %>%
  mutate(mean = rowMeans(.), flux = sqrt(40) * (mean - 5))

ggplot(simulations, aes(x=mean)) +
  ggtitle("Histogram of Sample Means\n") +
  labs(x = "Mean of 40 Random Exponentials", y = "Frequency") +
  geom_histogram(col="black", fill="cornflowerblue", alpha=.5, binwidth=.25) +
  geom_vline(aes(xintercept = mean(mean), color = "Theoretical"), show.legend = TRUE) +
  scale_colour_manual(name = "Legend", values = c(Theoretical = "firebrick")) +
  theme_bw(base_family = "Avenir", base_size = 12)

sampleMean <- mean(simulations$mean)

ggplot(simulations, aes(x=var)) +
  ggtitle("Histogram of Sample Variances\n") +
  labs(x = "Variances of 40 Random Exponentials", y = "Frequency") +
  geom_histogram(col="black", fill="cornflowerblue", alpha=.5, binwidth=5) +
  geom_vline(aes(xintercept = mean(var), color = "Sample"), show.legend = TRUE) +
  scale_colour_manual(name = "Legend", values = c(Sample = "firebrick")) +
  theme_bw(base_family = "Avenir", base_size = 12)

var(simulations$mean)

