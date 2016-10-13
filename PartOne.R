library(ggplot2)
library(gridExtra)

set.seed(13031)

ex1 <- data.frame(val = rexp(1000, 1.33))
ex1Plot <-  ggplot(ex1, aes(x=val)) + 
  geom_density(col = "black", fill = "cornflowerblue", alpha = .25) +
  ggtitle(expression(paste("Exponential distribution with ", lambda, "=1.33"))) +
  labs(x = "X", y = "Density") +
  theme_bw(base_family = "Avenir", base_size = 12)

ex2 <- data.frame(val = rexp(1000, 1))
ex2Plot <-  ggplot(ex2, aes(x=val)) + 
  geom_density(col = "black", fill = "cornflowerblue", alpha = .25) +
  ggtitle(expression(paste("Exponential distribution with ", lambda, "=1"))) +
  labs(x = "X", y = "Density") +
  theme_bw(base_family = "Avenir", base_size = 12)

ex3 <- data.frame(val = rexp(1000, .8))
ex3Plot <-  ggplot(ex3, aes(x=val)) + 
  geom_density(col = "black", fill = "cornflowerblue", alpha = .25) +
  ggtitle(expression(paste("Exponential distribution with ", lambda, "=0.8"))) +
  labs(x = "X", y = "Density") +
  theme_bw(base_family = "Avenir", base_size = 12)

ex4 <- data.frame(val = rexp(1000, .67))
ex4Plot <-  ggplot(ex4, aes(x=val)) + 
  geom_density(col = "black", fill = "cornflowerblue", alpha = .25) +
  ggtitle(expression(paste("Exponential distribution with ", lambda, "=0.67"))) +
  labs(x = "X", y = "Density") +
  theme_bw(base_family = "Avenir", base_size = 12)

grid.arrange(ex1Plot, ex2Plot, ex3Plot, ex4Plot)

