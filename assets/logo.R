

## Logo AR


# Rdge regression
library(ggplot2)
library(dplyr)
library(mvtnorm)
library(RColorBrewer)

set.seed(11)
sigma2 <- matrix(c(3, 3.5,
              3.5, 5), 
            nrow = 2, ncol = 2, byrow = TRUE)

mu2 <- c(X = 2.5, Y = 5.7)

s.mvn <- mvtnorm::rmvnorm(1000, mean = mu2, sigma = sigma2)
s.mvn <- s.mvn %>% as.data.frame()


data.grid <- expand.grid(s.1 = seq(-5, 8, length.out=200), s.2 = seq(-5, 12, length.out=200))
q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mu2, sigma = sigma2))
blues <- brewer.pal(n = 9, name = "Blues")
blues <- c("#FFFFFF", blues)
# blues <- c(adjustcolor( "white", alpha.f = 0.01), blues)


ggplot() + 
  # geom_contour(data=q.samp, aes(x=s.1,y=s.2,z=prob), bins = 4, lwd = 1, col = "plum2", alpha = .7) + 
  geom_contour_filled(data=q.samp, aes(x=s.1,y=s.2,z=prob)) +
  scale_fill_manual(values = blues) + 
  # geom_density_2d_filled(data = s.mvn, aes(x=X, y=Y), alpha = 0.95) + 
  geom_point(aes(x=0, y=0), col = "orange", pch = 18, cex = 80, alpha = 0.5) + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  xlim(c(-3, 8)) + 
  ylim(c(-3, 11)) + 
  stat_ellipse(level = 0.9) +
  stat_ellipse(level = 0.95, color = 2) +
  stat_ellipse(level = 0.99, color = 3) + 
  # geom_point(aes(x=mu2[1], y=mu2[2]), cex = 2) + 
  theme_void() + 
  theme(legend.position = "none")

ggsave('assets/logo.png', width = 7)


# basis function
  
x <- seq(from  = -2, to = 2, length.out = 1000)
y1 <- dnorm(x = x, mean = -1, sd = 0.5)
y2 <- dnorm(x = x, mean = 0, sd = 0.5)
y3 <- dnorm(x = x, mean = 1, sd = 0.5)
y4 <- -0.342*y1 + 0.384*y2 + 3*y3

data.frame(
  x=rep(x, times = 4), 
  y = c(y1, y2, y3, y4), 
  z = rep(c("basis1", "basis2", "basis3", "res"), each = length(x))
) -> dat2plot

ggplot() + 
  geom_line(data = dat2plot %>% dplyr::filter(!z %in% "res"), aes(x=x, y=y, col = z), lwd = 0.9) + 
  # scale_color_viridis_d(option = "H", begin = 0.6, end = 0.9) + 
  # scale_color_viridis_d(option = "B", begin = 0.6, end = 0.9) + 
  scale_color_viridis_d(option = "G", begin = 0.5, end = 0.9) + 
  geom_line(data = dat2plot %>% dplyr::filter(z %in% "res"), aes(x=x, y=y), lwd = 1.3, col = "red3") + 
  theme_void() + 
  theme(legend.position = "none")







##
##

