# packages

library(ggplot2)
library(dplyr)
library(mvtnorm)
library(RColorBrewer)
library(ggrepel)
library(pBrackets)
library(patchwork)
library(ggforce)

bracketsGrob <- function(...) {
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(
    {
      do.call(grid.brackets, l)
    },
    e
  )
}


##
## Lasso

sigma2 <- matrix(
  c(
    3, 3.5,
    3.5, 5
  ),
  nrow = 2, ncol = 2, byrow = TRUE
)

mu2 <- c(X = 2.5, Y = 5.7)

lambda <- 2.5

data.frame(
  x = c(lambda, 0, -lambda, 0),
  y = c(0, -lambda, 0, lambda)
) -> datlambda


data.grid <- expand.grid(s.1 = seq(-5, 8, length.out = 200), s.2 = seq(-5, 12, length.out = 200))
q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mu2, sigma = sigma2))
blues <- brewer.pal(n = 9, name = "Blues")
blues <- c("#FFFFFF", blues)


br1 <- bracketsGrob(x1 = -0.4, y1 = 0.15, x2 = -0.4, y2 = 0.8, h = 0.1, lwd = 1.5)

ggplot() +
  geom_contour_filled(data = q.samp, aes(x = s.1, y = s.2, z = prob)) +
  scale_fill_manual(values = blues) +
  geom_polygon(data = datlambda, aes(x = x, y = y), col = NA, fill = "orange", alpha = 0.5) +
  xlim(c(-3, 8)) +
  ylim(c(-3, 11)) +
  stat_ellipse(level = 0.9) +
  stat_ellipse(level = 0.95, color = 2) +
  stat_ellipse(level = 0.99, color = 3) +
  geom_point(aes(x = mu2[1], y = mu2[2]), cex = 2, col = "red") +
  geom_point(aes(x = 0, y = 2.5), cex = 2, col = "red") +
  theme_void() +
  theme(legend.position = "none") +
  geom_segment(aes(x = 0, y = 2.5, xend = 2, yend = 2.5),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_segment(aes(x = 2.5, y = 5.7, xend = 4.5, yend = 5.7),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    geom = "text", x = 5, y = 5.7, label = expression(hat(beta)[OLS]),
    color = "black"
  ) +
  annotate(
    geom = "text", x = 2.5, y = 2.5, label = expression(hat(beta)[lasso]),
    color = "black"
  ) +
  annotate(
    geom = "text", x = -0.5, y = 10, label = expression(beta[1]),
    color = "black"
  ) +
  annotate(
    geom = "text", x = 7, y = -.5, label = expression(beta[2]),
    color = "black"
  ) +
  annotate(
    geom = "text", x = 2, y = -2, label = expression(L[1] ~ ": |" ~ beta[1] ~ "|" ~ +~"|" ~ beta[2] ~ "| <" ~ lambda),
    color = "black"
  ) +
  annotate(
    geom = "text", x = -.5, y = 1.3, label = expression(lambda),
    color = "black"
  ) +
  # annotate(geom="text", x=-.5, y=-1.3, label=expression(lambda),
  #          color="black") +
  geom_segment(aes(x = 0, y = -3, xend = 0, yend = 10.8),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_segment(aes(x = -3, y = 0, xend = 8, yend = 0),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotation_custom(br1, xmin = 0.5, xmax = 2, ymin = -0.35, ymax = 3) -> p1


##
## Ridge

sigma2 <- matrix(
  c(
    3, 3.5,
    3.5, 5
  ),
  nrow = 2, ncol = 2, byrow = TRUE
)

mu2 <- c(X = 2.5, Y = 5.7)


data.grid <- expand.grid(s.1 = seq(-5, 8, length.out = 200), s.2 = seq(-5, 12, length.out = 200))
q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mu2, sigma = sigma2))
blues <- brewer.pal(n = 9, name = "Blues")
blues <- c("#FFFFFF", blues)


ggplot() +
  geom_contour_filled(data = q.samp, aes(x = s.1, y = s.2, z = prob)) +
  scale_fill_manual(values = blues) +
  geom_circle(aes(x0 = 0, y0 = 0, r = lambda), col = NA, fill = "orange", alpha = 0.5) +
  xlim(c(-3, 8)) +
  ylim(c(-3, 11)) +
  stat_ellipse(level = 0.9) +
  stat_ellipse(level = 0.95, color = 2) +
  stat_ellipse(level = 0.99, color = 3) +
  geom_point(aes(x = mu2[1], y = mu2[2]), cex = 2, col = "red") +
  geom_point(aes(x = 0.2, y = 2.45), cex = 2, col = "red") +
  theme_void() +
  theme(legend.position = "none") +
  geom_segment(aes(x = 0.2, y = 2.45, xend = 3, yend = 2.45),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_segment(aes(x = 2.5, y = 5.7, xend = 4.5, yend = 5.7),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    geom = "text", x = 5, y = 5.7, label = expression(hat(beta)[OLS]),
    color = "black"
  ) +
  annotate(
    geom = "text", x = 3.5, y = 2.45, label = expression(hat(beta)[ridge]),
    color = "black"
  ) +
  annotate(
    geom = "text", x = -0.5, y = 10, label = expression(beta[1]),
    color = "black"
  ) +
  annotate(
    geom = "text", x = 7, y = -.5, label = expression(beta[2]),
    color = "black"
  ) +
  annotate(
    geom = "text", x = 2, y = -2, label = expression(L[2] ~ ": " ~ beta[1]^2 ~ +~ beta[2]^2 ~ " <" ~ lambda),
    color = "black"
  ) +
  annotate(
    geom = "text", x = -.6, y = 1.3, label = expression(sqrt(lambda)),
    color = "black"
  ) +
  geom_segment(aes(x = 0, y = -3, xend = 0, yend = 10.8),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_segment(aes(x = -3, y = 0, xend = 8, yend = 0),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotation_custom(br1, xmin = 0.5, xmax = 2, ymin = -0.35, ymax = 2.8)

p1 | p2



##
## Elastic net

# the elastic net penalty can be seen as (1-a)*|beta| + a*beta^2 < lambda
# we can fix alpha for now to 0.5
beta_2 <- seq(from = -2.5, to = 2.5, length.out = 1000)
alpha <- 0.51
lambda <- 2.5

get_en <- function(alpha, beta_2, lambda) {
  A <- 1 - 2 * alpha
  B <- 2 * (1 - alpha) * (abs(beta_2) - alpha * abs(beta_2) - lambda)
  C <- (1 - 2 * alpha) * beta_2^2 - 2 * lambda * (1 - alpha) * abs(beta_2) + lambda^2
  D <- B^2 - 4 * A * C

  beta13 <- (-B - sqrt(D)) / (2 * A)
  beta14 <- -(-B - sqrt(D)) / (2 * A)

  return(list(beta13, beta14))
}


dat.plot1 <-
  data.frame(
    beta_1 = get_en(alpha = 0.51, lambda = 2.5, beta_2 = beta_2) %>% unlist(),
    beta_2 = c(beta_2, beta_2)
  )
dat.plot1 <- dat.plot1[!is.nan(dat.plot$beta_1), ]


dat.plot2 <-
  data.frame(
    beta_1 = get_en(alpha = 0, lambda = 2.5, beta_2 = beta_2) %>% unlist(),
    beta_2 = c(beta_2, beta_2)
  )
dat.plot2 <- dat.plot2[!is.nan(dat.plot2$beta_1), ]


dat.plot3 <-
  data.frame(
    beta_1 = get_en(alpha = 1, lambda = 2.5, beta_2 = beta_2) %>% unlist(),
    beta_2 = c(beta_2, beta_2)
  )
dat.plot3 <- dat.plot3[!is.nan(dat.plot3$beta_1), ]



ggplot() +
  geom_contour_filled(data = q.samp, aes(x = s.1, y = s.2, z = prob)) +
  scale_fill_manual(values = blues) +
  geom_path(data = dat.plot1, aes(x = beta_1, y = beta_2), col = "blue4", lwd = 1) +
  geom_path(data = dat.plot2, aes(x = beta_1, y = beta_2), lty = "dashed", col = "blue4") +
  geom_path(data = dat.plot3, aes(x = beta_1, y = beta_2), lty = "dotted", col = "blue4") +
  # geom_vline(xintercept = 0) +
  # geom_hline(yintercept = 0) +
  xlim(c(-3, 8)) +
  ylim(c(-3, 11)) +
  geom_segment(aes(x = 1.7, y = 1.8, xend = 3.5, yend = 1.8),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_segment(aes(x = 1.9, y = 1, xend = 3.5, yend = 1),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_segment(aes(x = 2.2, y = .3, xend = 3.5, yend = .3),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    geom = "text", x = 5.12, y = 1.85, label = "Ridge penalty (a=1)",
    color = "black"
  ) +
  annotate(
    geom = "text", x = 5.6, y = 1, label = "Elastic net penalty (a=0.5)",
    color = "black"
  ) +
  annotate(
    geom = "text", x = 5.2, y = .35, label = "Lasso penalty (a=0)",
    color = "black"
  ) +
  annotate(
    geom = "text", x = -0.5, y = 10, label = expression(beta[1]),
    color = "black"
  ) +
  annotate(
    geom = "text", x = 7, y = -.5, label = expression(beta[2]),
    color = "black"
  ) +
  geom_segment(aes(x = 0, y = -3, xend = 0, yend = 10.8),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_segment(aes(x = -3, y = 0, xend = 8, yend = 0),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  theme_void() +
  theme(legend.position = "none")
