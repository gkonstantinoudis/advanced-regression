
library(dplyr)
library(ggplot2)
library(patchwork)
library(splines)
library(blmeco) # package to calculate waic
library(mgcv)
library(dlnm)

triceps <- read.csv("E:/Postdoc Imperial/Lectures/2023_AdvancedRegression/AdvancedRegression2022-2023/data/triceps/triceps.csv")

# Question 1.1
triceps %>% head()
triceps %>% pull(triceps) %>% hist()
lm(triceps~age, data = triceps) -> mod_linear
mod_linear %>% summary()
mod_linear %>% confint()

For each additional year in the age there is a 0.2 (0.20, 0.24) cm increase in the triceps skinfold thickness

# Question 1.2
The main assumption is that the relationship between triceps skinfold thickness and age is linear. We can plot them
in a scatterplot to see if this assumption is valid.

ggplot() + geom_point(data = triceps, aes(x = age, y = triceps), cex = 1.2) + theme_bw()

Having plotted the results, the main drawback of the approach in question 1.1 is that it misses the initial declining trend
for the first 10 years of life.

# Question 1.3
mod_po2 <- lm(triceps~age+I(age^2), data = triceps)
mod_po3 <- lm(triceps~age+I(age^2)+I(age^3), data = triceps)
mod_po4 <- lm(triceps~age+I(age^2)+I(age^3)+I(age^4), data = triceps)

data.frame(x=rep(triceps$age, times = 3),
           fitted=c(mod_po2$fitted.values, mod_po3$fitted.values, mod_po4$fitted.values),
           pol = rep(c("Order 2", "Order 3", "Order 4"), each = mod_po2$fitted.values %>% length())) %>%
  ggplot() + geom_point(data = triceps, aes(x = age, y = triceps), cex = 1.2) +
  geom_line(aes(x = x, y = fitted, col = pol), cex = 1.2) +
  theme_bw()

It is interesting that the mod_po2 gives zero coefficient to the $x^2$, with the effect being very similar with the linear approach.
mod_po2 %>% summary()

anova(mod_po2, mod_po3)
anova(mod_po3, mod_po4)
anova(mod_po2, mod_po4)

The likelihood ratio test shows the need of a more flexible model, with degree 4 polynomial.

# Question 1.4
predict(mod_po4, newdata = data.frame(age = triceps$age), se.fit = TRUE) -> pred_p

data.frame(x = triceps$age,
           mean = pred_p$fit,
           UL = pred_p$fit + 1.96*pred_p$se.fit,
           LL = pred_p$fit - 1.96*pred_p$se.fit
           ) %>%
  ggplot() + geom_point(data = triceps, aes(x = age, y = triceps), cex = 1.2) +
  geom_line(aes(x = x, y = mean), cex = 1.2) +
  geom_ribbon(aes(x = x, ymin = LL, ymax = UL), fill = "blue", alpha = 0.2) +
  theme_bw()


# Question 1.5
omega <- 2*pi/25
mod_fourier_p25 <- lm(triceps~I(sin(omega*age))+I(cos(omega*age)), data = triceps)
omega <- 2*pi/50
mod_fourier_p50 <- lm(triceps~I(sin(omega*age))+I(cos(omega*age)), data = triceps)
omega <- 2*pi/100
mod_fourier_p100 <- lm(triceps~I(sin(omega*age))+I(cos(omega*age)), data = triceps)


data.frame(x=rep(triceps$age, times = 3),
           fitted=c(mod_fourier_p25$fitted.values, mod_fourier_p50$fitted.values, mod_fourier_p100$fitted.values),
           pol = rep(c("P=25", "P=50", "P100"), each = mod_fourier_p25$fitted.values %>% length())) %>%
  ggplot() + geom_point(data = triceps, aes(x = age, y = triceps), cex = 1.2) +
  geom_line(aes(x = x, y = fitted, col = pol), cex = 1.2) +
  theme_bw()



##
##

# Question 2.1

mod_lt_1 <- lm(triceps~age:(age<10), data = triceps)
mod_lt_1 %>% summary()

predict(mod_lt_1, newdata = data.frame(age = triceps$age), se.fit = TRUE) -> pred_lt_1

data.frame(x = triceps$age,
           mean = pred_lt_1$fit,
           UL = pred_lt_1$fit + 1.96*pred_lt_1$se.fit,
           LL = pred_lt_1$fit - 1.96*pred_lt_1$se.fit,
           disc = ifelse(triceps$age<10, 1, 2)) %>%
  ggplot() + geom_point(data = triceps, aes(x = age, y = triceps), cex = .7, alpha = 0.2) +
  geom_line(aes(x = x, y = mean, group = disc), cex = 1.2) +
  geom_ribbon(aes(x = x, ymin = LL, ymax = UL), fill = "blue", alpha = 0.2) +
  theme_bw() -> p1

I will do the same for the 2 thresholds

mod_lt_2 <- lm(triceps~age:(age<10)+age:(age<40), data = triceps)

predict(mod_lt_2, newdata = data.frame(age = triceps$age), se.fit = TRUE) -> pred_lt_2

data.frame(x = triceps$age,
           mean = pred_lt_2$fit,
           UL = pred_lt_2$fit + 1.96*pred_lt_2$se.fit,
           LL = pred_lt_2$fit - 1.96*pred_lt_2$se.fit,
           disc = case_when(triceps$age<10~1,
                            triceps$age>=10 & triceps$age<40~2,
                            triceps$age>=40~3)) %>%
  ggplot() + geom_point(data = triceps, aes(x = age, y = triceps), cex = .7, alpha = 0.2) +
  geom_line(aes(x = x, y = mean, group = disc), cex = 1.2) +
  geom_ribbon(aes(x = x, ymin = LL, ymax = UL), fill = "blue", alpha = 0.2) +
  theme_bw() -> p2


p1|p2

mod_lt_2 %>% summary()
mod_lt_2 %>% confint()

For the model with the 3 thresholds: For every year increase in the age, for people aged less than 10 years old, there weak evidence of a
0.19 cm decrease in the triceps skinfold thickness, for people aged between 10 and 40 years old, for every year increase
in the age, there is a 0.17 cm increase in the triceps skinfold thickness and for people 40 years old or older, for every year increase
in the age, there is a 0.04 cm increase in the triceps skinfold thickness.

The discontinuity on the threshold is a limitation of this approach, because it does not allow us to investigate the natural progress of the process.


# Question 2.2

mod_lsplines <- lm(triceps~age + I((age-10)*(age<10)), data = triceps)

predict(mod_lsplines, newdata = data.frame(age = triceps$age), se.fit = TRUE) -> pred_lsplines

data.frame(x = triceps$age,
           mean = pred_lsplines$fit,
           UL = pred_lsplines$fit + 1.96*pred_lsplines$se.fit,
           LL = pred_lsplines$fit - 1.96*pred_lsplines$se.fit) %>%
  ggplot() + geom_point(data = triceps, aes(x = age, y = triceps), cex = .7, alpha = 0.2) +
  geom_line(aes(x = x, y = mean), cex = 1.2) +
  geom_ribbon(aes(x = x, ymin = LL, ymax = UL), fill = "blue", alpha = 0.2) +
  theme_bw()

mod_lsplines %>% summary()
mod_lsplines %>% confint()

For people aged between 10 years or older for every year increase
in the age there is a 0.26 cm increase in the triceps skinfold thickness. For every year increase in the age,
for people aged less than 10 years old, there is a  -0.306+0.264 = -0.042 cm change in the in the triceps skinfold thickness


# Question 2.3

kn <- c(5, 10, 15)
list_res <- pred_res <- ci_graph <- list()
for(i in 1:3){
  list_res[[i]] <- lm(triceps~bs(age, degree=1, knots=kn[i]), data = triceps)
  pred_res[[i]] <- predict(list_res[[i]], newdata = data.frame(age = triceps$age), se.fit = TRUE)

  ci_graph[[i]] <- data.frame(
    est = pred_res[[i]]$fit,
    UL = pred_res[[i]]$fit + 1.96*pred_res[[i]]$se.fit,
    LL = pred_res[[i]]$fit - 1.96*pred_res[[i]]$se.fit,
    thr = kn[i]
  )
}

ci <- do.call(rbind, ci_graph)
ci$age <- rep(triceps$age, times = 3)
ggplot(data = ci) + geom_point(data = triceps, aes(x = age, y = triceps), cex = .7, alpha = 0.2) +
  geom_line(aes(x = age, y = est), cex = 1.2) +
  geom_ribbon(aes(x = age, ymin = LL, ymax = UL), fill = "blue", alpha = 0.2) +
  facet_grid(cols = vars(thr)) + theme_bw()

The main problem with these models is that for different threshold they results a different fit. This threshold specific
volatility makes them not very attractive to use when we have no information about the threshold. We can use criteria such as
the waic to get an indication about better fits.

lapply(list_res, WAIC)

with the smallest being for a knot at 5 years.


# Question 2.4
mod_lsplines %>% summary()
list_res[[2]] %>% summary()


# Question 2.5

b <- bs(triceps$age, degree = 1, knots = 10)
str(b)
b1 <- b[, 1]  ## basis 1
b2 <- b[, 2]  ## basis 2

data.frame(x=rep(triceps$age, times=2),
           b=c(b1, b2),
           basis=rep(c("basis 1", "basis 2"), each = x %>% length())) %>%
  ggplot() + geom_line(aes(x=x, y=b), cex=.8) + facet_grid(cols=vars(basis)) + theme_bw()



lm(triceps$triceps~b1+b2) %>% summary()
list_res[[2]] %>% summary()

The values of the coefficients in the bs version of the call are essentially scalling factors, that calibrate the
basis function. What we are really interested in is the combination of those to get the overall fit.


devtools::install_github("ZheyuanLi/SplinesUtils")
library(SplinesUtils)
RegSplineAsPiecePoly(list_res[[2]], "bs(age, degree = 1, knots = kn[i])", shift = FALSE)

The interpretation now is the natural one without needing any reparametrisation: For every year increase in the age,
for people aged less than 10 years old, there is a -0.0412 cm change in the in the triceps skinfold thickness. For every year increase in
the age, for people older than 10 years old, there is a  0.265 cm increase in the in the triceps skinfold thickness.



##
##

# Question 3.1

b <- bs(triceps$age, degree = 3, knots = 10)
str(b)
b1 <- b[, 1]  ## basis 1
b2 <- b[, 2]  ## basis 2
b3 <- b[, 3]  ## basis 3
b4 <- b[, 4]  ## basis 4

data.frame(x=rep(triceps$age, times=4),
           b=c(b1, b2, b3, b4),
           basis=rep(c("basis 1", "basis 2", "basis 3", "basis 4"), each = x %>% length())) %>%
  ggplot() + geom_line(aes(x=x, y=b), cex=.8) + facet_grid(cols=vars(basis)) + theme_bw()


# Question 3.2
mod_bs_3_splines <- lm(triceps~bs(age, knots = 10, degree = 3), data = triceps)
mod_bs_3_splines %>% summary()


The coefficients scale the evaluated B-spline basis functions according to the data in order to fit the curve.

# Question 3.3

x2pred <- seq(from = 1, to = 50, length.out = 200)
predict(mod_bs_3_splines, newdata = data.frame(age=x2pred), se.fit = TRUE) -> pred_bs_3_splines

data.frame(
  x=x2pred,
  est=pred_bs_3_splines$fit,
  UL=pred_bs_3_splines$fit+1.96*pred_bs_3_splines$se.fit,
  LL=pred_bs_3_splines$fit-1.96*pred_bs_3_splines$se.fit
) %>%
  ggplot() + geom_point(data = triceps, aes(x = age, y = triceps), cex = .7, alpha = 0.2) +
  geom_line(aes(x = x, y = est), cex = 1.2) +
  geom_ribbon(aes(x = x, ymin = LL, ymax = UL), fill = "blue", alpha = 0.2) + theme_bw() -> p1


# Question 3.4
mod_ns_3_splines <- lm(triceps~ns(age, knots = c(1, 10, 50)), data = triceps)
mod_ns_3_splines %>% summary()

predict(mod_ns_3_splines, newdata = data.frame(age=x2pred), se.fit = TRUE) -> pred_ns_3_splines

data.frame(
  x=x2pred,
  est=pred_ns_3_splines$fit,
  UL=pred_ns_3_splines$fit+1.96*pred_ns_3_splines$se.fit,
  LL=pred_ns_3_splines$fit-1.96*pred_ns_3_splines$se.fit
) %>%
  ggplot() + geom_point(data = triceps, aes(x = age, y = triceps), cex = .7, alpha = 0.2) +
  geom_line(aes(x = x, y = est), cex = 1.2) +
  geom_ribbon(aes(x = x, ymin = LL, ymax = UL), fill = "blue", alpha = 0.2) + theme_bw() -> p2


p1|p2

The behaviour of ns in the boundaries is less wiggly, because it assumes linearity. Here is more prominent on the
max boundary.


# Question 3.5

lapply(c(1, 10, 50, 100), function(X){
  ggplot() + geom_point(data = triceps, aes(x = age, y = triceps), cex = .7, alpha = 0.2) +
    geom_line(aes(x = triceps$age, y = lm(triceps~ns(age, df=X), data = triceps)$fitted.values), cex = 1.2) +
    theme_bw() + ggtitle(paste("df=",X, sep = " ")) %>% return()
}) -> p_ns

(p_ns[[1]]|p_ns[[2]])/(p_ns[[3]]|p_ns[[4]])

As we increase the degrees of freedom, them bias decreases, whereas as the degrees of freedom decrease the bias increases whereas
the variance decreases.

# Question 3.6
mgcv::gam(triceps~s(age), family = "gaussian", data = triceps) -> mod_psplines
plot(mod_psplines)

# or

predict (mod_psplines, data.frame(age = triceps$age), se.fit = TRUE) -> pred_splines
UL <- pred_splines$fit + 1.96*pred_splines$se.fit
LL <- pred_splines$fit - 1.96*pred_splines$se.fit

ggplot( ) +
  geom_point(data = triceps, aes(x=age, y=triceps), cex = 1) +
  geom_line(data = triceps, aes(x=age, y=pred_splines$fit) , linewidth = .5) +
  geom_ribbon(aes(ymin=LL, ymax=UL, x=triceps$age), fill="red", alpha=0.2) + theme_bw()


It looks that the penalised spline provides a very good fit to the data, capturing all the main features/trends
while at the same time not being very variable.

##
##

hanes <- read.csv("E:/Postdoc Imperial/Lectures/2023_AdvancedRegression/AdvancedRegression2022-2023/data/hanes.csv")
hanes %>% head()

# Question 4.1
glm(d.total~age+factor(sex)+factor(race)+factor(booze)+factor(smokever)+bp1sys, family = "binomial", data = hanes) -> mod1
mod1 %>% summary()
coefficients(mod1)["bp1sys"] %>% exp()
(mod1 %>% confint() %>% exp())*100


After accounting for age, sex, ethnicity alcohol consumption and smoking we have observed a 0.8% (0.4-1.24%) increase in the odds for dying
for every unit increase in the systolic blood pressure.


# Question 4.2
glm(d.total~age+factor(sex)*bp1sys + factor(race)+factor(booze)+factor(smokever), family = "binomial", data = hanes) -> mod2
mod2 %>% summary()

There is weak evidence of an interaction, implying that after accounting for age, sex, ethnicity alcohol consumption and smoking
sex does not modify systolic blood pressure associated mortality risk.

anova(mod1, mod2)
The likelihood ratio test also argues that there is weak evidence supporting an interaction term.

# Question 4.3
mgcv::gam(d.total~s(age)+factor(sex) + bp1sys + factor(race)+factor(booze)+factor(smokever), family = "binomial", data = hanes) -> mod3
mod3 %>% summary()

plot(mod3)

# or
summary(hanes)
predict(mod3, hanes %>%
          dplyr::select(age, sex, bp1sys, race, booze, smokever) %>%
          mutate(sex=0, bp1sys=mean(bp1sys, na.rm = TRUE), race=1, booze=0, smokever=0), se.fit = TRUE) -> pred_mod3
UL <- pred_mod3$fit + 1.96*pred_mod3$se.fit
LL <- pred_mod3$fit - 1.96*pred_mod3$se.fit

ggplot() +
  geom_line(data=hanes, aes(x=age, y=pred_mod3$fit)) +
  geom_ribbon(data=hanes, aes(x=age, ymin=LL, ymax=UL) , fill=" blue", alpha =0.1) +
  theme_bw() + ylab("log odds")

The effect looks reasonably linear.

# Question 4.4
mgcv::gam(d.total~age+factor(sex) + bp1sys + factor(race)+factor(booze)+factor(smokever)+s(ser.chol), family = "binomial", data = hanes) -> mod4
mod4 %>% summary()

plot(mod4)

It looks that the serum cholesterol needs a more flexible fit, but overall the effect seems to be around 0.


# Question 4.5
mgcv::gam(d.heart ~age+factor(sex) + bp1sys + factor(race)+factor(booze)+factor(smokever)+s(ser.chol), family = "binomial", data = hanes) -> mod5
mod5 %>% summary()

plot(mod5)

The effect here seems to be linear. For parsirmonity reasons we will use a linear term instead
mgcv::gam(d.heart ~age+factor(sex) + bp1sys + factor(race)+factor(booze)+factor(smokever)+ser.chol, family = "binomial", data = hanes) %>%
  summary()


##
##


# Question 5.1
There is no universal definition of the heatwave. The three major components for defining a heatwave is a
temperature metric (min, mean, max), the intensity (a threshold once reached implying extreme heat) and the
duration (day for which the temperature with excess the above-mentioned threshold).


# Question 5.2

chicagoNMMAPS %>% head()

threshold <- quantile(chicagoNMMAPS$temp, probs = 0.95)
chicagoNMMAPS %>%
  dplyr::mutate(
    heatwave = dplyr::case_when(temp >= threshold &
                                dplyr::lag(temp, 1) >= threshold &
                                dplyr::lag(temp, 2) >= threshold ~ 1,
                                TRUE ~ 0)
  ) -> chicagoNMMAPS

chicagoNMMAPS %>%
  dplyr::mutate(
    heatwave = dplyr::case_when(heatwave == 1 |
                                  dplyr::lead(heatwave, 1) == 1 |
                                  dplyr::lead(heatwave, 2) == 1 ~ 1,
                                TRUE ~ 0)
    ) -> chicagoNMMAPS


mgcv::gam(resp ~ s(time) + s(month) + dow + pm10 + factor(heatwave),
              data = chicagoNMMAPS , family = "poisson" ) -> mod_heatwave1

mod_heatwave1 %>% summary()


(exp(0.11 - 1.96*0.03) - 1)*100; (exp(0.11 + 1.96*0.03) - 1)*100

There is a 5-18% increase in the respiratory mortality risk during heatwave days, after accoutning for day of week, temporal trends and PM10.


# Question 5.3

k <- 1:10
res_store <- list()

for(i in 1:length(k)){

  if(i==1){
    chicagoNMMAPS %>%
      dplyr::mutate(
        heatwave_lagged = dplyr::case_when(heatwave == 1 |
                                             dplyr::lead(heatwave, 1) == 1 ~ 1,
                                           TRUE ~ 0)
      ) -> chicagoNMMAPS
  }else{
    chicagoNMMAPS %>%
      dplyr::mutate(
        heatwave_lagged = dplyr::case_when(heatwave_lagged == 1 |
                                             dplyr::lead(heatwave_lagged, 1) == 1 ~ 1,
                                           TRUE ~ 0)
      ) -> chicagoNMMAPS
  }


  mgcv::gam(resp ~ s(time) + s(month) + dow + pm10 + factor(heatwave_lagged),
            data = chicagoNMMAPS , family = "poisson" )  -> tmp

  res_store[[i]] <- list(est = (exp(coef(tmp)["factor(heatwave_lagged)1"])-1)*100,
                         LL = (exp(coef(tmp)["factor(heatwave_lagged)1"] - 1.96*summary(tmp)$se["factor(heatwave_lagged)1"])-1)*100,
                         UL = (exp(coef(tmp)["factor(heatwave_lagged)1"] + 1.96*summary(tmp)$se["factor(heatwave_lagged)1"])-1)*100)
}




lapply(res_store, unlist) %>% do.call(rbind, .) %>% as_tibble() %>% mutate(type =
                                                                             factor(paste0("lag ", k),
                                                                                    levels = paste0("lag ",k))) -> plotres

colnames(plotres)[1:3] <- c("est", "LL", "UL")

ggplot(data = plotres) +
  geom_point(aes(x=type, y=est)) +
  geom_errorbar(aes(x=type, ymin=LL, ymax=UL, width = 0.1)) +
  geom_hline(yintercept = 0, col = "red", linetype = "dotted") + theme_bw() + ylab("% increase in respiratory mortality") + xlab("Lags") -> p1


ggplot(data = plotres) +
  geom_point(aes(x=type, y=est)) +
  geom_line(aes(x=type, y=est, group=1)) +
  geom_ribbon(aes(x=type, ymin=LL, ymax=UL, group = 1), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 0, col = "red", linetype = "dotted") + theme_bw() + ylab("% increase in respiratory mortality") + xlab("Lags") -> p2

p1/p2

The effect of heatwave on the lag dimention looks linear. There evidence of an effect at lag 10 is weak.


# Question 5.4
cb.heatwave <- crossbasis(chicagoNMMAPS$temp, lag=10, argvar=list(fun="thr", thr = threshold), arglag=list(fun="lin"))
cb.heatwave %>% summary()

mgcv::gam(resp ~ s(time) + s(month) + dow + pm10 + cb.heatwave,
          data = chicagoNMMAPS, family = "poisson")  -> dlnm_heatwave_model

dlnm_heatwave_model %>% summary()
(exp(0.015)-1)*100
Overall and across all lags considered there is a 1.5% increase in the respiratory mortality risk for every 1oC increase in temperatures higher than the 95th
percentile of the temperature.

There is a declining trend of the effect of temperatures higher than the 95th percentile of the temperature across the lags with the log effect to reduce by
-0.002 for every subbsequent lag (0.2% decrease in the % respiratory mortality risk scale).


The heatwave estimate differs from this, as the heatwave shows a risk during heatwave days (a period of extreme heat), wheres the current estimate gives
increase in risk for every 1oC increase in the temperature.

# Question 5.5

pred.hw <- crosspred(cb.heatwave, dlnm_heatwave_model, at=-20:30, bylag=1)

pred.hw$allRRfit["30"]
pred.hw$allRRlow["30"]
pred.hw$allRRhigh["30"]

Relative to temperatures lower than the 95th percentile of the temperature, there is a 20% (2%, 43%) increased respiratory mortality risk across
the 0-10 lags when the temperature is 30oC.


# Question 5.6
plot(pred.hw, theta=150, phi=10, lphi=100, xlab = "Temperature", zlab="RR")
plot(pred.hw, "slice", lag=1)
plot(pred.hw, "slice", var=30)


library(plotly)
p <- plot_ly()
p <- add_surface(p,
               x = 0:10, # the lags
               y = -20:30, # the temperature as is on the pred.hw$matRRfit defined by at
               z = pred.hw$matRRfit)

layout(p, scene = list(xaxis = list(title = "Lag"),
                      yaxis = list(title = "Temperature", range = c(0,30)),
                      zaxis = list(title = "Relative risk"))
)



install.packages("tinytex")
webshot::install_phantomjs()
