## ----include=FALSE------------------------------------------------------------
## load packages
library(car)
library(performance)
library(lmtest)
library(DHARMa)
library(patchwork)
library(tidyverse)


## -----------------------------------------------------------------------------
dat <- read.csv("data/lm_example.csv")
str(dat)


## -----------------------------------------------------------------------------
#| eval: false
#| code-line-numbers: "4"
## library(car)
## library(performance)
## library(lmtest)
## library(tidyverse)


## ----plot1, fig.show='hide'---------------------------------------------------
ggplot(data = dat, aes(x = age, y = fklngth)) +
  facet_grid(. ~ locate) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  stat_smooth(se = FALSE, color = "red") +
  labs(
    y = "Fork length",
    x = "Age"
  )


## ----plot1, echo=FALSE, fig.show='show'---------------------------------------
ggplot(data = dat, aes(x = age, y = fklngth)) +
  facet_grid(. ~ locate) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  stat_smooth(se = FALSE, color = "red") +
  labs(
    y = "Fork length",
    x = "Age"
  )


## -----------------------------------------------------------------------------
dat <- dat %>%
  mutate(
    lage = log10(age),
    lfkl = log10(fklngth)
  )


## -----------------------------------------------------------------------------
#| label: l10
#| eval: false
## ggplot(data = dat, aes(x = lage, y = lfkl)) +
##   facet_grid(. ~ locate) +
##   geom_point() +
##   stat_smooth(method = lm, se = FALSE) +
##   stat_smooth(se = FALSE, color = "red") +
##   labs(
##     y = "log 10 Fork length",
##     x = "Log 10 Age"
##   )


## -----------------------------------------------------------------------------
#| label: l10
#| echo: false
ggplot(data = dat, aes(x = lage, y = lfkl)) +
  facet_grid(. ~ locate) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  stat_smooth(se = FALSE, color = "red") +
  labs(
    y = "log 10 Fork length",
    x = "Log 10 Age"
  )


## -----------------------------------------------------------------------------
m1 <- lm(lfkl ~ lage + locate + lage:locate, data = dat)
summary(m1)


## -----------------------------------------------------------------------------
Anova(m1, type = 3)


## -----------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(m1)


## -----------------------------------------------------------------------------
#| eval: false
## check_model(m1)






## -----------------------------------------------------------------------------
shapiro.test(residuals(m1))


## -----------------------------------------------------------------------------
bptest(m1)


## -----------------------------------------------------------------------------
resettest(m1, power = 2:3, type = "fitted", data = dat)


## -----------------------------------------------------------------------------
mouflon0 <- read.csv("data/mouflon.csv")
mouflon <- mouflon0 %>%
  arrange(age) %>%
  mutate(
    reproduction = case_when(
      age >= 13 ~ 0,
      age <= 4 ~ 1,
      .default = reproduction
    )
  )


## -----------------------------------------------------------------------------
#| label: f_plot
#| eval: false
## bubble <- data.frame(
##   age = rep(2:16, 2),
##   reproduction = rep(0:1, each = 15),
##   size = c(table(mouflon$age, mouflon$reproduction))
## ) %>%
##   mutate(size = ifelse(size == 0, NA, size))
## ggplot(
##   bubble,
##   aes(x = age, y = reproduction, size = size)
## ) +
##   geom_point(alpha = 0.8) +
##   scale_size(range = c(.1, 20), name = "Nb individuals")


## -----------------------------------------------------------------------------
#| label: f_plot
#| echo: false
bubble <- data.frame(
  age = rep(2:16, 2),
  reproduction = rep(0:1, each = 15),
  size = c(table(mouflon$age, mouflon$reproduction))
) %>%
  mutate(size = ifelse(size == 0, NA, size))
ggplot(
  bubble,
  aes(x = age, y = reproduction, size = size)
) +
  geom_point(alpha = 0.8) +
  scale_size(range = c(.1, 20), name = "Nb individuals")


## -----------------------------------------------------------------------------
m1 <- glm(reproduction ~ age, data = mouflon,   family = binomial)
summary(m1)


## -----------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m1)
plot(simulationOutput)


## -----------------------------------------------------------------------------
#| output-location: slide
mouflon$logit_ypred <- 3.19921 - 0.36685 * mouflon$age
plot(logit_ypred ~ jitter(age), mouflon)
points(mouflon$age, mouflon$logit_ypred, col = "red", type = "l", lwd = 2)


## -----------------------------------------------------------------------------
#| output-location: slide
mouflon$ypred <- exp(mouflon$logit_ypred) / (1 + exp(mouflon$logit_ypred))
ggplot(mouflon, aes(x = age, y = reproduction)) +
  geom_jitter(height = 0.01) +
  geom_line(aes(y=ypred), color = "red")


## -----------------------------------------------------------------------------
#| label: pred_glm
#| eval: false
## dat_predict <- data.frame(
##   age = seq(min(mouflon$age), max(mouflon$age), length = 100)
## ) %>%
##   mutate(
##     reproduction = predict(m1, type = "response", newdata = .)
##   )
## 
## ggplot(mouflon, aes(x = age, y = reproduction)) +
##   geom_jitter(height = 0.01) +
##   geom_line(data = dat_predict, aes(x = age, y = reproduction), color = "red")


## -----------------------------------------------------------------------------
#| label: pred_glm
#| echo: false
dat_predict <- data.frame(
  age = seq(min(mouflon$age), max(mouflon$age), length = 100)
) %>%
  mutate(
    reproduction = predict(m1, type = "response", newdata = .)
  )

ggplot(mouflon, aes(x = age, y = reproduction)) +
  geom_jitter(height = 0.01) +
  geom_line(data = dat_predict, aes(x = age, y = reproduction), color = "red")


## -----------------------------------------------------------------------------
m2 <- glm(
  reproduction ~ age + mass_sept + as.factor(sex_lamb) +
    mass_gain + density + temp,
  data = mouflon,
  family = binomial
)


## -----------------------------------------------------------------------------
#| eval: false
## check_model(m2)





## -----------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m2)
plot(simulationOutput)


## -----------------------------------------------------------------------------
gala <- read.csv("data/gala.csv")
plot(Species ~ Area, gala)


## -----------------------------------------------------------------------------
plot(Species ~ log(Area), gala)


## -----------------------------------------------------------------------------
hist(gala$Species)


## -----------------------------------------------------------------------------
modpl <- glm(Species ~ Area + Elevation + Nearest, family = poisson, gala)
summary(modpl)


## -----------------------------------------------------------------------------
res <- simulateResiduals(modpl)
testDispersion(res)


## -----------------------------------------------------------------------------
c(mean(gala$Species), var(gala$Species))
par(mfrow = c(1, 2))
hist(gala$Species)
hist(rpois(nrow(gala), mean(gala$Species)))


## -----------------------------------------------------------------------------
testZeroInflation(res)


## -----------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(modpl)

