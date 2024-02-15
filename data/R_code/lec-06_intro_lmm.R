## ----include = TRUE, echo = FALSE---------------------------------------------
rikz <- read.table(file = "data/RIKZ.txt", header = TRUE, dec = ".")
head(rikz)


## NA

## ----include = TRUE, echo = FALSE---------------------------------------------
library(ggplot2)
library(ggeffects)
library(lme4)
mod.mix <- with(rikz, lmer(Richness ~ NAP + scale(Exposure, scale = F) + (1 | Beach)))
pred <- ggpredict(mod.mix, "NAP")
ggplot(rikz, aes(x = NAP, y = Richness)) +
  geom_point(size = 3) +
  geom_line(data = pred, aes(x = x, y = predicted), color = "blue", linewidth = 2) +
  ylim(0, 13)


## ----echo = FALSE-------------------------------------------------------------
pred <- ggpredict(mod.mix, terms = c("NAP", "Beach"), type = "random", ci_level = NA)
ggplot(rikz, aes(x = NAP, y = Richness)) +
  geom_point(size = 3) +
  ylim(0, 13) +
  geom_line(data = pred, aes(x = x, y = predicted, color = group), linewidth = 1) +
  theme(legend.position = "none")


## ----include = TRUE-----------------------------------------------------------
summary(mod.mix)


## ----include = TRUE, echo = FALSE---------------------------------------------
mod.fix <- with(rikz, lm(Richness ~ NAP + as.factor(Beach)))
mod.mix <- with(rikz, lmer(Richness ~ NAP + (1 | Beach)))
dat <- data.frame(
  fix = mod.fix$coefficients[1] + c(0, mod.fix$coefficients[3:10]),
  mix = coef(mod.mix)$Beach[, 1]
)
ggplot(dat, aes(x = fix, y = mix)) +
  geom_point(size = 3) +
  geom_abline(linewidth = 2) +
  xlab("Estimated as fixed effects") +
  ylab("Estimated as random effects")


## ----eval = TRUE, include = FALSE---------------------------------------------
mod.mix <- with(rikz, lmer(Richness ~ NAP + Exposure + (1 | Beach)))


## ----eval = TRUE, echo = FALSE------------------------------------------------
mod.mix.avec <- with(rikz, lmer(Richness ~ NAP + Exposure + (1 | Beach)))
mod.mix.sans <- with(rikz, lmer(Richness ~ NAP + (1 | Beach)))


## ----eval = TRUE, include = FALSE---------------------------------------------
mod.mix <- lmer(Richness ~ NAP + scale(Exposure, scale = F) + (NAP | Beach), data = rikz)


## -----------------------------------------------------------------------------
#| echo: false
pred <- ggpredict(
  mod.mix,
  terms = c("NAP", "Beach"), type = "random", ci_level = NA
)
ggplot(rikz, aes(x = NAP, y = Richness, color = as.factor(Beach))) +
  geom_point(size = 3) +
  ylim(0, 13) +
  geom_line(
    data = pred, aes(x = x, y = predicted, color = group), linewidth = 1
   ) +
  theme(legend.position = "none")

