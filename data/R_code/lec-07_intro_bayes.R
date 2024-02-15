## -----------------------------------------------------------------------------
#| echo: false
n <- 100
prior <- 0.01
type_1 <- 0.05
type_2 <- 0.2

ba <- n * (1 - prior) * type_1
bb <- n * (1 - prior) * (1 - type_1)
bc <- n * prior * (1 - type_2)
bd <- n * prior * type_2


## -----------------------------------------------------------------------------
#| echo: false
n <- 100
prior <- 0.2
type_1 <- 0.05
type_2 <- 0.2

ba <- n * (1 - prior) * type_1
bb <- n * (1 - prior) * (1 - type_1)
bc <- n * prior * (1 - type_2)
bd <- n * prior * type_2


## -----------------------------------------------------------------------------
#| echo: false
library(tidyverse)
dat <- data.frame(x = seq(0, 1, len = 1001)) %>%
  mutate(
    prior = dbeta(x, 4, 6),
    likelihood = dbeta(x, 8, 2),
    posterior = prior * likelihood / 0.3
  )
ggplot(dat, aes(x = x)) +
  geom_line(aes(y = prior, color = "pr"), linewidth = 1.5) +
  geom_area(aes(y = prior, color = "pr", fill = "pr"), alpha = 0.5) +
  labs(x = "Parameter value", y = "Probability density", color = "", fill = "") +
  scale_color_manual(
    values = c("pr" = "red"),
    labels = c("Prior")
  ) +
  scale_fill_manual(
    values = c("pr" = "red"),
    labels = c("Prior")
  ) +
  guides(color = "none") +
  ylim(0, max(dat$likelihood))


## -----------------------------------------------------------------------------
#| echo: false
ggplot(dat, aes(x = x)) +
  geom_line(aes(y = prior, color = "pr"), linewidth = 1.5) +
  geom_area(aes(y = prior, color = "pr", fill = "pr"), alpha = 0.5) +
  geom_line(aes(y = likelihood, color = "lik"), linewidth = 1.5) +
  geom_area(aes(y = likelihood, color = "lik", fill = "lik"), alpha = 0.5) +
  labs(x = "Parameter value", y = "Probability density", color = "", fill = "") +
  scale_color_manual(
    values = c("pr" = "red", "lik" = "yellow", "ps" = "orange"),
    labels = c("Prior", "Likelihood", "Posterior")
  ) +
  scale_fill_manual(
    values = c("pr" = "red", "lik" = "yellow"),
    labels = c("Likelihood", "Prior")
  ) +
  guides(color = "none") +
  ylim(0, max(dat$likelihood))


## -----------------------------------------------------------------------------
#| echo: false
ggplot(dat, aes(x = x)) +
  geom_line(aes(y = prior, color = "pr"), linewidth = 1.5) +
  geom_area(aes(y = prior, color = "pr", fill = "pr"), alpha = 0.5) +
  geom_line(aes(y = likelihood, color = "lik"), linewidth = 1.5) +
  geom_area(aes(y = likelihood, color = "lik", fill = "lik"), alpha = 0.5) +
  geom_line(aes(y = posterior, color = "ps"), linewidth = 1.5) +
  geom_area(aes(y = posterior, color = "ps", fill = "ps"), alpha = 0.5) +
  labs(x = "Parameter value", y = "Probability density", color = "", fill = "") +
  scale_color_manual(
    values = c("pr" = "red", "lik" = "yellow", "ps" = "orange"),
    labels = c("Prior", "Likelihood", "Posterior")
  ) +
  scale_fill_manual(
    values = c("pr" = "red", "lik" = "yellow", "ps" = "orange"),
    labels = c("Likelihood", "Prior", "Posterior")
  ) +
  guides(color = "none") +
  ylim(0, max(dat$likelihood))


## -----------------------------------------------------------------------------
#| echo: false
dat_text <- data.frame(
  x = c(0.38, 0.61, 0.85), y = 2,
  label = c("Horse", "Mule", "Donkey")
)
ggplot(dat, aes(x = x)) +
  geom_line(aes(y = prior, color = "pr"), linewidth = 1.5) +
  geom_area(aes(y = prior, color = "pr", fill = "pr"), alpha = 0.5) +
  geom_line(aes(y = likelihood, color = "lik"), linewidth = 1.5) +
  geom_area(aes(y = likelihood, color = "lik", fill = "lik"), alpha = 0.5) +
  geom_line(aes(y = posterior, color = "ps"), linewidth = 1.5) +
  geom_area(aes(y = posterior, color = "ps", fill = "ps"), alpha = 0.5) +
  labs(x = "Parameter value", y = "Probability density", color = "", fill = "") +
  scale_color_manual(
    values = c("pr" = "red", "lik" = "yellow", "ps" = "orange"),
    labels = c("Prior", "Likelihood", "Posterior")
  ) +
  scale_fill_manual(
    values = c("pr" = "red", "lik" = "yellow", "ps" = "orange"),
    labels = c("Likelihood", "Prior", "Posterior")
  ) +
  guides(color = "none") +
  ylim(0, max(dat$likelihood)) +
  geom_text(data = dat_text, aes(x = x, y = y, label = label), size = 10)


## -----------------------------------------------------------------------------
#| echo: false
dat <- data.frame(x = seq(0, 1, len = 1000)) %>%
  mutate(
    pr_1 = dbeta(x, 4, 16),
    pr_2 = dbeta(x, 16, 4),
    lik = dbeta(x, 300, 200),
    pr_lik_1 = pr_1 * lik,
    ps_1 = pr_lik_1 / sum(pr_lik_1) * sum(pr_1),
    pr_lik_2 = pr_2 * lik,
    ps_2 = pr_lik_2 / sum(pr_lik_2) * sum(pr_2)
  )
ggplot(dat, aes(x = x)) +
  geom_line(aes(y = pr_1, color = "pr"), linewidth = 1.5) +
  geom_line(aes(y = pr_2, color = "pr"), linewidth = 1.5, linetype = 2) +
  geom_line(aes(y = lik, color = "lik"), linewidth = 1.5) +
  geom_area(aes(y = lik, color = "lik", fill = "lik"), alpha = 0.5) +
  geom_line(aes(y = ps_1, color = "ps"), linewidth = 1.5) +
  geom_line(aes(y = ps_2, color = "ps"), linewidth = 1.5, linetype = 2) +
  labs(x = "Parameter value", y = "Probability density", color = "", fill = "") +
  scale_color_manual(
    values = c("lik" = "yellow", "pr" = "red", "pr" = "red", "ps" = "orange"),
    labels = c("Likelihood", "Prior", "Posterior")
  ) +
  scale_fill_manual(values = c("lik" = "yellow")) +
  guides(fill = "none")


## -----------------------------------------------------------------------------
#| echo: false
dat <- data.frame(x = seq(0, 1, len = 1000)) %>%
  mutate(
    pr = dunif(x, 0, 1),
    lik = dbeta(x, 60, 40),
    pr_lik = pr * lik,
    ps = pr_lik / sum(pr_lik) * sum(pr),
  )
ggplot(dat, aes(x = x)) +
  geom_line(aes(y = pr, color = "pr"), linewidth = 1.5) +
  geom_line(aes(y = lik, color = "lik"), linewidth = 1.5) +
  geom_area(aes(y = lik, color = "lik", fill = "lik"), alpha = 0.5) +
  geom_line(aes(y = ps, color = "ps"), linewidth = 1.5) +
  labs(x = "Parameter value", y = "Probability density", color = "", fill = "") +
  scale_color_manual(
    values = c("lik" = "yellow", "pr" = "red", "pr" = "red", "ps" = "orange"),
    labels = c("Likelihood", "Prior", "Posterior")
  ) +
  scale_fill_manual(values = c("lik" = "yellow")) +
  guides(fill = "none")


## -----------------------------------------------------------------------------
#| echo: false
dat <- data.frame(x = seq(0, 1, len = 1001)) %>%
  mutate(
    pr = dbeta(x, 1, 2),
    lik = dbeta(x, 60, 40),
    pr_lik = pr * lik,
    ps = pr_lik / sum(pr_lik) * 1000,
  )
inter_ps <- c(
  min(which(cumsum(dat$ps) / sum(dat$ps) > 0.025)),
  min(which(cumsum(dat$ps) / sum(dat$ps) > 0.975))
)
dat_inter_1 <- dat[1:inter_ps[1], ]
dat_inter_2 <- dat[inter_ps[2]:nrow(dat), ]
dat_text <- data.frame(
  x = c(qbeta(0.025, 60, 40), 0.6, qbeta(0.975, 60, 40)),
  label = c("95%CI", "beta", "95% CI"),
  y = 9
)
ggplot(dat, aes(x = x)) +
  geom_line(aes(y = pr, color = "pr"), linewidth = 1.5) +
  geom_line(aes(y = lik, color = "lik"), linewidth = 1.5) +
  geom_area(aes(y = lik, color = "lik", fill = "lik"), alpha = 0.5) +
  geom_line(aes(y = ps, color = "ps"), linewidth = 1.5) +
  labs(x = "Parameter value", y = "Probability density", color = "", fill = "") +
  scale_color_manual(
    values = c("lik" = "yellow", "pr" = "red", "pr" = "red", "ps" = "orange"),
    labels = c("Likelihood", "Prior", "Posterior")
  ) +
  scale_fill_manual(values = c("lik" = "yellow")) +
  guides(fill = "none") +
  xlim(0.4, 0.8) +
  ylim(0, 9) +
  geom_vline(aes(xintercept = 0.6), linetype = 2) +
  geom_vline(aes(xintercept = qbeta(0.025, 60, 40)), color = "grey") +
  geom_vline(aes(xintercept = qbeta(0.975, 60, 40)), colour = "grey") +
  geom_area(data = dat_inter_1, aes(y = ps), alpha = 0.3) +
  geom_area(data = dat_inter_2, aes(y = ps), alpha = 0.3) +
  geom_text(data = dat_text, aes(x = x, y = y, label = label), nudge_x = 0.015, size = 10)


## -----------------------------------------------------------------------------
#| echo: false
dat <- data.frame(x = seq(-2, 5, len = 1001)) %>%
  mutate(
    pr = dnorm(x, 0, 0.5),
    lik = dnorm(x, 2.2, 1),
    pr_lik = pr * lik,
    ps = pr_lik / sum(pr_lik) * sum(pr),
  )
ggplot(dat, aes(x = x)) +
  geom_line(aes(y = pr, color = "pr"), linewidth = 1.5) +
  geom_line(aes(y = lik, color = "lik"), linewidth = 1.5) +
  geom_area(aes(y = lik, color = "lik", fill = "lik"), alpha = 0.5) +
  geom_line(aes(y = ps, color = "ps"), linewidth = 1.5) +
  geom_pointrange(aes(x = 2.2, y = 0.05, xmin = qnorm(0.025, 2.2, 1), xmax = qnorm(0.975, 2.2, 1))) +
  labs(x = "Parameter value", y = "Probability density", color = "", fill = "") +
  geom_vline(aes(xintercept = 0), color = "grey", linetype = 2) +
  scale_color_manual(
    values = c("lik" = "yellow", "pr" = "red", "pr" = "red", "ps" = "orange"),
    labels = c("Likelihood", "Prior", "Posterior")
  ) +
  scale_fill_manual(values = c("lik" = "yellow")) +
  guides(fill = "none")

