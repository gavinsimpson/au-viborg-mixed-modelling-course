# Analyse the Rat hormone data from Fahrmeir et al (2013) Regression: Models,
#  Methods and Applications. Springer
# Analyse the Rat hormone data from Fahrmeir et al (2013) Regression: Models,
#  Methods and Applications. Springer
pkgs <- c("ggplot2", "readr", "dplyr", "forcats", "tidyr", "marginaleffects")

# load packages
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE,
  quietly = TRUE)

# load data
rats_url <- "https://bit.ly/rat-hormone"
rats <- read_table(rats_url, col_types = "dddddddddddd-")
# ignore the warning - it"s due to trailing white space at the ends of each
#   row in the file

rats <- rats |>
  mutate(
    treatment = fct_recode(
      factor(group, levels = c(1, 2, 3)),
      Low = "1",
      High = "2",
      Control = "3"
    ),
    treatment = fct_relevel(
      treatment, c("Control", "Low", "High")
    ),
    subject = factor(subject)
  )

rats |>
  na.omit() |>
  count(subject) |>
  count(n, name = "n_rats")

plt_labs <- labs(
  y = "Head height (distance in pixels)",
  x = "Age in days",
  colour = "Treatment"
)

# plot growth curves on the original time scale - non-linear!
rats |>
  ggplot(
    aes(
      x = time,
      y = response,
      group = subject,
      colour = treatment
    )
  ) +
  geom_point(size = 1) +
  geom_line() +
  facet_wrap(~ treatment, ncol = 3) +
  plt_labs

# plot growth curves on the transformed time scale - approx. linear!
rats |>
  ggplot(
    aes(
      x = transf_time,
      y = response,
      group = subject,
      colour = treatment
    )
  ) +
  geom_point(size = 1) +
  geom_line() +
  facet_wrap(~ treatment, ncol = 3) +
  plt_labs

# Models
# 1 - correlated random effects
m1_lmer <- lmer(response ~ treatment:transf_time +
                    (1 | subject) + (0 + transf_time | subject),
                data = rats)
# 0 uncorrelated random effects
m0_lmer <- lmer(response ~ treatment:transf_time +
                  (1 + transf_time | subject),
                data = rats)

# extract the fixed effects
fixef(m1_lmer)

# extract the variance components
summary(m1_lmer)$varcor

# model summary
summary(m1_lmer)

# plot the estimated growth curves
# first including no random effects
m1_lmer |>
    plot_predictions(condition = c("transf_time", "treatment"), re.form = NA)

# and now including all the random effects - not much of a diff here
m1_lmer |>
    plot_predictions(condition = c("transf_time", "treatment"), re.form = NULL)

# from the summary, note that there is 0 variance of the rat-specific
# growth rates is ~ 0 and is what causes the warning
summary(m1_lmer)

# So let's drop this and just have random intercept for the rats
m2_lmer <- lmer(response ~ treatment:transf_time +
                    (1 | subject),
                data = rats)

# variance components
summary(m2_lmer)$varcor

# use AIC to see if one model is better than the other
AIC(m1_lmer, m2_lmer)

# Use LRT instead
anova(m1_lmer, m2_lmer)

