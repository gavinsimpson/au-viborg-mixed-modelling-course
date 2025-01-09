# Analyse the Rat hormone data from Fahrmeir et al (2013) Regression: Models,
#  Methods and Applications. Springer
pkgs <- c("mgcv", "lme4", "ggplot2", "readr", "dplyr", "forcats", "tidyr",
  "gratia")

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
