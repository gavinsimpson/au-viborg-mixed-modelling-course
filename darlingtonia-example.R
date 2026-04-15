# comments
library("readr")
library("dplyr")
library("here")
library("ggplot2")
library("marginaleffects")

wasp <- read_csv(here("data", "darlingtonia.csv"), comment = "#",
                 col_types = "dl")

wasp |> print(n = 32)

# fit the model logistic regression
m <- glm(visited ~ leafHeight, data = wasp, family = binomial(link = "logit"))

summary(m)

# data to predict at
pdat <- with(wasp,
             tibble(leafHeight = seq(min(leafHeight),
                                     max(leafHeight),
                                     length = 100)))
# predict
pred <- predict(m, pdat, type = "link", se.fit = TRUE)

ilink <- family(m)$linkinv # g-1()

pdat <- pdat |>
  bind_cols(data.frame(pred)) |>
  mutate(fitted = ilink(fit),
         upper = ilink(fit + (1.96 * se.fit)),
         lower = ilink(fit - (1.96 * se.fit)))

# plot
ggplot(wasp, aes(x = leafHeight,
                 y = as.numeric(visited))) +
  geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper,
                  x = leafHeight), data = pdat,
              inherit.aes = FALSE, alpha = 0.2) +
  geom_line(data = pdat, aes(y = fitted)) +
  labs(x = "Leaf Height [cm]",
       y = "Probability of visitation")

plot_predictions(m, condition = "leafHeight") +
  geom_point(aes(x = leafHeight, y = as.numeric(visited)), data = wasp)

## Maddy peat bog

maddy <- read_csv(here("data", "maddy-peat.csv"), col_types = "cdddddd")
maddy <- mutate(maddy, midDepth = upperDepth - (0.5 * abs(upperDepth - lowerDepth)),
                calMid = calUpper - (0.5 * abs(calUpper - calLower)))
maddy

ggplot(maddy, aes(x = midDepth, y = calMid)) +
  geom_point() +
  labs(y = "Calibrated Age", x = "Depth")



summary(m_gamma)
