library("readr")
library("dplyr")
library("here")
library("ggplot2")
library("marginaleffects")
library("glmmTMB")

moth <- readr::read_csv("data/uefunex.csv")

moth |>
  group_by(treatment) |>
  summarise(n = n(), mean = mean(parasitoid), median = median(parasitoid),
            sd = sd(parasitoid))

moth |>
  ggplot(aes(x = treatment, y = parasitoid)) +
  geom_violin(aes(fill = treatment))

moth |>
  ggplot(aes(x = treatment, y = parasitoid)) +
  geom_violin(aes(fill = treatment)) +
  scale_y_sqrt()

# install.packages("ggforce")
moth |>
  ggplot(aes(x = treatment, y = parasitoid)) +
  geom_violin(aes(fill = treatment)) +
  scale_y_continuous(trans = ggforce::power_trans((1/4)))

moth_glm1 <- glm(parasitoid ~ moth + treatment + moth:treatment,
                 family = poisson, data = moth)

moth_glm2 <- glm(parasitoid ~ treatment + moth + treatment:moth,
                 family = poisson, data = moth)

summary(moth_glm1)

anova(moth_glm1, test = "LRT")

moth |>
  ggplot(aes(y = parasitoid, x = moth, color = treatment)) +
  geom_jitter(stat = "identity", width = 0.05, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  theme(legend.position = "bottom")

moth_glm1 |>
  plot_predictions(condition = c("moth", "treatment"), points = 0.8)

presid <- resid(moth_glm1, type = "pearson")
n <- nrow(moth)

params <- length(coef(moth_glm1))
disp <- sum(presid^2) / (n - params)
disp

moth_glm2 <- glm(parasitoid ~ moth + treatment + moth:treatment,
                 family = quasipoisson, data = moth)
summary(moth_glm2)

library("mgcv")
moth_glm5 <- gam(parasitoid ~ scale(moth) + treatment + scale(moth):treatment,
                 family = nb(), method = "ML", data = moth)
summary(moth_glm5)

library("glmmTMB")
moth_glm4 <- glmmTMB(parasitoid ~ moth + treatment + moth:treatment,
                     family = nbinom2("log"), REML = FALSE, data = moth)
summary(moth_glm4)

library("marginaleffects")
moth_glm4 |>
  plot_predictions(condition = c("moth", "treatment"),
                   vcov = TRUE, type = "response")

ilink <- family(moth_glm4)$linkinv

moth_glm4 |>
  plot_predictions(condition = c("moth", "treatment"),
                   vcov = TRUE, type = "link", transform = ilink)
moth_glm5 |>
  plot_predictions(condition = c("moth", "treatment"),
                   type = "link", transform = ilink)
