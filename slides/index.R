## ----setup, include=FALSE, cache=FALSE----------------------------------------
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(cache = TRUE, dev = 'svg', echo = TRUE, message = FALSE, warning = FALSE,
                      fig.height = 6, fig.width = 1.777777 * 6, fig.align = "center")

library('here')
library('mgcv')
library('ggplot2')
library('purrr')
library("tibble")
library('patchwork')
library('tidyr')
library('readr')
library("dplyr")
library("forcats")
library("ggokabeito")
## plot defaults
theme_set(theme_minimal(base_size = 16, base_family = 'Fira Sans'))



## ----eval = FALSE-------------------------------------------------------------
# #install.packages("usethis")
# usethis::use_course("https://bit.ly/422MKkY")


## -----------------------------------------------------------------------------
library("dplyr")
library("readr")
library("tibble")
# install.packages("here")
library("here")
library("ggplot2")


## ----sim-model, fig.align="center", out.width="50%", echo=FALSE---------------
knitr::include_graphics("resources/co2-model.png")


## ----statistical-model-example, echo = FALSE, fig.align="center", out.width="60%"----
set.seed(1)
df <- data.frame(x = runif(100))
df <- transform(df, y = 2 + (0.85 * x) + rnorm(100, sd = 0.25))
ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm")


## -----------------------------------------------------------------------------
dbinom(x = 7, size = 10, prob = 0.7)


## ----binomial-pdf, echo = FALSE-----------------------------------------------
## Binomial Probability mass function
s <- seq(0, 40, by = 1)
n <- rep(c(20,20,40), each = length(s))
binom.pmf <- data.frame(x = rep(s, 3),
                        n = rep(c(20,20,40), each = length(s)),
                        p = rep(c(0.5, 0.7, 0.5), each = length(s)))
binom.pmf <- transform(binom.pmf,
                       pmf = dbinom(x, size = n, prob = p),
                       params = paste("n=", n, "; p=", p, sep = ""))

plt.binom <- ggplot(binom.pmf, aes(x = x, y = pmf, fill = params)) +
    geom_col() + labs(y = "Probability Mass", x = "No. of Successes") +
    facet_wrap(~ params) + scale_fill_okabe_ito()
plt.binom


## ----poisson-pdf, echo = FALSE------------------------------------------------
s <- seq(0, 20, by = 1)
poisson.pmf <- data.frame(x = rep(s, 3),
                          lambda = rep(c(1,4,10), each = length(s)))
poisson.pmf <- transform(poisson.pmf,
                         pmf = dpois(x, lambda = lambda),
                         params = paste("lambda=", lambda, sep = ""))

plt.poisson <- ggplot(poisson.pmf, aes(x = x, y = pmf, fill = params)) +
    geom_col() + labs(y = "Probability Mass", x = "Count") +
    facet_wrap(~ params) + scale_fill_okabe_ito()
plt.poisson


## ----dist-fig-1, fig.align = "center", out.width = "80%", echo = FALSE--------
knitr::include_graphics("resources/fig-gaussian-lm-descriptive-figure-1.png")


## ----dist-fig-2, fig.align = "center", out.width = "80%", echo = FALSE--------
knitr::include_graphics("resources/fig-poisson-lm-descriptive-figure-1.png")


## ----dist-fig-3, fig.align = "center", out.width = "80%", echo = FALSE--------
knitr::include_graphics("resources/fig-other-dist-descr-1.png")


## ----load-darl-data, echo = TRUE, eval = FALSE--------------------------------
# wasp <- read_csv("data/darlingtonia.csv", comment = "#",
#                  col_types = "dl")
# wasp


## ----load-darl-data-real, echo = FALSE, eval = TRUE---------------------------
wasp <- read_csv("../data/darlingtonia.csv", comment = "#",
                 col_types = "dl")
wasp


## ----fit-darlingtonia, echo = TRUE--------------------------------------------
m <- glm(visited ~ leafHeight, data = wasp, family = binomial)
m


## ----summary-darlingtonia, echo = TRUE----------------------------------------
summary(m)


## ----predict-darlingtonia, echo = TRUE, eval = FALSE--------------------------
# # data to predict at
# pdat <- with(wasp,
#              tibble(leafHeight = seq(min(leafHeight),
#                                      max(leafHeight),
#                                      length = 100)))
# # predict
# pred <- predict(m, pdat, type = "link", se.fit = TRUE)
# ilink <- family(m)$linkinv # g-1()
# pdat <- pdat |>
#   bind_cols(data.frame(pred)) |>
#   mutate(fitted = ilink(fit),
#          upper = ilink(fit + (2 * se.fit)),
#          lower = ilink(fit - (2 * se.fit)))
# # plot
# ggplot(wasp, aes(x = leafHeight,
#                  y = as.numeric(visited))) +
#     geom_point() +
#     geom_ribbon(aes(ymin = lower, ymax = upper,
#                     x = leafHeight), data = pdat,
#                 inherit.aes = FALSE, alpha = 0.2) +
#     geom_line(data = pdat, aes(y = fitted)) +
#     labs(x = "Leaf Height [cm]",
#          y = "Probability of visitation")


## ----predict-darlingtonia, eval = TRUE, echo = FALSE, fig.height = 6, fig.width = 6----
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
         upper = ilink(fit + (2 * se.fit)),
         lower = ilink(fit - (2 * se.fit)))
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


## ----coeftab-darlingtonia, results = "asis", echo = FALSE---------------------
knitr::kable(round(summary(m)$coefficients, 4), format = "pipe")


## ----load-moth-show, eval = FALSE---------------------------------------------
# moth <- readr::read_csv("data/uefunex.csv")

## ----load-moth-hide, echo = FALSE---------------------------------------------
moth <- readr::read_csv("../data/uefunex.csv")


## ----moth-summary-------------------------------------------------------------
library("dplyr")
moth |>
    group_by(treatment) |>
    summarise(n = n(), mean = mean(parasitoid), median = median(parasitoid),
        sd = sd(parasitoid))


## ----plot-moth-1, out.width = "80%", fig.align = "center"---------------------
library("ggplot2")
moth |>
    ggplot(aes(x = treatment, y = parasitoid)) +
    geom_violin(aes(fill = treatment))


## ----plot-moth-sqrt, out.width = "80%", fig.align = "center"------------------
moth |>
    ggplot(aes(x = treatment, y = parasitoid)) +
    geom_violin(aes(fill = treatment)) +
    scale_y_sqrt()


## ----plot-moth-root-root, out.width = "70%", fig.align = "center"-------------
# install.packages("ggforce")
moth |>
    ggplot(aes(x = treatment, y = parasitoid)) +
    geom_violin(aes(fill = treatment)) +
    scale_y_continuous(trans = ggforce::power_trans((1/4)))


## ----fit-poisson-glm-moth-----------------------------------------------------
moth_glm1 <- glm(parasitoid ~ moth + treatment + moth:treatment,
    family = poisson, data = moth)


## ----summary-poisson-glm-moth-------------------------------------------------
summary(moth_glm1)


## -----------------------------------------------------------------------------
anova(moth_glm1, test = "LRT")


## ----plot-moth-glm1, eval=FALSE-----------------------------------------------
# moth |>
#     ggplot(aes(y = parasitoid, x = moth, color = treatment)) +
#     geom_jitter(stat = "identity", width = 0.05, height = 0.05) +
#     geom_smooth(method = "glm", method.args = list(family = "poisson")) +
#     theme(legend.position = "bottom")


## ----plot-moth-glm1, echo = FALSE, fig.width = 7, fig.height = 5--------------
moth |>
    ggplot(aes(y = parasitoid, x = moth, color = treatment)) +
    geom_jitter(stat = "identity", width = 0.05, height = 0.05) +
    geom_smooth(method = "glm", method.args = list(family = "poisson")) +
    theme(legend.position = "bottom")


## ----moth-summary-------------------------------------------------------------
library("dplyr")
moth |>
    group_by(treatment) |>
    summarise(n = n(), mean = mean(parasitoid), median = median(parasitoid),
        sd = sd(parasitoid))


## -----------------------------------------------------------------------------
presid <- resid(moth_glm1, type = "pearson")
n <- nrow(moth)
params <- length(coef(moth_glm1))
disp <- sum(presid^2) / (n - params)
disp


## -----------------------------------------------------------------------------
moth_glm2 <- glm(parasitoid ~ moth + treatment + moth:treatment,
    family = quasipoisson, data = moth)

summary(moth_glm2)


## -----------------------------------------------------------------------------
library("mgcv")
moth_glm3 <- gam(parasitoid ~ moth + treatment + moth:treatment,
    family = nb(), method = "ML", data = moth)
summary(moth_glm3)


## ----glmmtmb-negbin-version---------------------------------------------------
library("glmmTMB")
moth_glm4 <- glmmTMB(parasitoid ~ moth + treatment + moth:treatment,
    family = nbinom2("log"), REML = FALSE, data = moth)
summary(moth_glm4)


## -----------------------------------------------------------------------------
library("marginaleffects")
moth_glm4 |>
    plot_predictions(condition = c("moth", "treatment"),
        vcov = TRUE, type = "response")


## ----load-coffee-show, eval = FALSE-------------------------------------------
# coffee <- readr::read_csv("data/caudill.csv")

## ----load-coffe-hide, echo = FALSE--------------------------------------------
coffee <- readr::read_csv("../data/caudill.csv")


## -----------------------------------------------------------------------------
coffee <- coffee |>
    dplyr::filter(habitat != "Forest")


## -----------------------------------------------------------------------------
coffee_glm <- glm(specdens_small ~ canopy + midstrata + lowstrata + groundcov +
        treerichness + treeheight + coffeeheight,
    data = coffee, family = poisson)
summary(coffee_glm)


## ----load-gnu-show, eval = FALSE----------------------------------------------
# gnu <- readr::read_csv("data/sinclair.csv")

## ----load-gnu-hide, echo = FALSE----------------------------------------------
gnu <- readr::read_csv("../data/sinclair.csv")


## -----------------------------------------------------------------------------
xtabs(gnus ~ death + sex + marrow, data = gnu)


## -----------------------------------------------------------------------------
gnu_glm1 <- glm(gnus ~ death * sex * marrow,
    data = gnu, family = poisson)


## -----------------------------------------------------------------------------
anova(gnu_glm1, test = "Chisq")


## -----------------------------------------------------------------------------
drop1(gnu_glm1)


## -----------------------------------------------------------------------------
gnu_glm2 <- glm(gnus ~ death + sex + marrow +
        death:sex + death:marrow + sex:marrow,
    data = gnu, family = poisson)

anova(gnu_glm1, gnu_glm2, test = "LRT")


## -----------------------------------------------------------------------------
gnu_glm2 <- glm(gnus ~ death + sex + marrow +
        death:sex + death:marrow + sex:marrow,
    data = gnu, family = poisson)


## -----------------------------------------------------------------------------
gnu_glm2a <- glm(gnus ~ (death + sex + marrow)^2,
    data = gnu, family = poisson)

all.equal(coef(gnu_glm2), coef(gnu_glm2a))


## ----fig.width = 6, fig.align="center", out.width="75%"-----------------------
layout(matrix(1:4, ncol = 2, byrow = TRUE))
moth_glm1 |> plot()
layout(1)


## ----fig.width = 6, fig.align="center", out.width="75%", cache = FALSE--------
library("gglm")
moth_glm1 |> gglm()


## ----fig.width = 6, fig.align="center", out.width="75%"-----------------------
ggplot(data = moth_glm1) + stat_fitted_resid() # plot(moth_glm1, which = 1)


## ----fig.width = 6, fig.align="center", out.width="75%"-----------------------
ggplot(data = moth_glm1) + stat_normal_qq() # plot(moth_glm1, which = 2)


## ----fig.width = 6, fig.align="center", out.width="75%"-----------------------
library("gratia")
qq_plot(moth_glm1, method = "simulate")


## ----fig.width = 6, fig.align="center", out.width="75%"-----------------------
ggplot(data = moth_glm1) + stat_scale_location() # plot(moth_glm1, which = 3)


## ----fig.width = 6, fig.align="center", out.width="75%"-----------------------
ggplot(data = moth_glm1) + stat_cooks_leverage() # plot(moth_glm1, which = 5)


## ----fig.width = 6, fig.align="center", out.width="75%"-----------------------
gglm(moth_glm1)    # Poisson


## ----fig.width = 6, fig.align="center", out.width="75%"-----------------------
gglm(moth_glm3)    # Negative binomial


## ----load-performance, cache = FALSE, fig.width = 1.777777 * 12, fig.height = 12, out.width = "85%", fig.align = "center"----
library("performance")
check_model(moth_glm1)


## ----dharma-residuals-image, fig.align = "center", out.width = "50%", echo = FALSE----
knitr::include_graphics("resources/dharma-residuals.png")


## ----dharma-1-----------------------------------------------------------------
library("mgcViz")
library("DHARMa")

testDispersion(moth_glm1, plot = FALSE)


## ----dharma-2-----------------------------------------------------------------
testDispersion(moth_glm3, plot = FALSE)


## ----dharma-sim-resids--------------------------------------------------------
resids <- simulateResiduals(fittedModel = moth_glm1, plot = FALSE)


## ----dharma-plots-possion, fig.align = "center", out.width = "90%"------------
plot(resids)


## ----dharma-plots-negbin, fig.align = "center", out.width = "90%"-------------
resids <- simulateResiduals(fittedModel = moth_glm3, plot = FALSE)
plot(resids)


## ----echo = FALSE-------------------------------------------------------------
knitr::include_graphics("resources/slider-switch-annotated-80.jpg")


## -----------------------------------------------------------------------------
# install.packages("palmerpenguins")
library("palmerpenguins")
library("tidyr")
penguins <- penguins |> drop_na()

model_slider <- lm(body_mass_g ~ flipper_length_mm, data = penguins)
model_switch <- lm(body_mass_g ~ species, data = penguins)


## -----------------------------------------------------------------------------
library("broom")
tidy(model_slider)
tidy(model_switch)


## -----------------------------------------------------------------------------
tidy(model_slider)


## -----------------------------------------------------------------------------
tidy(model_switch)


## -----------------------------------------------------------------------------
gglm(model_slider)


## -----------------------------------------------------------------------------
gglm(model_switch)


## ----glm-penguins-------------------------------------------------------------
glm_slider <- glm(body_mass_g ~ flipper_length_mm, data = penguins, family = Gamma("log"))
glm_switch <- glm(body_mass_g ~ species, data = penguins, family = Gamma("log"))


## -----------------------------------------------------------------------------
gglm(glm_slider)


## -----------------------------------------------------------------------------
gglm(glm_switch)


## ----dependson = "glm-penguins"-----------------------------------------------
tidy(glm_slider)


## -----------------------------------------------------------------------------
exp(0.0115)


## ----echo = FALSE-------------------------------------------------------------
knitr::include_graphics("resources/mixer-board-annotated-80.jpg")


## -----------------------------------------------------------------------------
model_mixer <- lm(body_mass_g ~ flipper_length_mm + bill_depth_mm + species + sex,
                  data = penguins)
tidy(model_mixer)


## -----------------------------------------------------------------------------
library("marginaleffects")


## -----------------------------------------------------------------------------
broom::tidy(gnu_glm1)


## -----------------------------------------------------------------------------
gnu


## -----------------------------------------------------------------------------
gnu_glm1 |> slopes(variable = "death")


## -----------------------------------------------------------------------------
(df_predation <- datagrid(model = gnu_glm1, death = "Predation"))


## -----------------------------------------------------------------------------
(df_other <- datagrid(model = gnu_glm1, death = "Other"))


## -----------------------------------------------------------------------------
p_predation <- predict(gnu_glm1, newdata = df_predation, type = "response")
p_other <- predict(gnu_glm1, newdata = df_other, type = "response")

p_predation
p_other


## -----------------------------------------------------------------------------
p_predation - p_other


## -----------------------------------------------------------------------------
mean(p_predation - p_other)


## -----------------------------------------------------------------------------
library("marginaleffects")
gnu_glm1 |> avg_slopes(variable = "death")


## -----------------------------------------------------------------------------
gnu_glm1 |> avg_comparisons(variables = "death")


## -----------------------------------------------------------------------------
gnu_glm1 |> comparisons(variable = "death")


## -----------------------------------------------------------------------------
gnu_glm1 |> avg_slopes()


## -----------------------------------------------------------------------------
gnu_glm1 |> avg_predictions(newdata = "mean", variable = "death")


## -----------------------------------------------------------------------------
gnu_glm1 |> avg_comparisons(newdata = "mean", variable = "death")


## -----------------------------------------------------------------------------
datagrid(model = gnu_glm1, death = c("Predation", "Other"))


## -----------------------------------------------------------------------------
gnu_glm1 |> avg_comparisons(newdata = "mean", variable = "death", by = c("sex", "marrow"))
gnu_glm1 |> avg_comparisons(variable = "death", by = c("sex", "marrow"))


## -----------------------------------------------------------------------------
gnu_glm1 |> avg_comparisons(variables = list(marrow = "pairwise"))


## -----------------------------------------------------------------------------
gnu_glm1 |> avg_comparisons(variables = list(marrow = "pairwise"), by = "sex")


## -----------------------------------------------------------------------------
gnu_glm1 |> avg_comparisons(variables = list(marrow = "pairwise"), by = c("sex", "death"))


## -----------------------------------------------------------------------------
gnu_glm1 |> avg_comparisons(variables = list(marrow = "pairwise"),
    by = c("sex", "death"), multcomp = "fdr")


## ----random-effects-cartoon, echo = FALSE-------------------------------------
X <- rep(1:10, times = 3)
y <- rep(c(3,10,20), each = 10) + (1.2 * X)
Xy <- data.frame(y = y, X = X, group = rep(paste("Site", 1:3), each = 10))


X2 <- rep(1:10, times = 3)
y2 <- rep(c(1,3,6.5), each = 10) + (rep(c(0.5, 1, 2.5), each = 10) * X)
Xy2 <- data.frame(y = y2, X = X2, group = rep(paste("Site", 1:3), each = 10))

ylim <- range(Xy$y, Xy2$y)
layout(matrix(1:2, ncol = 2))
op <- par(mar = c(5,4,5,2) + 0.1)
plot(y ~ X, data = Xy, type = "n", ylim = ylim,
     main = "Random intercepts")
for (i in 1:3) {
    lines(y ~ X, data = Xy, subset = group == paste("Site", i))
}
plot(y ~ X, data = Xy, type = "n", ylim = ylim,
     main = "Random slopes & intercepts")
for (i in 1:3) {
    lines(y ~ X, data = Xy2, subset = group == paste("Site", i))
}
par(op)
layout(1)


## ----chicks-load--------------------------------------------------------------
data(ChickWeight)
glimpse(ChickWeight)


## ----chicks-plot, out.width = "75%", fig.align = "center"---------------------
ChickWeight |>
  ggplot(aes(x = Time, y = weight, group = Chick)) +
  geom_line(alpha = 0.7) +
  ylim(0, NA) +
  facet_wrap(~ Diet, labeller = label_both)


## ----chick-lmer1--------------------------------------------------------------
library("lme4")
chick_lmm1 <- lmer(weight ~ 1 + Time + (1 + Time | Chick),
  data = ChickWeight)


## ----summary-chick-1----------------------------------------------------------
summary(chick_lmm1)


## ----predictions-chick-lmm1, eval = FALSE-------------------------------------
# library("marginaleffects")
# p_chick_lmm1 <- predictions(chick_lmm1,
#     newdata = datagrid(Chick = ChickWeight$Chick,
#         Time = 0:21))
# 
# p_chick_lmm1 |>
# ggplot(aes(Time, estimate, level = Chick)) +
#     geom_line() +
#     labs(y = "Predicted weight",
#         x = "Time",
#         title = "Linear growth model")


## ----predictions-chick-lmm1, echo = FALSE-------------------------------------
library("marginaleffects")
p_chick_lmm1 <- predictions(chick_lmm1,
    newdata = datagrid(Chick = ChickWeight$Chick,
        Time = 0:21))

p_chick_lmm1 |>
ggplot(aes(Time, estimate, level = Chick)) +
    geom_line() +
    labs(y = "Predicted weight",
        x = "Time",
        title = "Linear growth model")


## ----pop-predictions-chick-lmm1, eval = FALSE---------------------------------
# pop_chick_lmm1 <- predictions(chick_lmm1,
#     newdata = datagrid(Chick = NA, Time = 0:21),
#     re.form = NA) # <-- !!
# 
# pop_chick_lmm1 |>
#     ggplot(aes(x = Time, y = estimate,
#         ymin = conf.low, ymax = conf.high)) +
#     geom_ribbon(alpha = .1, fill = "red") +
#     geom_line() +
#     labs(y = "Predicted weight",
#         x = "Time",
#         title = "Population-level trajectories")


## ----pop-predictions-chick-lmm1, echo = FALSE---------------------------------
pop_chick_lmm1 <- predictions(chick_lmm1,
    newdata = datagrid(Chick = NA, Time = 0:21),
    re.form = NA) # <-- !!

pop_chick_lmm1 |>
    ggplot(aes(x = Time, y = estimate,
        ymin = conf.low, ymax = conf.high)) +
    geom_ribbon(alpha = .1, fill = "red") +
    geom_line() +
    labs(y = "Predicted weight",
        x = "Time",
        title = "Population-level trajectories")


## ----chick-pop-level-eff------------------------------------------------------
chick_lmm1 |> plot_predictions(condition = c("Time"), re.form = NA)


## ----chick-lmer2--------------------------------------------------------------
chick_lmm2 <- lmer(weight ~ 1 + Time + Diet + Time:Diet + (1 + Time | Chick),
  data = ChickWeight)


## ----summary-chick-2----------------------------------------------------------
summary(chick_lmm2)


## ----predictions-chick-lmm2, eval = FALSE-------------------------------------
# p_chick_lmm2 <- predictions(chick_lmm2)
# 
# p_chick_lmm2 |>
# ggplot(aes(Time, estimate, level = Chick)) +
#     geom_line() +
#     labs(y = "Predicted weight",
#         x = "Time",
#         title = "Linear growth model") +
#     facet_wrap(~ Diet, labeller = label_both)


## ----predictions-chick-lmm2, echo = FALSE-------------------------------------
p_chick_lmm2 <- predictions(chick_lmm2)

p_chick_lmm2 |>
ggplot(aes(Time, estimate, level = Chick)) +
    geom_line() +
    labs(y = "Predicted weight",
        x = "Time",
        title = "Linear growth model") +
    facet_wrap(~ Diet, labeller = label_both)


## ----pop-predictions-chick-lmm2, eval = FALSE---------------------------------
# pop_chick_lmm2 <- predictions(chick_lmm2,
#     newdata = datagrid(Chick = NA, Diet = 1:4, Time = 0:21),
#     re.form = NA) # <-- !!
# 
# pop_chick_lmm2 |>
#     ggplot(aes(x = Time, y = estimate,
#         ymin = conf.low, ymax = conf.high)) +
#     geom_ribbon(alpha = .1, fill = "red") +
#     geom_line() +
#     labs(y = "Predicted weight",
#         x = "Time",
#         title = "Population-level trajectories") +
#     facet_wrap(~ Diet, labeller = label_both)


## ----pop-predictions-chick-lmm2, echo = FALSE---------------------------------
pop_chick_lmm2 <- predictions(chick_lmm2,
    newdata = datagrid(Chick = NA, Diet = 1:4, Time = 0:21),
    re.form = NA) # <-- !!

pop_chick_lmm2 |>
    ggplot(aes(x = Time, y = estimate,
        ymin = conf.low, ymax = conf.high)) +
    geom_ribbon(alpha = .1, fill = "red") +
    geom_line() +
    labs(y = "Predicted weight",
        x = "Time",
        title = "Population-level trajectories") +
    facet_wrap(~ Diet, labeller = label_both)


## ----chick-pop-plot-predictions-lmm2------------------------------------------
chick_lmm2 |> plot_predictions(condition = c("Time", "Diet"), re.form = NA)


## ----chick-lmer3--------------------------------------------------------------
chick_lmm3 <- lmer(weight ~ 1 + Time + Time:Diet + (1 + Time | Chick),
  data = ChickWeight)


## ----summary-chick-3----------------------------------------------------------
summary(chick_lmm3)


## ----predictions-chick-lmm3, eval = FALSE-------------------------------------
# p_chick_lmm3 <- predictions(chick_lmm3)
# 
# p_chick_lmm3 |>
# ggplot(aes(Time, estimate, level = Chick)) +
#     geom_line() +
#     labs(y = "Predicted weight",
#         x = "Time",
#         title = "Linear growth model 2") +
#     facet_wrap(~ Diet, labeller = label_both)


## ----predictions-chick-lmm3, echo = FALSE, cache = FALSE----------------------
p_chick_lmm3 <- predictions(chick_lmm3)

p_chick_lmm3 |>
ggplot(aes(Time, estimate, level = Chick)) +
    geom_line() +
    labs(y = "Predicted weight",
        x = "Time",
        title = "Linear growth model 2") +
    facet_wrap(~ Diet, labeller = label_both)


## ----pop-predictions-chick-lmm3, eval = FALSE---------------------------------
# pop_chick_lmm3 <- predictions(chick_lmm3,
#     newdata = datagrid(Chick = NA, Diet = 1:4, Time = 0:21),
#     re.form = NA) # <-- !!
# 
# pop_chick_lmm3 |>
#     ggplot(aes(x = Time, y = estimate,
#         ymin = conf.low, ymax = conf.high)) +
#     geom_ribbon(alpha = .1, fill = "red") +
#     geom_line() +
#     labs(y = "Predicted weight",
#         x = "Time",
#         title = "Population-level trajectories 2") +
#     facet_wrap(~ Diet, labeller = label_both)


## ----pop-predictions-chick-lmm3, echo = FALSE---------------------------------
pop_chick_lmm3 <- predictions(chick_lmm3,
    newdata = datagrid(Chick = NA, Diet = 1:4, Time = 0:21),
    re.form = NA) # <-- !!

pop_chick_lmm3 |>
    ggplot(aes(x = Time, y = estimate,
        ymin = conf.low, ymax = conf.high)) +
    geom_ribbon(alpha = .1, fill = "red") +
    geom_line() +
    labs(y = "Predicted weight",
        x = "Time",
        title = "Population-level trajectories 2") +
    facet_wrap(~ Diet, labeller = label_both)


## ----chick-pop-plot-predictions-lmm2------------------------------------------
chick_lmm2 |> plot_predictions(condition = c("Time", "Diet"), re.form = NA)


## ----chick-pop-plot-predictions-lmm3------------------------------------------
chick_lmm3 |> plot_predictions(condition = c("Time", "Diet"), re.form = NA)


## -----------------------------------------------------------------------------
chick_lmm3 |> avg_slopes(variable = "Time", by = "Diet", re.form = NA)


## -----------------------------------------------------------------------------
chick_lmm3 |> avg_comparisons(
    variables = "Time", by = "Diet",
    hypothesis = difference ~ pairwise, re.form = NA
)


## -----------------------------------------------------------------------------
chick_lmm3 |> avg_comparisons(
    variables = "Time", by = "Diet",
    hypothesis = difference ~ reference, re.form = NA
)


## ----chick-lmer4--------------------------------------------------------------
chick_lmm4 <- lmer(weight ~ 1 + Time + Time:Diet + (1 + Time + Time:Diet | Chick),
  data = ChickWeight)


## ----echo = FALSE, out.width = "50%", fig.align = "center"--------------------
knitr::include_graphics("resources/dharma-residuals.png")


## -----------------------------------------------------------------------------
library("DHARMa")
sim_lmm3 <- simulateResiduals(chick_lmm3, plot = FALSE)


## -----------------------------------------------------------------------------
plot(sim_lmm3)


## -----------------------------------------------------------------------------
chick_glmm1 <- glmer(
    weight ~ 1 + Time + Time:Diet + (1 + Time | Chick),
    data = ChickWeight,
    family = Gamma(link = "log")
)
sim_glmm1 <- simulateResiduals(chick_glmm1, plot = FALSE)


## -----------------------------------------------------------------------------
plot(sim_glmm1)


## -----------------------------------------------------------------------------
chick_lmm5 <- lmer(weight ~ 1 + poly(Time, 2) + poly(Time, 2):Diet + (1 + poly(Time, 2) | Chick),
    data = ChickWeight)
sim_lmm5 <- simulateResiduals(chick_lmm5, plot = FALSE)


## -----------------------------------------------------------------------------
plot(sim_lmm5)


## ----setup-rat-hormone-example, echo = FALSE----------------------------------
#rats_url <- "https://bit.ly/rat-hormone"
#rats <- read_table(rats_url, col_types = "dddddddddddd-")
rats <- read_table("data/rats.txt", col_types = "dddddddddddd-")
# ignore the warning - it"s due to trailing white space at the ends of each
#   row in the file

rats <- rats |>
    mutate(treatment = fct_recode(factor(group, levels = c(1, 2, 3)),
                                  Low = "1",
                                  High = "2",
                                  Control = "3"),
           treatment = fct_relevel(treatment, c("Control", "Low", "High")),
           subject = factor(subject))

plt_labs <- labs(y = "Head height (distance in pixels)",
                 x = "Age in days",
                 colour = "Treatment")

rat_plt <- ggplot(rats, aes(x = time, y = response,
                            group = subject, colour = treatment)) +
    geom_line() +
    facet_wrap(~ treatment, ncol = 3) +
    plt_labs


## ----plot-rat-data, echo = FALSE----------------------------------------------
rat_plt


## ----obs-per-rat, echo = FALSE------------------------------------------------
rats %>%
    na.omit() %>%
    count(subject) %>%
    count(n, name = "n_rats")


## ----echo = TRUE, eval = FALSE, echo = TRUE-----------------------------------
# clover <- readr::read_csv("data/clover.csv")


## ----echo = TRUE, eval = TRUE, echo = FALSE-----------------------------------
clover <- readr::read_csv("../data/clover.csv")


## -----------------------------------------------------------------------------
clover <- clover |>
    mutate(
        fspecies = factor(species),
        freplication = factor(replication),
        ftime = factor(time)
    )
library("lme4")


## -----------------------------------------------------------------------------
clover


## -----------------------------------------------------------------------------
clover |> glimpse()


## -----------------------------------------------------------------------------
clover_glmm1 <- glmer(cbind(nhard, nsoft) ~ time + (time | fspecies) +
        (1 | fspecies:ftime) + (1 | ftime:freplication),
    family = binomial(link = "logit"), data = clover)


## -----------------------------------------------------------------------------
library("glmmTMB")
clover_tmb1 <- glmmTMB(cbind(nhard, nsoft) ~ time + (time | fspecies) +
        (1 | fspecies:ftime) + (1 | ftime:freplication),
    family = binomial(link = "logit"), data = clover)


## ----plot-gamm-dharma, fig.align="center", out.width="75%"--------------------
library("mgcv")
chick_gam1 <- bam(weight ~ Diet + s(Time, by = Diet, k = 5) + s(Time, Chick, bs = "fs", k = 5),
    data = ChickWeight, method = "fREML")
sim_gam1 <- simulateResiduals(chick_gam1, plot = FALSE)
plot(sim_gam1)


## ----chick-gam-model-1, fig.align="center", out.width="75%"-------------------
chick_gam2 <- bam(weight ~ Diet + s(Time, by = Diet, k = 5) + s(Time, Chick, bs = "fs", k = 5),
    data = ChickWeight, method = "fREML", family = Gamma(link = "log"))
sim_gam2 <- simulateResiduals(chick_gam2, plot = FALSE)
plot(sim_gam2)


## ----chick-gam-model-2, fig.align="center", out.width="75%"-------------------
chick_gam3 <- bam(weight ~ Diet + s(Time, by = Diet, k = 5) + s(Time, Chick, bs = "fs", k = 5),
    data = ChickWeight, method = "fREML", family = tw(link = "log"))
sim_gam3 <- simulateResiduals(chick_gam3, plot = FALSE)
plot(sim_gam3)


## ----chick-gam-model-3, fig.align="center", out.width="75%"-------------------
chick_gam3 |> plot_predictions(condition = c("Time", "Diet"),
    exclude = "s(Time,Chick)")


## ----model-diagnostics-glmm, fig.width = 1.777777 * 12, fig.height = 12, out.width = "85%", fig.align = "center"----
library("performance")
check_model(chick_glmm1)

