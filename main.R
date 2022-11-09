library(haven)
library(here)
library(lme4)
library(emmeans)

mtl <- haven::read_sav(here("MTL_lateral.sav"))
colnames(mtl)


## Create binary logistic regression for congruency ~ culture.
## (1) Across all cultures, do people tend to make 
## congruent gestures more than 50% of the time?
congruency_model <- glmer(Congruency ~ num_CULTURE + (1 | Subject),
                          data = mtl, family = "binomial")

summary(congruency_model)
## Is there an interaction of field x level?
## Use an emmeans contrast
cong_emm <- congruency_model |> emmeans(~num_CULTURE)
cong_emm |> summary(infer = T)
summary(cong_emm, type = "response", response = T, infer = T)

cong_emm |>
  contrast("pairwise") |>
  summary(infer = T, adjust = "none", type = "response")

