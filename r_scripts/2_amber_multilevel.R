#### Packages ####
library(lme4) # for the analysis
library(haven) # to load the SPSS .sav file
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(lmerTest) # to get p-value estimations that are not part of the standard lme4 packages
library(performance) # to calcualte ICC (replaces sjstats::icc())

#### Data ####
amber <- read_rds("data/amber.rds")
amber_centered <-
  amber %>%
  mutate(npi_mean = npi_mean - mean(amber$npi_mean, na.rm = T),
         cni_mean = cni_mean - mean(amber$cni_mean, na.rm = T),
         ggc = ggc - mean(amber$ggc, na.rm = T)) %>%
  left_join(amber %>%   group_by(team) %>%
              summarise(team_npi = mean(npi_mean)))

#### Plotting data ####

## scatter
ggplot(data  = amber,
       aes(x = npi_mean,
           y = ggc))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
  theme_minimal()+
  labs(title = "NPI narcissism vs. Group conflict")

## Scatter with regression line
ggplot(data  = amber,
       aes(x = npi_mean,
           y = ggc))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
  geom_smooth(method = lm,
              se     = FALSE,
              col    = "black",
              size   = .5,
              alpha  = .8)+ # to add regression line
  theme_minimal()+
  labs(title = "NPI narcissism vs. Group conflict",
       subtitle = "add regression line")

## Scatter coloured by team
ggplot(data  = amber,
       aes(x = npi_mean,
           y = ggc,
           col = as.numeric(team),
           group = team)) +
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter") +# to add some random noise for plotting purposes
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(45)) +
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5,
              alpha  = .8) + # to add regression line
  labs(title = "NPI narcissism vs. Group conflict",
       subtitle = "add colours and regression lines for different teams")

##### Analysis ####

#### Intercept only model ####

amber_intercept <- lmer(formula = ggc ~ 1 + (1|team),
                        data = amber_centered)
summary(amber_intercept)
icc(amber_intercept)

#### First level predictors ####

amber_mlm1 <- lmer(ggc ~ 1 + npi_mean*cni_mean +(1|team),
                   data = amber_centered)
summary(amber_mlm1)

#### First and second level predictors ####

amber_mlm2 <- lmer(ggc ~ 1 + npi_mean*cni_mean + team_npi +
                     (1|team),
                   data = amber_centered)
summary(amber_mlm2)

# Variance explained
interceptonlymodel_var <- as.data.frame(VarCorr(amber_intercept)) # VarCorr() to extract random effect variances and std devs
model2_var <- as.data.frame(VarCorr(amber_mlm2))
# level 1
(interceptonlymodel_var$vcov[2] - model2_var$vcov[2]) / interceptonlymodel_var$vcov[2]
# level 2
(interceptonlymodel_var$vcov[1] - model2_var$vcov[1]) / interceptonlymodel_var$vcov[1]

#### First and second level predictors with random slopes (1) ####
amber_mlm3 <- lmer(ggc ~ 1 + npi_mean*cni_mean + team_npi +
                     (1 + npi_mean*cni_mean + team_npi|team),
                   data = amber_centered)
summary(amber_mlm3)
ranova(amber_mlm3)
