#### Packages ####
library(lme4) # for the analysis
library(haven) # to load the SPSS .sav file
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(lmerTest) # to get p-value estimations that are not part of the standard lme4 packages
library(performance) # to calcualte ICC (replaces sjstats::icc())

#### Data ####
toby_matt <- read_rds("data/toby_matt.rds")

toby_matt %>% Amelia::missmap()


toby_matt_centered <-
  toby_matt %>%
  mutate(npi_mean_1 = npi_mean_1 - mean(toby_matt$npi_mean_1, na.rm = T),
         cni_mean_1 = cni_mean_1 - mean(toby_matt$cni_mean_1, na.rm = T),
         geq_atg_s_1 = geq_atg_s_1 - mean(toby_matt$geq_atg_s_1, na.rm = T),
         geq_atg_t_1 = geq_atg_t_1 - mean(toby_matt$geq_atg_t_1, na.rm = T),
         geq_gi_s_1 = geq_gi_s_1 - mean(toby_matt$geq_gi_s_1, na.rm = T)) %>%
  left_join(toby_matt %>%
              group_by(team) %>%
              summarise(team_npi = mean(npi_mean_1, na.rm = T),
                        team_cni = mean(cni_mean_1, na.rm = T)))

colnames(toby_matt)

##### geq_atg_s_1 #####

#### Plotting data ####

## scatter
ggplot(data  = toby_matt,
       aes(x = npi_mean_1,
           y = geq_atg_s_1))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
  theme_minimal()+
  labs(title = "NPI narcissism vs. Group conflict")

## Scatter with regression line
ggplot(data  = toby_matt,
       aes(x = npi_mean_1,
           y = geq_atg_s_1))+
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
ggplot(data  = toby_matt,
       aes(x = npi_mean_1,
           y = geq_atg_s_1,
           col = as.numeric(team),
           group = team)) +
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter") +# to add some random noise for plotting purposes
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(44)) +
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5,
              alpha  = .8) + # to add regression line
  labs(title = "NPI narcissism vs. Group conflict",
       subtitle = "add colours and regression lines for different teams")

##### Analysis ####

#### Intercept only model ####

toby_matt_intercept <- lmer(formula = geq_atg_s_1 ~ 1 + (1|team),
                        data = toby_matt_centered)
summary(toby_matt_intercept)
icc(toby_matt_intercept)

#### First level predictors ####

toby_matt_mlm1 <- lmer(geq_atg_s_1 ~ 1 + npi_mean_1*cni_mean_1 +(1|team),
                   data = toby_matt_centered)
summary(toby_matt_mlm1)

#### First and second level predictors ####

toby_matt_mlm2 <- lmer(geq_atg_s_1 ~ 1 + npi_mean_1*cni_mean_1 + team_npi +
                     (1|team),
                   data = toby_matt_centered)
summary(toby_matt_mlm2)

# Variance explained
interceptonlymodel_var <- as.data.frame(VarCorr(toby_matt_intercept)) # VarCorr() to extract random effect variances and std devs
model2_var <- as.data.frame(VarCorr(toby_matt_mlm2))
# level 1
(interceptonlymodel_var$vcov[2] - model2_var$vcov[2]) / interceptonlymodel_var$vcov[2]
# level 2
(interceptonlymodel_var$vcov[1] - model2_var$vcov[1]) / interceptonlymodel_var$vcov[1]

#### First and second level predictors with random slopes (1) ####
toby_matt_mlm3 <- lmer(geq_atg_s_1 ~ 1 + npi_mean_1*cni_mean_1 + team_npi +
                     (1 + npi_mean_1*cni_mean_1 + team_npi|team),
                   data = toby_matt_centered)
summary(toby_matt_mlm3)
ranova(toby_matt_mlm3)

##### geq_atg_t_1 #####

#### Plotting data ####

## scatter
ggplot(data  = toby_matt,
       aes(x = npi_mean_1,
           y = geq_atg_t_1))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
  theme_minimal()+
  labs(title = "NPI narcissism vs. Group conflict")

## Scatter with regression line
ggplot(data  = toby_matt,
       aes(x = npi_mean_1,
           y = geq_atg_t_1))+
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
ggplot(data  = toby_matt,
       aes(x = npi_mean_1,
           y = geq_atg_t_1,
           col = as.numeric(team),
           group = team)) +
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter") +# to add some random noise for plotting purposes
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(44)) +
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5,
              alpha  = .8) + # to add regression line
  labs(title = "NPI narcissism vs. Group conflict",
       subtitle = "add colours and regression lines for different teams")

##### Analysis ####

#### Intercept only model ####

toby_matt_intercept <- lmer(formula = geq_atg_t_1 ~ 1 + (1|team),
                            data = toby_matt_centered)
summary(toby_matt_intercept)
icc(toby_matt_intercept)

#### First level predictors ####

toby_matt_mlm1 <- lmer(geq_atg_t_1 ~ 1 + npi_mean_1*cni_mean_1 +(1|team),
                       data = toby_matt_centered)
summary(toby_matt_mlm1)

#### First and second level predictors ####

toby_matt_mlm2 <- lmer(geq_atg_t_1 ~ 1 + npi_mean_1*cni_mean_1 + team_npi +
                         (1|team),
                       data = toby_matt_centered)
summary(toby_matt_mlm2)

# Variance explained
interceptonlymodel_var <- as.data.frame(VarCorr(toby_matt_intercept)) # VarCorr() to extract random effect variances and std devs
model2_var <- as.data.frame(VarCorr(toby_matt_mlm2))
# level 1
(interceptonlymodel_var$vcov[2] - model2_var$vcov[2]) / interceptonlymodel_var$vcov[2]
# level 2
(interceptonlymodel_var$vcov[1] - model2_var$vcov[1]) / interceptonlymodel_var$vcov[1]

#### First and second level predictors with random slopes (1) ####
toby_matt_mlm3 <- lmer(geq_atg_t_1 ~ 1 + npi_mean_1*cni_mean_1 + team_npi +
                         (1 + npi_mean_1*cni_mean_1 + team_npi|team),
                       data = toby_matt_centered)
summary(toby_matt_mlm3)
ranova(toby_matt_mlm3)


##### geq_gi_s_1 #####

#### Plotting data ####

## scatter
ggplot(data  = toby_matt,
       aes(x = npi_mean_1,
           y = geq_gi_s_1))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
  theme_minimal()+
  labs(title = "NPI narcissism vs. Group conflict")

## Scatter with regression line
ggplot(data  = toby_matt,
       aes(x = npi_mean_1,
           y = geq_gi_s_1))+
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
ggplot(data  = toby_matt,
       aes(x = npi_mean_1,
           y = geq_gi_s_1,
           col = as.numeric(team),
           group = team)) +
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter") +# to add some random noise for plotting purposes
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(44)) +
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5,
              alpha  = .8) + # to add regression line
  labs(title = "NPI narcissism vs. Group conflict",
       subtitle = "add colours and regression lines for different teams")

##### Analysis ####

#### Intercept only model ####

toby_matt_intercept <- lmer(formula = geq_gi_s_1 ~ 1 + (1|team),
                            data = toby_matt_centered)
summary(toby_matt_intercept)
icc(toby_matt_intercept)

#### First level predictors ####

toby_matt_mlm1 <- lmer(geq_gi_s_1 ~ 1 + npi_mean_1*cni_mean_1 +(1|team),
                       data = toby_matt_centered)
summary(toby_matt_mlm1)

#### First and second level predictors ####

toby_matt_mlm2 <- lmer(geq_gi_s_1 ~ 1 + npi_mean_1*cni_mean_1 + team_npi +
                         (1|team),
                       data = toby_matt_centered)
summary(toby_matt_mlm2)

# Variance explained
interceptonlymodel_var <- as.data.frame(VarCorr(toby_matt_intercept)) # VarCorr() to extract random effect variances and std devs
model2_var <- as.data.frame(VarCorr(toby_matt_mlm2))
# level 1
(interceptonlymodel_var$vcov[2] - model2_var$vcov[2]) / interceptonlymodel_var$vcov[2]
# level 2
(interceptonlymodel_var$vcov[1] - model2_var$vcov[1]) / interceptonlymodel_var$vcov[1]

#### First and second level predictors with random slopes (1) ####
toby_matt_mlm3 <- lmer(geq_gi_s_1 ~ 1 + npi_mean_1*cni_mean_1 + team_npi +
                         (1 + npi_mean_1*cni_mean_1 + team_npi|team),
                       data = toby_matt_centered)
summary(toby_matt_mlm3)
ranova(toby_matt_mlm3)

