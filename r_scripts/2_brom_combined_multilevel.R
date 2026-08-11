#### Packages ####
library(lme4) # for the analysis
library(haven) # to load the SPSS .sav file
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(lmerTest) # to get p-value estimations that are not part of the standard lme4 packages
library(performance) # to calcualte ICC (replaces sjstats::icc())

#### Data ####
brom_combined <- read_rds("data/brom_combined.rds")

brom_combined %>%
  group_by(team) %>%
  count() %>%
  View

brom_combined_centered <-
  brom_combined %>%
  mutate(npi_mean = npi_mean - mean(brom_combined$npi_mean, na.rm = T),
         cni_mean = cni_mean - mean(brom_combined$cni_mean, na.rm = T),
         igc_proc = igc_proc - mean(brom_combined$igc_proc, na.rm = T),
         igc_rel = igc_rel - mean(brom_combined$igc_rel, na.rm = T),
         igc_task = igc_task - mean(brom_combined$igc_task, na.rm = T),
         geq_atg_s = geq_atg_s - mean(brom_combined$geq_atg_s, na.rm = T),
         geq_atg_t = geq_atg_t - mean(brom_combined$geq_atg_t, na.rm = T),
         geq_gi_s = geq_gi_s - mean(brom_combined$geq_gi_s, na.rm = T),
         geq_gi_t = geq_gi_t - mean(brom_combined$geq_gi_t, na.rm = T)) %>%
  left_join(brom_combined %>%
              group_by(team) %>%
              summarise(team_npi = mean(npi_mean, na.rm = T),
                        team_cni = mean(cni_mean, na.rm = T)))

colnames(brom_combined)

##### igc_proc #####

#### Plotting data ####

## scatter
ggplot(data  = brom_combined,
       aes(x = npi_mean,
           y = igc_proc))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
  theme_minimal()+
  labs(title = "NPI narcissism vs. Group conflict")

## Scatter with regression line
ggplot(data  = brom_combined,
       aes(x = npi_mean,
           y = igc_proc))+
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
ggplot(data  = brom_combined,
       aes(x = npi_mean,
           y = igc_proc,
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

brom_combined_intercept <- lmer(formula = igc_proc ~ 1 + (1|team),
                        data = brom_combined_centered)
summary(brom_combined_intercept)
icc(brom_combined_intercept)

#### First level predictors ####

brom_combined_mlm1 <- lmer(igc_proc ~ 1 + npi_mean*cni_mean +(1|team),
                   data = brom_combined_centered)
summary(brom_combined_mlm1)

#### First and second level predictors ####

brom_combined_mlm2 <- lmer(igc_proc ~ 1 + npi_mean*cni_mean + team_npi +
                     (1|team),
                   data = brom_combined_centered)
summary(brom_combined_mlm2)

# Variance explained
interceptonlymodel_var <- as.data.frame(VarCorr(brom_combined_intercept)) # VarCorr() to extract random effect variances and std devs
model2_var <- as.data.frame(VarCorr(brom_combined_mlm2))
# level 1
(interceptonlymodel_var$vcov[2] - model2_var$vcov[2]) / interceptonlymodel_var$vcov[2]
# level 2
(interceptonlymodel_var$vcov[1] - model2_var$vcov[1]) / interceptonlymodel_var$vcov[1]

#### First and second level predictors with random slopes (1) ####
brom_combined_mlm3 <- lmer(igc_proc ~ 1 + npi_mean*cni_mean + team_npi +
                     (1 + npi_mean*cni_mean + team_npi|team),
                   data = brom_combined_centered)
summary(brom_combined_mlm3)
ranova(brom_combined_mlm3)
