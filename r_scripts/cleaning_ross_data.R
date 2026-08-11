#### Packages ####
library(tidyverse)
library(haven)
library(DataExplorer)
library(psych)
library(tidyselect)

#### Read raw data in ####
amber_raw <- read_sav("raw_data/Amber et al bsc data.sav")
brom_msc_raw <- read_sav("raw_data/Brom Sibson Full Data Set MSC.sav")
brom_combined_raw <- read_sav("raw_data/Combined Brom's BSC and MSC.sav") %>%
  mutate(CNI = ifelse(CNI == 999, NA, CNI))
danielle_raw <- read_sav("raw_data/Danielle Murphy SPSS Data.sav")
dan_james_raw <- read_sav("raw_data/SPSS Data set with the removal of NPI and CNI inventory check items Dan and James (1) (1).sav")
toby_matt_raw <- read_sav("raw_data/TobyMatt Master.sav")

#### Clean amber ####
amber <-
  amber_raw %>%
  janitor::clean_names() %>% # all colnames to snake case
  select(-c(ends_with("mean"))) %>%
  # creating factor variables
  transmute(coder = as.factor(coder),
            age = as.numeric(age),
            sex = as.factor(case_when(sex == 1 ~ "Male",
                                      sex == 2 ~ "Female")),
            sport = as_factor(sport),
            team = as_factor(team_name),
            level = as_factor(level),
            starting_status = as_factor(starting_status),
            coach = case_when(coach == 1 ~ TRUE,
                              coach == 2 ~ FALSE),
            captain_name = as_factor(captain_name),
            last_result = as_factor(last_result),
            setting = as_factor(setting),
            # numeric columns
            across(.cols = ("experience" | "tenure" | "training" |
                              starts_with("panas") | starts_with("grp_") |
                              starts_with("cni") | starts_with("npi") |
                              starts_with("tipi") | starts_with("social_des")),
                   as.numeric),
            # removing inappropriate values
            across(paste0("panas", 1:10),
                   .fns = ~ifelse(between(.x, 1, 5), .x, NA)),
            across(paste0("npi", 1:16),
                   .fns = ~ifelse(between(.x, 1, 2), .x, NA)),
            across(paste0("grp_conf", 1:17),
                   .fns = ~ifelse(between(.x, 1, 9), .x, NA)),
            across(paste0("grp_goals", 1:4),
                   .fns = ~ifelse(between(.x, 1, 6), .x, NA)),
            across(paste0("cni", 1:16),
                   .fns = ~ifelse(between(.x, 1, 7), .x, NA)),
            across(paste0("tipi", 1:10),
                   .fns = ~ifelse(between(.x, 1, 7), .x, NA)),
            # reverse scoring and recoding items
            npi1recode = if_else(npi1 == 1, 1, 0),
            npi2recode = if_else(npi2 == 2, 1, 0),
            npi3recode = if_else(npi3 == 1, 1, 0),
            npi4recode = if_else(npi4 == 2, 1, 0),
            npi5recode = if_else(npi5 == 2, 1, 0),
            npi6recode = if_else(npi6 == 1, 1, 0),
            npi7recode = if_else(npi7 == 2, 1, 0),
            npi8recode = if_else(npi8 == 1, 1, 0),
            npi9recode = if_else(npi9 == 1, 1, 0),
            npi10recode = if_else(npi10 == 2, 1, 0),
            npi11recode = if_else(npi11 == 1, 1, 0),
            npi12recode = if_else(npi12 == 2, 1, 0),
            npi13recode = if_else(npi13 == 2, 1, 0),
            npi14recode = if_else(npi14 == 1, 1, 0),
            npi15recode = if_else(npi15 == 2, 1, 0),
            npi16recode = if_else(npi16 == 1, 1, 0),
            # calculating mean scores
            npi_mean = (npi1recode + npi2recode + npi3recode + npi4recode +
                          npi5recode + npi6recode + npi7recode + npi8recode +
                          npi9recode + npi10recode + npi11recode + npi12recode +
                          npi13recode + npi14recode + npi15recode +
                          npi16recode)/16,
            cni_mean = (cni1 + cni2 + cni3 + cni4 + cni5 + cni6 + cni7 + cni8 +
                          cni9 + cni10 + cni11 + cni12 + cni13 + cni14 + cni15 +
                          cni16)/16,
            open = (tipi5 +(8-tipi10))/2,
            consc = (tipi3 +(8-tipi8))/2,
            extrv = (tipi1 +(8-tipi6))/2,
            agree = (tipi2 +(8-tipi7))/2,
            emot_stab = (tipi4 +(8-tipi9))/2,
            grp_conf_mean = (grp_conf1 + grp_conf2 + grp_conf3 + grp_conf4 +
                               grp_conf5 + grp_conf6 + grp_conf7 + grp_conf8 +
                               grp_conf9 + grp_conf10 + grp_conf11 +
                               grp_conf12 + grp_conf13 + grp_conf14 +
                               grp_conf15 + grp_conf16 + grp_conf17)/17,
            ggc_mean = (grp_goals1 + (7 - grp_goals2) + grp_goals3 + grp_goals4)/4,
            igc_proc = (grp_conf6 + grp_conf9 + grp_conf12 + grp_conf14)/4,
            igc_rel = (grp_conf1 + grp_conf4 + grp_conf7 + grp_conf10)/4,
            igc_task = (grp_conf2 + grp_conf8 + grp_conf11)/3
  ) %>%
  tibble()

amber_raw %>%
  select(starts_with("grp_conf"), ZTaskConf, Relatioconf) %>%
  cor.plot()

colnames(amber_raw)

#### Clean brom_combined ####
brom_combined <-
  brom_combined_raw %>%
  janitor::clean_names() %>%
  transmute(age = age,
            sex = as_factor(sex),
            sport = as_factor(sport),
            team = as_factor(team),
            captain = if_else(captain_ot_not == 1, TRUE, FALSE),

            # numeric columns
            cni_mean = as.numeric(cni),
            npi_mean = as.numeric(npi_mean),
            gpc_mean = as.numeric(gpc_mean),

            # removing inappropriate values
            across(paste0("geq", 1:18, "_2"),
                   .fns = ~ifelse(between(.x, 1, 9), .x, NA)),
            across(paste0("igc", 1:14),
                   .fns = ~ifelse(between(.x, 1, 9), .x, NA)),

            # calculating mean scores
              # Process conflict
            igc_proc = (igc6 + igc9 + igc12 + igc14)/4,
              # Relationship conflict
            igc_rel = (igc1 + igc4 + igc7 + igc10)/4,
              # Task conflict
            igc_task = (igc2 + igc8 +igc11)/3,
              # Individual Attractions to the Group–Social (ATG-S)
            geq_atg_s = (geq1_2 + geq3_2 + geq5_2 + geq7_2 + geq9_2)/5,
              # Individual Attractions to the Group–Task (ATG-T)
            geq_atg_t = (geq2_2 + geq4_2 + geq6_2 + geq8_2)/4,
              # Group Integration–Social (GI-S)
            geq_gi_s = (geq11_2 + geq13_2 + geq15_2 + geq17_2)/4,
              # Group Integration–Task (GI-T)
            geq_gi_t = (geq10_2 + geq12_2 + geq14_2 + geq16_2 + geq18_2)/5) %>%
  tibble()


#### Clean brom_msc ####
brom_msc <-
  brom_msc_raw %>%
  janitor::clean_names() %>%
  # creating factor variables
  transmute(age = as.numeric(age),
            sex = as.factor(case_when(sex == 1 ~ "Male",
                                      sex == 2 ~ "Female")),
            sport = as_factor(sport),
            team = as_factor(team),
            # numeric columns
            across(.cols = (starts_with("group_goal_commitment") |
                              starts_with("cni") | starts_with("npi") |
                              starts_with("igc")),
                   as.numeric),
            # removing inappropriate values
            across(paste0("npi", 1:16),
                   .fns = ~ifelse(between(.x, 1, 2), .x, NA)),
            across(paste0("igc", 1:14),
                   .fns = ~ifelse(between(.x, 1, 9), .x, NA)),
            across(paste0("group_goal_commitment", 1:4),
                   .fns = ~ifelse(between(.x, 1, 6), .x, NA)),
            across(paste0("cni", 1:16),
                   .fns = ~ifelse(between(.x, 1, 7), .x, NA)),
            # reverse scoring and recoding items
            npi1recode = if_else(npi1 == 1, 1, 0),
            npi2recode = if_else(npi2 == 2, 1, 0),
            npi3recode = if_else(npi3 == 1, 1, 0),
            npi4recode = if_else(npi4 == 2, 1, 0),
            npi5recode = if_else(npi5 == 2, 1, 0),
            npi6recode = if_else(npi6 == 1, 1, 0),
            npi7recode = if_else(npi7 == 2, 1, 0),
            npi8recode = if_else(npi8 == 1, 1, 0),
            npi9recode = if_else(npi9 == 1, 1, 0),
            npi10recode = if_else(npi10 == 2, 1, 0),
            npi11recode = if_else(npi11 == 1, 1, 0),
            npi12recode = if_else(npi12 == 2, 1, 0),
            npi13recode = if_else(npi13 == 2, 1, 0),
            npi14recode = if_else(npi14 == 1, 1, 0),
            npi15recode = if_else(npi15 == 2, 1, 0),
            npi16recode = if_else(npi16 == 1, 1, 0),
            # calculating mean scores
            npi_mean = (npi1recode + npi2recode + npi3recode + npi4recode +
                          npi5recode + npi6recode + npi7recode + npi8recode +
                          npi9recode + npi10recode + npi11recode + npi12recode +
                          npi13recode + npi14recode + npi15recode +
                          npi16recode)/16,
            cni_mean = (cni1 + cni2 + cni3 + cni4 + cni5 + cni6 + cni7 + cni8 +
                          cni9 + cni10 + cni11 + cni12 + cni13 + cni14 + cni15 +
                          cni16)/16,
            ggc_mean = (group_goal_commitment1 + (7 - group_goal_commitment2) +
                     group_goal_commitment3 + group_goal_commitment4)/4,
            igc_proc = (igc6 + igc9 + igc12 + igc14)/4,
            igc_rel = (igc1 + igc4 + igc7 + igc10)/4,
            igc_task = (igc2 + igc8 + igc11)/3
  ) %>%
  tibble()

colnames(brom_msc)

#### Clean toby_matt ####
toby_matt <-
  toby_matt_raw %>%
  janitor::clean_names() %>%
  transmute(age = as.numeric(age),
            sex = factor(sex, levels = 1:2, labels = c("Male", "Female")),
            sport = as_factor(sport),
            team_name = as_factor(team_name),
            team = as_factor(team), # seems to be an id for the team name
            team_npi = team_npi, # presumably mean team npi from one of the time points? CHECK
            player_coach = if_else(player_coach == 1, TRUE, FALSE),
            comp_level = as_factor(comp_level),
            time_at_club = as.numeric(time_at_club),
            captain = if_else(captain_or_not == 1, TRUE, FALSE),
            # Fixing labels
            npi1 = npi_1,
            npi2 = npi_2,
            npi3 = npi_3,
            npi4 = npi_4,
            npi5 = npi_5,
            npi6 = npi_6,
            npi7 = npi_7,
            npi8 = npi_8,
            ics1 = ics_1,
            # Numeric columns
            across(.cols = (starts_with("npi") | starts_with("cni") |
                              starts_with("geq") | starts_with("ics") |
                              starts_with("tipi") | starts_with("ggc")),
                   as.numeric)) %>%
  # Remove unwanted/unclear variables
  select(-c(paste0("npi_", 1:8), "cni", "npi_agg", "npi", ics_1)) %>%
  rename_with(~str_c(., "_1"), # add time point label to t1 vars
              .cols = c(paste0("npi", 1:40), paste0("cni", 1:16),
                        paste0("geq", 1:18), paste0("ics", 1:17),
                        paste0("ggc", 1:4), paste0("tipi", 1:10))) %>%
  mutate(
    # Calculate mean score variables
    open_1 = (tipi5_1 +(8-tipi10_1))/2,
    consc_1 = (tipi3_1 +(8-tipi8_1))/2,
    extrv_1 = (tipi1_1 +(8-tipi6_1))/2,
    agree_1 = (tipi2_1 +(8-tipi7_1))/2,
    emot_stab_1 = (tipi4_1 +(8-tipi9_1))/2,
    npi_mean_1 = (npi1_1 + npi2_1 + npi3_1 + npi4_1 + npi5_1 + npi6_1 +
                    npi7_1 + npi8_1 + npi9_1 + npi10_1 + npi11_1 +
                    npi12_1 + npi13_1 + npi14_1 + npi15_1 + npi16_1 +
                    npi17_1 + npi18_1 + npi19_1 + npi20_1 + npi21_1 +
                    npi22_1 + npi23_1 + npi24_1 + npi25_1 + npi26_1 +
                    npi27_1 + npi28_1 + npi29_1 + npi30_1 + npi31_1 +
                    npi32_1 + npi33_1 + npi34_1 + npi35_1 + npi36_1 +
                    npi37_1 + npi38_1 + npi39_1 + npi40_1)/40,
    cni_mean_1 = (cni1_1 + cni2_1 + cni3_1 + cni4_1 + cni5_1 + cni6_1 +
                    cni7_1 + cni8_1 + cni9_1 + cni10_1 + cni11_1 +
                    cni12_1 + cni13_1 + cni14_1 + cni15_1 + cni16_1)/16,
    # Individual Attractions to the Group–Social (ATG-S)
    geq_atg_s_1 = (geq1_1 + geq3_1 + geq5_1 + geq7_1 + geq9_1)/5,
    # Individual Attractions to the Group–Task (ATG-T)
    geq_atg_t_1 = (geq2_1 + geq4_1 + geq6_1 + geq8_1)/4,
    # Group Integration–Social (GI-S)
    geq_gi_s_1 = (geq11_1 + geq13_1 + geq15_1 + geq17_1)/4,
    # Group Integration–Task (GI-T)
    geq_gi_t_1 = (geq10_1 + geq12_1 + geq14_1 + geq16_1 + geq18_1)/5,
    # Group goal commitment
    ggc_1 = ggc1_1 + (7 - ggc2_1) + ggc3_1 + ggc4_1,
    open_2 = (tipi5_2 +(8-tipi10_2))/2,
    consc_2 = (tipi3_2 +(8-tipi8_2))/2,
    extrv_2 = (tipi1_2 +(8-tipi6_2))/2,
    agree_2 = (tipi2_2 +(8-tipi7_2))/2,
    emot_stab_2 = (tipi4_2 +(8-tipi9_2))/2,
    npi_mean_2 = (npi1_2 + npi2_2 + npi3_2 + npi4_2 + npi5_2 + npi6_2 +
                    npi7_2 + npi8_2 + npi9_2 + npi10_2 + npi11_2 +
                    npi12_2 + npi13_2 + npi14_2 + npi15_2 + npi16_2 +
                    npi17_2 + npi18_2 + npi19_2 + npi20_2 + npi21_2 +
                    npi22_2 + npi23_2 + npi24_2 + npi25_2 + npi26_2 +
                    npi27_2 + npi28_2 + npi29_2 + npi30_2 + npi31_2 +
                    npi32_2 + npi33_2 + npi34_2 + npi35_2 + npi36_2 +
                    npi37_2 + npi38_2 + npi39_2 + npi40_2)/40,
    # Individual Attractions to the Group–Social (ATG-S)
    geq_atg_s_2 = (geq1_2 + geq3_2 + geq5_2 + geq7_2 + geq9_2)/5,
    # Individual Attractions to the Group–Task (ATG-T)
    geq_atg_t_2 = (geq2_2 + geq4_2 + geq6_2 + geq8_2)/4,
    # Group Integration–Social (GI-S)
    geq_gi_s_2 = (geq11_2 + geq13_2 + geq15_2 + geq17_2)/4,
    # Group Integration–Task (GI-T)
    geq_gi_t_2 = (geq10_2 + geq12_2 + geq14_2 + geq16_2 + geq18_2)/5,
    # Group goal commitment
    ggc_2 = ggc1_2 + (7 - ggc2_2) + ggc3_2 + ggc4_2) %>%
  tibble()


#### Merged data sets ####

amber_brom_msc_merged <-
bind_rows(amber, brom_msc) %>%
  select(sport, team, sex, npi_mean, cni_mean, ggc_mean,
         igc_proc, igc_rel, igc_task)

#### Write to rds ####
write_rds(amber, file = "data/amber.rds")
write_rds(amber_brom_msc_merged, file = "data/amber_brom_msc_merged.rds")
write_rds(brom_combined, file = "data/brom_combined.rds")
write_rds(brom_msc, file = "data/brom_msc.rds")
write_rds(toby_matt, file = "data/toby_matt.rds")

