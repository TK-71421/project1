library(tidyverse) 

# exploratory data analysis 
data = read_csv("nfl/raw_data/standings.csv")

data$Tm = gsub("[\\*\\+]", "", data$Tm)
data$Tm[data$Tm == "Oakland Raiders"] = "Las Vegas Raiders"
data$Tm[data$Tm == "St. Louis Rams"] = "Los Angeles Rams"
data$Tm[data$Tm == "San Diego Chargers"] = "Los Angeles Chargers"
data$Tm[data$Tm == "Washington Redskins"] = "Washington Commanders"
data$Tm[data$Tm == "Washington Football Team"] = "Washington Commanders"

# read in playoff data to create additional features
playoff_data = read_csv("nfl/raw_data/playoffs.csv")

#standardize team names to match standings.csv
playoff_data$`Winner/tie`[playoff_data$`Winner/tie` == "Oakland Raiders"] <- "Las Vegas Raiders"
playoff_data$`Loser/tie`[playoff_data$`Loser/tie` == "Oakland Raiders"]  <- "Las Vegas Raiders"

playoff_data$`Winner/tie`[playoff_data$`Winner/tie` == "St. Louis Rams"] <- "Los Angeles Rams"
playoff_data$`Loser/tie`[playoff_data$`Loser/tie` == "St. Louis Rams"]  <- "Los Angeles Rams"

playoff_data$`Winner/tie`[playoff_data$`Winner/tie` == "San Diego Chargers"] <- "Los Angeles Chargers"
playoff_data$`Loser/tie`[playoff_data$`Loser/tie` == "San Diego Chargers"]  <- "Los Angeles Chargers"

playoff_data$`Winner/tie`[playoff_data$`Winner/tie` == "Washington Redskins"] <- "Washington Commanders"
playoff_data$`Loser/tie`[playoff_data$`Loser/tie` == "Washington Redskins"]  <- "Washington Commanders"

playoff_data$`Winner/tie`[playoff_data$`Winner/tie` == "Washington Football Team"] <- "Washington Commanders"
playoff_data$`Loser/tie`[playoff_data$`Loser/tie` == "Washington Football Team"]  <- "Washington Commanders"

#adding made_playoffs & and made_superbowl binary features from playoffs.csv
playoff_teams = playoff_data %>%
  select(Date, `Winner/tie`, `Loser/tie`) %>%
  mutate(Year = as.numeric(substr(Date, 1, 4))) %>% # extract season year from date
  pivot_longer(cols = c(`Winner/tie`, `Loser/tie`), values_to = "Tm") %>%
  distinct(Year, Tm) %>%
  mutate(made_playoffs = 1)

superbowl_teams = playoff_data %>%
  filter(grepl("SuperBowl", Week)) %>%
  mutate(Year = as.numeric(substr(Date, 1, 4))) %>% # extract season year from date
  pivot_longer(cols = c(`Winner/tie`, `Loser/tie`), values_to = "Tm") %>%
  distinct(Year, Tm) %>%
  mutate(made_superbowl = 1)

# left join new features into standings data
data = data %>%
  left_join(playoff_teams, by = c("year" = "Year", "Tm")) %>%
  left_join(superbowl_teams, by = c("year" = "Year", "Tm"))

#impute 0's for unmatched teams (did not make playoffs or superbowl)
data = data %>% 
  mutate(
    made_playoffs = ifelse(is.na(made_playoffs), 0, made_playoffs),
    made_superbowl = ifelse(is.na(made_superbowl), 0, made_superbowl)
  )

#explore correlation between these new features and win percent
ggplot(data, aes(x=made_playoffs, y=pct)) + 
  geom_point()

ggplot(data, aes(x=made_superbowl, y=pct)) + 
  geom_point()
# no clear trend that teams who made playoffs/superbowl tend to have higher win percentage

# high same-season correlation between PD and win pct 
ggplot(data, aes(x=PD, y=pct)) + 
  geom_point() 

# what about past year PD vs. current year win pct? 
calculate_past_metrics = function(data, gap) {

  pd = data %>% select(Tm, year, PD, pct, made_playoffs, made_superbowl) 
  data = data %>% 
    left_join(
      pd, by = "Tm", 
      relationship = "many-to-many", 
      suffix = c("_cur", "_past")
    ) %>% 
    filter(year_cur == year_past + gap) 

  return(data)
}

data_past = calculate_past_metrics(data, gap=1)

# this correlation is not as strong (but still exists)
ggplot(data_past, aes(x=PD_past, y=pct_cur)) + 
  geom_point() 

m1 = lm(pct_cur ~ PD_past, data = data_past) 
summary(m1)
# past PD explains roughly 16% of the variance in win pct 

# being in playoffs/superbowl last year vs win pct
m1b = lm(pct_cur ~ made_superbowl_past, data = data_past)
summary(m1b)

m1c = lm(pct_cur ~ made_playoffs_past, data = data_past)
summary(m1c)
# these features on their own don't have much predictive power; very poor correlation (as seen by scatterplots)
# made_superbowl seems insignificant

# does past win pct do better? 
m2 = lm(pct_cur ~ pct_past, data = data_past)
summary(m2) 
# actually does worse! only about 13% 

# try combining the two 
m3 = lm(pct_cur ~ PD_past + pct_past, data = data_past) 
summary(m3)
# pct_past no longer significant after accounting for PD_past 

# what about current year - 2? 
data_past2 = calculate_past_metrics(data, gap=2)
m4 = lm(pct_cur ~ PD_past, data = data_past2) 
summary(m4) 
# relationship less strong, but still significant 


# test for playoffs/superbowl as well
m4a = lm(pct_cur ~ made_playoffs_past, data = data_past2)
summary(m4a)

m4b = lm(pct_cur ~ made_superbowl_past, data = data_past2)
summary(m4b)
# made_superbowl seems insignificant
# made_playoffs 2 years past seems more significant than 1 year

data_past3 = calculate_past_metrics(data, gap=3)
m5 = lm(pct_cur ~ PD_past, data = data_past3) 
summary(m5)
# current year - 3 is no longer significant 
# but still might be worth including 

#looking @ the same for made_playoffs/superbowl
m5a = lm(pct_cur ~ made_playoffs_past, data = data_past3)
summary(m5a)

m5b = lm(pct_cur ~ made_superbowl_past, data = data_past3)
summary(m5b)
# made_superbowl again worse than guessing/insignificant
# made_playoffs seems to get more significant

# what if we tried everything together? 
# this is the jankiest function ever... but it works 
calculate_all_past = function(data) {
  pd = data %>% select(Tm, year, PD, made_superbowl, made_playoffs) 
   data = data %>% 
    left_join(
      pd, by = "Tm", 
      relationship = "many-to-many", 
      suffix = c("_cur", "_past")
    ) %>% 
    filter(year_past + 3 >= year_cur & year_past != year_cur) %>% 
    mutate(
      year_indicator = case_when(
        year_past + 1 == year_cur ~ '1', 
        year_past + 2 == year_cur ~ '2', 
        year_past + 3 == year_cur ~ '3'
      ) 
    ) %>% 
    drop_na(year_indicator) %>% 
    pivot_wider(
      names_from = year_indicator,
      values_from = c(PD_past, made_playoffs_past, made_superbowl_past),
      values_fill =  list(PD_past = NA, made_playoffs_past = NA, made_superbowl_past = NA) # ensure columns exist if no match
    ) %>%
    group_by(year_cur, Tm) %>% 
    summarize(
      W = mean(W), 
      L = mean(L), 
      win_pct = mean(pct), 
      across(starts_with("PD_past"), ~mean(.x, na.rm = TRUE)),
      across(starts_with("made_playoffs_past"), 
             ~ifelse(all(is.na(.x)), 0, max(.x, na.rm = TRUE))),
      across(starts_with("made_superbowl_past"), 
             ~ifelse(all(is.na(.x)), 0, max(.x, na.rm = TRUE))),
      .groups = "drop"
      
      # tweaking with code, adding the new features below that way breaks things
      
      #past_1 = mean(past_1, na.rm = TRUE), 
      #past_2 = mean(past_2, na.rm = TRUE), 
      #past_3 = mean(past_3, na.rm = TRUE),
      
      #add playoff lookbacks
      #playoffs_past_1 = mean(made_playoffs_past_1, na.rm = TRUE),
      #playoffs_past_2 = mean(made_playoffs_past_2, na.rm = TRUE),
      #playoffs_past_3 = mean(made_playoffs_past_3, na.rm = TRUE),
      
      # add superbowl lookbacks
      #superbowl_past_1 = mean(made_superbowl_past_1, na.rm = TRUE),
      #superbowl_past_2 = mean(made_superbowl_past_2, na.rm = TRUE),
      #superbowl_past_3 = mean(made_superbowl_past_3, na.rm = TRUE)
    ) %>% 
    filter(year_cur >= 2018) %>% #restrict to >= 2018 to ensure data exists for 3 years past 
    drop_na() 
  
  return(data)
}

data_all_past = calculate_all_past(data) 

m6 = lm(win_pct ~ PD_past_1 + PD_past_2 + PD_past_3, data = data_all_past) 
summary(m6)
# interestingly only the previous year matters

m6a = lm(win_pct ~ PD_past_1 + PD_past_2 + PD_past_3
                  + made_playoffs_past_1 + made_playoffs_past_2 + made_playoffs_past_3
                  + made_superbowl_past_1 + made_superbowl_past_2 + made_superbowl_past_3,
        data = data_all_past) 
summary(m6a)
# including prior made_playoffs & made_superbowl don't contribute to predictive power 
