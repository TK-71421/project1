library(tidyverse) 

# cleaning csvs + combing them in one file 
season2015 = read_csv("nfl/raw_data/season2015.csv") 
season2016 = read_csv("nfl/raw_data/season2016.csv") 
season2017 = read_csv("nfl/raw_data/season2017.csv") 
season2018 = read_csv("nfl/raw_data/season2018.csv") 
season2019 = read_csv("nfl/raw_data/season2019.csv") 
season2020 = read_csv("nfl/raw_data/season2020.csv")
season2021 = read_csv('nfl/raw_data/season2021.csv') 
season2022 = read_csv("nfl/raw_data/season2022.csv") 
season2023 = read_csv("nfl/raw_data/season2023.csv") 
season2024 = read_csv("nfl/raw_data/season2024.csv") 
season2025 = read_csv("nfl/raw_data/season2025.csv")

seasons15_to_24 = bind_rows(
    season2015, season2016, season2017, season2018, season2019, season2020, 
    season2021, season2022, season2023, season2024
)

seasons15_to_24 = seasons15_to_24 %>% 
    mutate(
        home_team_win = case_when(
            ...6 == '@' ~ 0, 
            .default = 1 
        ), 
        home_team_win = as.integer(home_team_win)
    )

saveRDS(seasons15_to_24, "nfl/raw_data/season15_to_24.rds") 

# exploratory data analysis 
data = read_csv("nfl/raw_data/standings.csv")

data$Tm = gsub("[\\*\\+]", "", data$Tm)
data$Tm[data$Tm == "Oakland Raiders"] = "Las Vegas Raiders"
data$Tm[data$Tm == "St. Louis Rams"] = "Los Angeles Rams"
data$Tm[data$Tm == "San Diego Chargers"] = "Los Angeles Chargers"
data$Tm[data$Tm == "Washington Redskins"] = "Washington Commanders"
data$Tm[data$Tm == "Washington Football Team"] = "Washington Commanders"

# high same-season correlation between PD and win pct 
ggplot(data, aes(x=PD, y=pct)) + 
  geom_point() 

# what about past year PD vs. current year win pct? 
calculate_past_metrics = function(data, gap) {

  pd = data %>% select(Tm, year, PD, pct) 
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

data_past3 = calculate_past_metrics(data, gap=3)
m5 = lm(pct_cur ~ PD_past, data = data_past3) 
summary(m5)
# current year - 3 is no longer significant 
# but still might be worth including 

# what if we tried everything together? 
# this is the jankiest function ever... but it works 
calculate_all_past = function(data) {
  pd = data %>% select(Tm, year, PD) 
   data = data %>% 
    left_join(
      pd, by = "Tm", 
      relationship = "many-to-many", 
      suffix = c("_cur", "_past")
    ) %>% 
    filter(year_past + 3 >= year_cur & year_past != year_cur) %>% 
    mutate(
      year_indicator = case_when(
        year_past + 1 == year_cur ~ 'past_1', 
        year_past + 2 == year_cur ~ 'past_2', 
        year_past + 3 == year_cur ~ 'past_3'
      ) 
    ) %>% 
    drop_na(year_indicator) %>% 
    pivot_wider(
      names_from = year_indicator,
      values_from = PD_past
    ) %>%
    group_by(year_cur, Tm) %>% 
    summarize(
      W = mean(W), 
      L = mean(L), 
      win_pct = mean(pct), 
      past_1 = mean(past_1, na.rm = TRUE), 
      past_2 = mean(past_2, na.rm = TRUE), 
      past_3 = mean(past_3, na.rm = TRUE)
    ) %>% 
    drop_na() 
  
  return(data)
}

data_all_past = calculate_all_past(data) 

m6 = lm(win_pct ~ past_1 + past_2 + past_3, data = data_all_past) 
summary(m6)
# interestingly only the previous year matters 
