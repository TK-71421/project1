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