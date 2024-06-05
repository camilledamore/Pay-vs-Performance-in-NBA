#removes all objects in workspace
rm(list=ls())

#load packages
library(tidyverse)
library(janitor)
library(stringr)
library(ggplot2)



#create df with NBA player salary data (and removing unnecessary column)
player_salary <- read_csv("NBA_player_salary_per_season.csv")
player_salary <- player_salary[,-1]

#create df with CPI data
cpi_fed <- read_csv("CPI_U_minneapolis_fed.csv")
cpi_fed <- cpi_fed[, c("year", "CPI")]


#merge dfs
player_salary_cpi1 <- player_salary %>%
  left_join(cpi_fed, by = c("season" = "year"))

#create base year for inflation adjustment and adjust all player salaries for inflation
cpi_2015 <- filter(cpi_fed, year == 2015)$CPI

player_salary_cpi2 <- player_salary_cpi1 %>%
  mutate(real_2015_salary = salary * (cpi_2015 / CPI))

player_salary_cpi <- player_salary_cpi2 %>%
  mutate(real_2015_salary_mill = real_2015_salary / 1000000)

#calculate minimum, max, and avg player salaries per season
salary_by_year_stat <- player_salary_cpi %>%
  group_by(season) %>%
  summarise(
    min_salary = min(real_2015_salary_mill, na.rm = TRUE),
    avg_salary = mean(real_2015_salary_mill, na.rm = TRUE),
    max_salary = max(real_2015_salary_mill, na.rm = TRUE)
  )


#Minutes played per game per player
#cleaning and prepping data
nba_minutes1 <- read_csv("nba_number_mins_played_per_player_per_game_2015-2020.csv")
nba_minutes1 <- nba_minutes1[,-1]
nba_minutes1$player_name <- tolower(nba_minutes1$player_name)

nba_minutes_dups <- nba_minutes1 %>%
  group_by(player_name, game_id) %>%
  count() %>%
  filter(n > 1)

nba_minutes <- nba_minutes1 %>%
  distinct(player_name, game_id, .keep_all = TRUE)

#Points scored played per game per player
#cleaning and prepping data
nba_points1 <- read_csv("nba_number_points_scored_per_player_per_game_2015-2020.csv")
nba_points1 <- nba_points1[,-1]
nba_points1$player_name <- tolower(nba_points1$player_name)
nba_points_dups <- nba_points1 %>%
  group_by(player_name, game_id) %>%
  count() %>%
  filter(n > 1)
nba_points <- nba_points1 %>%
  distinct(player_name, game_id, .keep_all = TRUE)

#combine tables, adds season data to performance
nba_games_perf <- inner_join(nba_minutes, nba_points, by = c("player_name", "game_id"))
nba_games_perf <- nba_games_perf %>%
  extract(col = min, into = "minutes", regex = "^(\\d{2})", remove = FALSE) %>%
  mutate(minutes = as.numeric(minutes))

#averages
nba_games_perf_avg <- nba_games_perf %>%
  summarise(avg_min = mean(minutes, na.rm = TRUE),
            avg_pts = mean(pts, na.rm = TRUE))


#creating performance metric vs salary for players
nba_gameid_vs_season <- read_csv("nba_match_gameid_to_season.csv") %>%
  select(-1)
nba_games_perf_season <- left_join(nba_games_perf, nba_gameid_vs_season, by = "game_id")

#player points per minute active game time 
nba_games_perf_season <- nba_games_perf_season %>% 
  mutate(pts_per_min = pts / minutes) %>% 
  na.omit()




#avgs overall (benchmark)
nba_games_perf_season_avg <- nba_games_perf_season %>%
  filter(!is.na(season)) %>%
  group_by(season) %>%
  summarise(
    avg_pts_per_min = mean(pts_per_min, na.rm = TRUE),
  ) %>%
  arrange(season)

#avgs per player per season
nba_games_indiv_perf_season_avg <- nba_games_perf_season %>%
  filter(!is.na(season)) %>%
  group_by(season, player_name) %>%
  summarise(
    avg_pts_per_min = mean(pts_per_min, na.rm = TRUE),
  ) %>%
  arrange(season)


# Vs.player salary
PERF_VS_SALARY_PER_PLAYER_PER_SEASON <- inner_join(player_salary, nba_games_indiv_perf_season_avg)

#avgs per player overall
PERF_VS_SALARY_PER_PLAYER_AVG <- PERF_VS_SALARY_PER_PLAYER_PER_SEASON %>% 
  ungroup() %>% 
  group_by(player_name) %>% 
  summarise(
    avg_salary = mean(salary/1000000, na.rm = TRUE),
    avg_pts_per_min = mean(avg_pts_per_min, na.rm = TRUE)
  )

#performance metric vs salary avgs
player_salary_perf <- inner_join(salary_by_year_stat, nba_games_perf_season_avg, by = "season")



#PLOT

ggplot(PERF_VS_SALARY_PER_PLAYER_AVG, aes(x = avg_salary, y = avg_pts_per_min, color = avg_pts_per_min)) +
  geom_point() +
  labs(
    x = "Average Salary (Millions)",
    y = "Average Points per Minute",
    color = "Avg Points per Min"
  ) +
  scale_color_gradient(low = "blue", high = "orange") +  
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_minimal()
  




