library(tidyverse)

load("data/england.rda")

# assign points to results (2 pts for a win up to 1980-81 season then 3 pts for a win afterwards)

england <- england %>% 
  dplyr::mutate(
    homepts = dplyr::case_when(
      Season <= 1980 & result == "H" ~ 2,
      Season > 1980 & result == "H" ~ 3,
      result == "D" ~ 1,
      result == "A" ~ 0
    ),
    awaypts = dplyr::case_when(
      Season <= 1980 & result == "A" ~ 2,
      Season > 1980 & result == "A" ~ 3,
      result == "D" ~ 1,
      result == "H" ~ 0
    )
  )

# restrict to Tier 1 and assemble into total points per season

home_pts <- england %>%
  dplyr::filter(tier == 1) %>% 
  dplyr::group_by(Season, home) %>% 
  dplyr::summarize(pts = sum(homepts))

away_pts <- england %>%
  dplyr::filter(tier == 1) %>% 
  dplyr::group_by(Season, visitor) %>% 
  dplyr::summarize(pts = sum(awaypts))

total_pts <- home_pts %>% 
  dplyr::rename(Team = home) %>% 
  dplyr::bind_rows(
    away_pts %>% 
      dplyr::rename(Team = visitor)
  ) %>% 
  dplyr::group_by(Season, Team) %>% 
  dplyr::summarise(pts = sum(pts))

# create rolling sums

table <- total_pts %>% 
  dplyr::filter(Season == 1888) %>% 
  dplyr::select(Season, Team, Points = pts)

for (i in 1889:2017) {
  table <- total_pts %>% 
    dplyr::filter(Season <= i) %>% 
    dplyr::group_by(Team) %>% 
    dplyr::summarise(Points = sum(pts, na.rm = TRUE)) %>% 
    dplyr::mutate(Season = i) %>% 
    dplyr::bind_rows(table)
}

# add some historic facts to seasons

table <- table %>% 
  dplyr::mutate(
    SeasonLabel = dplyr::case_when(
      Season <= 1891 ~ paste(Season, "Football League is formed with 12 teams in 1888", sep = " - "),
      dplyr::between(Season, 1892, 1895) ~ paste(Season, "Second Division introduced in 1892", sep = " - "),
      dplyr::between(Season, 1914, 1918) ~ paste(Season, "League suspended during World War I", sep = " - "),
      dplyr::between(Season, 1920, 1924) ~ paste(Season, "Third Division North/South introduced in 1920/21", sep = " - "),
      dplyr::between(Season, 1925, 1928) ~ paste(Season, "New Offside Law introduced in 1925", sep = " - "),
      dplyr::between(Season, 1939, 1945) ~ paste(Season, "League suspended during World War II", sep = " - "),
      dplyr::between(Season, 1958, 1961) ~ paste(Season, "Regional Third Divisions amalgamated in 1958 to form Nationwide Third and Fourth Divisions", sep = " - "),
      dplyr::between(Season, 1965, 1968) ~ paste(Season, "Substitutes first allowed in 1965", sep = " - "),
      dplyr::between(Season, 1974, 1977) ~ paste(Season, "First match played on a Sunday in 1974", sep = " - "),
      dplyr::between(Season, 1981, 1984) ~ paste(Season, "Three points for a win introduced in 1981", sep = " - "),
      dplyr::between(Season, 1986, 1989) ~ paste(Season, "Play-offs introduced to decide some promotions", sep = " - "),
      dplyr::between(Season, 1992, 1995) ~ paste(Season, "Premier League formed in 1992, reducing Football League to three divisions", sep = " - "),
      dplyr::between(Season, 2004, 2007) ~ paste(Season, "Football League renames divisions in 2004 to Championship, League One and League Two", sep = " - "),
      dplyr::between(Season, 2013, 2016) ~ paste(Season, "Goal Line Technology introduced in Premier League in 2013", sep = " - "),
      1L == 1L ~ as.character(Season)
      
      
    )
  )

# save for use in graph

save(table, file = "./data/table.RData")



