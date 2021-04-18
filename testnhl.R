library(rvest)
library(tidyverse)

data_git_nhl <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/NHL/main/dataNHL.csv")

testNHL <- read_html("https://www.hockey-reference.com/leagues/NHL_2021_skaters-advanced.html") %>% 
  html_node("table.sortable") %>% 
  html_table() %>% janitor::row_to_names(row_number(1)) %>% 
  janitor::clean_names() %>% 
  filter(player != "Player") %>% 
  left_join(data_git_nhl, by =c("player"= "espn_player_name"))

reference_nhl_na <- testNHL%>% filter(is.na(cabezas))


#         Resultado de 80 jugadores sin foto de 862 que no estaban en la plantilla al ejecutar el código