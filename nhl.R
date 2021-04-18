library(rvest)
library(tidyverse)

nhl_logos <- read_html("https://espndeportes.espn.com/nhl/equipos") %>% 
  html_nodes(".pl3 > a") %>% 
  html_attr("href")%>% tibble() %>% 
  set_names(c("nhl_logos")) %>% 
  mutate(equipourl = str_extract(nhl_logos, "[^/]+$"), 
         equipoabr = str_split(nhl_logos, "/", simplify = TRUE)[ , 6]) 

# ----------------------

teams <- c("carolina-hurricanes",
           "chicago-blackhawks",
           "columbus-blue-jackets",
           "dallas-stars",
           "detroit-red-wings",
           "florida-panthers",
           "nashville-predators",
           "tampa-bay-lightning",
           "boston-bruins",
           "buffalo-sabres",
           "new-jersey-devils",
           "new-york-islanders",
           "new-york-rangers",
           "philadelphia-flyers",
           "pittsburgh-penguins",
           "washington-capitals",
           "calgary-flames",
           "edmonton-oilers",
           "montreal-canadiens",
           "ottawa-senators",
           "toronto-maple-leafs",
           "vancouver-canucks",
           "winnipeg-jets",
           "anaheim-ducks",
           "arizona-coyotes",
           "colorado-avalanche",
           "los-angeles-kings",
           "minnesota-wild",
           "san-jose-sharks",
           "st-louis-blues",
           "vegas-golden-knights")
         
abr <- c("car",
         "chi",
         "cbj",
         "dal",
         "det",
         "fla",
         "nsh",
         "tb",
         "bos",
         "buf",
         "nj",
         "nyi",
         "nyr",
         "phi",
         "pit",
         "wsh",
         "cgy",
         "edm",
         "mtl",
         "ott",
         "tor",
         "van",
         "wpg",
         "ana",
         "ari",
         "col",
         "la",
         "min",
         "sj",
         "stl",
         "vgs")
  
cabezas <- tibble(abr, teams) 

#-------------------------------------------------

nhl_logo_scrape <- function(abr) {
  logos <- glue::glue("https://a.espncdn.com/i/teamlogos/nhl/500/{abr}.png")
  
}

nhl_logo_final <- cabezas  %>%  
  mutate(data = map(abr,  ~ nhl_logo_scrape(.x)))

nhl_logo_df <- nhl_logo_final %>% unnest() %>%
  set_names(c("abr", "teams" , "espn_logo")) %>% 
  mutate(
    espn_logo = str_squish(espn_logo)
  )

#--------------------------------------------------

write.csv(nhl_logo_df , "nhl_logos_df.csv", row.names = FALSE)
nhl_logos <- read.csv("nhl_logos_df.csv")


#--------------------------------------------------
nhl_foto_scrape <- function(abr, teams) {
  Sys.sleep(3)
  url <- glue::glue("https://espndeportes.espn.com/nhl/equipo/plantel/_/nombre/{abr}/{teams}")
  read_html(url) %>%
    html_nodes(".headshot img") %>%
    html_attr("alt") %>% tibble()
}

nhl_lfoto_final <- cabezas  %>%
  mutate(data = map2(abr, teams, ~ nhl_foto_scrape(.x,.y)))

nhl_lfoto_df <- nhl_lfoto_final %>% unnest() %>%
  set_names(c("abr", "teams" , "espn_foto"))%>%
  mutate(id_player = str_extract(espn_foto, "[0-9]+")) 

#--------------------------------------------------
nhl_name_scrape <- function(abr, teams) {
  Sys.sleep(3)
  url <- glue::glue("https://espndeportes.espn.com/nhl/equipo/plantel/_/nombre/{abr}/{teams}")
  read_html(url) %>%
    html_nodes("td:nth-of-type(2) div") %>%
    html_text("class") %>% tibble()
}

nhl_name_final <- cabezas  %>%
  mutate(data = map2(abr, teams, ~ nhl_name_scrape(.x,.y)))

nhl_name_df <- nhl_name_final %>% unnest() %>%
   set_names(c("abr", "teams" , "espn_player_name"))%>%
    mutate(espn_player_name = str_squish(espn_player_name),
                     number = str_extract(espn_player_name, "[0-9]+"),
                     espn_player_name = str_remove(espn_player_name, "[0-9]+")) %>% 
                     select(espn_player_name, number)
#--------------------------------------------------

dataNHL <-tibble (nhl_name_df,nhl_lfoto_df) %>% 
          select(id_player, espn_player_name, number, espn_foto, teams, abr)

dataNHL <- dataNHL %>% left_join(nhl_logos, by =c("abr","teams"))%>%
                        mutate(
                          espn_team_name = case_when(
                            abr == "car" ~ "Carolina Hurricanes",
                            abr == "chi" ~ "Chicago Blackhawks",
                            abr == "cbj" ~ "Columbus Blue Jackets",
                            abr == "dal" ~ "Dallas Stars",
                            abr == "det" ~ "Detroit Red Wings",
                            abr == "fla" ~ "Florida Panthers",
                            abr == "nsh" ~ "Nashville Predators",
                            abr == "tb"  ~ "Tampa Bay Lightning",
                            abr == "bos" ~ "Boston Bruins",
                            abr == "buf" ~ "Buffalo Sabres",
                            abr == "nj" ~ "New Jersey Devils",
                            abr == "nyi" ~ "New York Islanders",
                            abr == "nyr" ~ "New York Rangers",
                            abr == "phi" ~ "Philadelphia Flyers",
                            abr == "pit" ~ "Pittsburgh Penguins",
                            abr == "wsh" ~ "Washington Capitals",
                            abr == "cgy" ~ "Calgary Flames",
                            abr == "edm" ~ "Edmonton Oilers",
                            abr == "mtl" ~ "Montreal Canadiens",
                            abr == "ott" ~ "Ottawa Senators",
                            abr == "tor" ~ "Toronto Maple Leafs",
                            abr == "van" ~ "Vancouver Canucks",
                            abr == "wpg" ~ "Winnipeg Jets",
                            abr == "ana" ~ "Anaheim Ducks",
                            abr == "ari" ~ "Arizona Coyotes",
                            abr == "col" ~ "Colorado Avalanche",
                            abr == "la"  ~ "Los Angeles Kings",
                            abr == "min" ~ "Minnesota Wild",
                            abr == "sj" ~ "San Jose Sharks",
                            abr == "stl" ~ "St. Louis Blues",
                            abr == "vgs" ~ "Vegas Golden Knights",
                            TRUE ~ abr)
                        )
dataNHL <- dataNHL %>% select(id_player,
                              espn_player_name,
                              player_number = number, 
                              cabezas = espn_foto,
                              espn_team_name,
                              espn_logo,
                              url_name = teams,
                              url_abr = abr) %>%
                              mutate(
                                refe_abrName = case_when(
                                   url_abr == "car" ~ "CAR",   
                                   url_abr == "chi" ~ "CHI",
                                   url_abr == "cbj" ~ "CBJ",
                                   url_abr == "dal" ~ "DAL",
                                   url_abr == "det" ~ "DET",
                                   url_abr == "fla" ~ "FLA",
                                   url_abr == "nsh" ~ "NSH",
                                   url_abr == "tb"  ~ "TBL",
                                   url_abr == "bos" ~ "BOS",
                                   url_abr == "buf" ~"BUF",
                                   url_abr == "nj" ~ "NJD",
                                   url_abr == "nyi" ~ "NYI",
                                   url_abr == "nyr" ~ "NYR",
                                   url_abr == "phi" ~ "PHI",
                                   url_abr == "pit" ~ "PIT",
                                   url_abr == "wsh" ~ "WSH",
                                   url_abr == "cgy" ~ "CGY",
                                   url_abr == "edm" ~ "EDM",
                                   url_abr == "mtl" ~ "MTL",
                                   url_abr == "ott" ~ "OTT",
                                   url_abr == "tor" ~ "TOR",
                                   url_abr == "van" ~ "VAN",
                                   url_abr == "wpg" ~ "WPG",
                                   url_abr == "ana" ~ "ANA",
                                   url_abr == "ari" ~ "ARI",
                                   url_abr == "col" ~ "COL",
                                   url_abr == "la"  ~ "LAK",
                                   url_abr == "min" ~ "MIN",
                                   url_abr == "sj" ~ "SJS",
                                   url_abr == "stl" ~ "STL",
                                   url_abr == "vgs" ~ "VEG",
                                  TRUE ~  url_abr)
                              )
                            
#--------------------------------------------------                            

write.csv(dataNHL, "dataNHL.csv", row.names = FALSE)
dataNHL <- read.csv("dataNHL.csv")

#--------------------------------------------------

data_git_nhl <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/NHL/main/dataNHL.csv")
data_git_nhl 

