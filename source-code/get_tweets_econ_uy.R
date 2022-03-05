# Conectarse a Twitter

library(rio)
library(glue)
library(here)
library(genero)
library(rtweet)
library(stringr)
library(tidyverse)

# Función para acceder a tweets (by @rlabuonora)

source(here("functions", "get_tweets.R"))

# Función para loguearse a Twitter 

source(here("token", "tokens.R"))

twitter_token <- create_token(
  app = "econ-gender-twitter",
  consumer_key = Sys.getenv("TWITTER_CONSUMER_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_SECRET"))

econ_uy <- lists_members("814632846618988544")

faltan <- c("LBonillaLastman", "estefaniagalv4n", "mariasauval", "TamaraSchandy", "flocarri", "luetchev", 
            "lucia_casal_", "carotbo", "gioia_de_melo", "SofiaHargui", "J_Bloomfield_", "AnaInesBalsa", 
            "JuanDubra1", "MarceloCaffera", "AlejandroCid_uy", "rafaguntin", "silacos", "vantonaccio", 
            "SilvinaPanizza", "isaantonaccio", "MDBSBC", "marianadeviaje", "triunfo_econ", "IgnacioAmaral",
            "JuanSPereyra1", "gra_sanroman", "Rodrigoceni2", "Aguiar28D", "ncichevski", "gabiloncho_katz",
            "joseigrivero", "PPicardo", "MichelinGustavo", "carlos_d_diaz", "VRobano", "carolina_balian", 
            "rodrinicob", "mathias_fondo", "Mati_strehl", "MColacce", "jfberrutti", "MPereiraElola", "moliverab", 
            "JimenaPardo11", "E_lopz_barrera", "JaviercitoAlejo", "Gabriel_Merlo_M", "kari_colombo", "maximachgo", 
            "GiuSimonc", "a_tololo", "grielpapa")

selector <- c("name", "screen_name", "location", "description", "followers_count", "friends_count", "created_at", 
              "favourites_count", "statuses_count")

econ_uy <- econ_uy %>%
  select(all_of(selector))

faltantes <- lookup_users(faltan) %>%
  select(all_of(selector))

econ_twitter <- econ_uy %>%
  bind_rows(faltantes)

# econ_twitter %>% write.csv2(here("data", "csv", "econ_twitter.csv"), row.names = FALSE)

econ_twitter %>% 
  write_rds(here("data", "rds", "econ_twitter.rds"))

econ_twitter <- read_rds(here("data", "rds", "econ_twitter.rds"))

# users_econ_twitter <- read_delim(here("data", "csv", "econ_twitter.csv"), delim = ";") 

users_econ_uy <- econ_twitter %>%
  pull(screen_name)

econ_twitter_uy_data <- get_timeline_unlimited(users_econ_uy, n = 1000) %>% 
  rtweet::flatten()

econ_twitter_uy_data <- read_rds(here("data", "rds", "econ_twitter.rds")) 

trying <- econ_twitter_uy_data %>% 
  left_join(gender, by = "screen_name")%>% 
  filter((!(screen_name %in% c("Fscrollini", "alfonsopratgay", "gbudino", "sfogel", "JackAldwinckle", "loeff", "niefpe")))) %>%
  drop_na(sex_prediction) 

trying %>% 
  write_rds(here("data", "rds", "econ_twitter_uy_data_gender.rds")) 

export(trying, here("data", "econ_twitter_uy_data_gender.xlsx"))

# gender_vector <- unique(econ_twitter_uy_data$name)

sex_prediction <- genero(econ_twitter_uy_data$name, result_as = c(male = "male", female = "female"), 
                            lang = "es", col = "gender")

econ_twitter_uy_full <- econ_twitter_uy_data %>% 
  cbind(sex_prediction) %>% 
  select(name, screen_name, sex_prediction, location, description, followers_count, friends_count, 
         created_at, favourites_count, statuses_count) %>% 
  mutate(sex_prediction = case_when(
    screen_name == "el_lado_f" ~ "female",
    screen_name == "JorgePolgar" ~ "male", 
    screen_name == "Matias__Brum" ~ "male",
    screen_name == "NatuCarrau" ~ "female",
    screen_name == "agusgreif" ~ "male",
    screen_name == "Juani_Dorrego" ~ "male",
    screen_name == "ramirorb23" ~ "male",
    screen_name == "JRodriguezWeber" ~ "male",
    screen_name == "mister__lemon" ~ "male",
    screen_name == "Montaldo_Pablo" ~ "male",
    screen_name == "GonzaloZunino" ~ "male",
    screen_name == "santi_garza" ~ "male",
    screen_name == "estefaniagalv4n" ~ "female",
    screen_name == "carotbo" ~ "female",
    screen_name == "joseigrivero" ~ "male",
    screen_name == "grantthorntonuy" ~ NA_character_, 
    screen_name == "BlasinayAsoc" ~ NA_character_, 
    T ~ sex_prediction)) %>% 
  filter((!(screen_name %in% c("Fscrollini", "alfonsopratgay", "gbudino", "sfogel", "JackAldwinckle", "loeff", "niefpe")))) %>%
  drop_na(sex_prediction) 

write_rds(econ_twitter_uy_full, here("data", "rds", "econ_twitter_uy_full.csv"))
export(econ_twitter_uy_full, here("data", "econ_twitter_uy_full.xlsx"))

# econ_twitter_uy_data <- apply(econ_twitter_uy_data, 2, as.character)
# write.csv2(econ_twitter_uy_data, here("data", "csv", "full_data.csv"), row.names = FALSE)


# econ_twitter_uy_data2 <- get_timeline(user_econ_uy, n = 1000) 
# econ_twitter_uy_data2 <- apply(econ_twitter_uy_data2, 2, as.character)
# write.csv2(econ_twitter_uy_data, here("data", "full_data_backup.csv"), row.names = FALSE)

# econ_twitter_uy_data <- apply(lu_pau, 2, as.character)
# 
# econ_twitter_uy_data %>% 
#   write.csv2(here("data", "econ_twitter_uy_data.csv"), row.names = FALSE)

# econ_twitter_friends <- get_friends(users_econ_twitter, retryonratelimit = TRUE)
# econ_twitter_followers <- get_friends(users_econ_twitter, retryonratelimit = TRUE)
# 
# econ_twitter_friends %>% 
#   write.csv2(here("data", "econ_twitter_friends.csv"), row.names = FALSE)
# 
# econ_twitter_followers %>% 
#   write.csv2(here("data", "econ_twitter_followers.csv"), row.names = FALSE)
# rtweet::ts_plot 
# get_friends() y su documentacióm
# get_followers()ef
##### ¡VEEEEEER! Acordarse de genderify.