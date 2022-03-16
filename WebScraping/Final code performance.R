#create tables from fbref site
first_table <- function(url){
  #read url
  link <- (url)
  page <-  read_html(link)
  
  #create dataset
  team <- page %>% html_nodes("table") %>% .[1] %>% html_table %>% .[[1]]
  
  #select statistics
  team <- team[, 1:20]
  team <- team[, -c(2,3,4)]
  
  #rename variable
  colnames(team) <- team[1,]
  team <- team[-c(1), ]
  colnames(team)[6] <- "goal 90"
  colnames(team)[7] <- "assit 90"
  colnames(team)[8] <- "G-p 90"
  
  
  #change point with no charachter
  team$Min <- gsub(',',"", team$Min)
  
  #change , with . and create numeric variable
  co_name <- colnames(team)
  co_name <- co_name[-1]
  
  #from ',' to '.', to ease the use of the dataset on R
  team[co_name] <- lapply(team[co_name], gsub, pattern = ",", replacement = "\\.")
  team[co_name] <- lapply(team[co_name], as.numeric)
  
  team <<- team
}
second_table <- function(url){
  #read url
  link <- url
  page <-  read_html(link)
  
  #extract table of interest
  team <- page %>% html_nodes("table") %>% .[5] %>% html_table %>% .[[1]]
  
  #reatin statistics of interest
  team <- team[, 1:17]
  team <- team[, -c(2,3,4,5,6,16,17)]
  colnames(team) <- team[1,]
  team <- team[-c(1), ]
  
  #rename columns
  colnames(team)[3] <- "Shot on Target"
  
  co_name <- colnames(team)
  co_name <- co_name[-1]
  
  #substitute values
  team[co_name] <- lapply(team[co_name], gsub, pattern = "^$", replacement = "0,00")
  team[co_name] <- lapply(team[co_name], gsub, pattern = ",", replacement = "\\.")
  team[co_name] <- lapply(team[co_name], as.numeric)
  
  team <<- team
}
third_table <- function(url){
  #read url
  link <- url
  page <-  read_html(link)
  
  #extract table from url page
  team <- page %>% html_nodes("table") %>% .[6] %>% html_table %>% .[[1]]
  
  #retain useful statistics
  team <- team[, -c(2,3,4,5,20,22,28)]
  colnames(team) <- team[1,]
  team <- team[-c(1), ]
  
  #work on name variables
  new_name <- c("total completed", "total tempted", " total %completed ", "total distance", "progressive distance", "short completed", "short attempted","short%", "mediumm completed", "medium attempted", "%medium", "long complted", "long attempted", "long %")
  colnames(team)[2:15] <- new_name
  co_name <- colnames(team)
  co_name <- co_name[-1]
  
  #substitute values
  team[co_name] <- lapply(team[co_name], gsub, pattern = "^$", replacement = "0,00")
  team[co_name] <- lapply(team[co_name], gsub, pattern = ",", replacement = "\\.")
  team[co_name] <- lapply(team[co_name], as.numeric)
  
  team <<- team
  
}
fourth_table <- function(url){
  #read url
  link <- url
  page <-  read_html(link)
  
  #exctract table
  team <- page %>% html_nodes("table") %>% .[8] %>% html_table %>% .[[1]]
  
  #reatin columns
  team <- team[, -c(2,3,4,5,22)]
  colnames(team) <- team[1,]
  team <- team[-c(1), ]
  
  #change names for columns
  new_name <- c("ATG in play", "ATG not in play", "ATG drib", "ATG shot", "ATG fouls", "ATG finalized")
  colnames(team)[12:17] <- new_name
  co_name <- colnames(team)
  co_name <- co_name[-1]
  
  #subs values
  team[co_name] <- lapply(team[co_name], gsub, pattern = "^$", replacement = "0,00")
  team[co_name] <- lapply(team[co_name], gsub, pattern = ",", replacement = "\\.")
  team[co_name] <- lapply(team[co_name], as.numeric)  
  
  team <<- team
}
fifth_table <- function(url){
  #read url
  link <- url
  page <-  read_html(link)
  
  #extract table
  team <- page %>% html_nodes("table") %>% .[9] %>% html_table %>% .[[1]]
  
  #retain useful statistics
  team <- team[, -c(2,3,4,5,29)]
  colnames(team) <- team[1,]
  team <- team[-c(1), ]
  
  #rename variables
  new_name <- c("Pressin def", "Press mid", "Press att")
  colnames(team)[14:16] <- new_name
  colnames(team)[7] <- "#contrs opponents"
  co_name <- colnames(team)
  co_name <- co_name[-1]
  
  #sobstitute values
  team[co_name] <- lapply(team[co_name], gsub, pattern = "^$", replacement = "0,00")
  team[co_name] <- lapply(team[co_name], gsub, pattern = ",", replacement = "\\.")
  team[co_name] <- lapply(team[co_name], as.numeric) 
  
  team <<- team
}
sixth_table <- function(url){
  #read url
  link <- url
  page <-  read_html(link)
  
  #extract table
  team <- page %>% html_nodes("table") %>% .[10] %>% html_table %>% .[[1]]
  
  #retain useful informations
  team <- team[, -c(2,3,4,5,30)]
  colnames(team) <- team[1,]
  team <- team[-c(1), ]
  colnames(team)[25] <- "prog. ric."
  
  co_name <- colnames(team)
  co_name <- co_name[-1]
  
  #subs values
  team[co_name] <- lapply(team[co_name], gsub, pattern = "^$", replacement = "0,00")
  team[co_name] <- lapply(team[co_name], gsub, pattern = ",", replacement = "\\.")
  team[co_name] <- lapply(team[co_name], as.numeric) 
  
  team <<- team
}
seventh_table <- function(url){
  #read url
  link <- url
  page <-  read_html(link)
  
  #extract table
  team <- page %>% html_nodes("table") %>% .[12] %>% html_table %>% .[[1]]
  
  #reatin useful columns
  team <- team[, -c(2,3,4,5,6,7,13,14,15,16,22)]
  colnames(team) <- team[1,]
  team <- team[-c(1), ]
  
  #rename variables
  colnames(team)[4] <- "sub_fouls"
  co_name <- colnames(team)
  co_name <- co_name[-1]
  
  #subsvelues
  team[co_name] <- lapply(team[co_name], gsub, pattern = ",", replacement = "\\.")
  team[co_name] <- lapply(team[co_name], as.numeric)
  
  team <<- team
}
#############################CREATE A FUNCTION FOR CREATE FOR EACH TEAM THE COMPLETE PERFOMANCE PLAYER DATASET
merging <- function(url){
  team1 <<- first_table(url)
  team2 <<- second_table(url)
  team3 <<- third_table(url)
  team4 <<- fourth_table(url)
  team5 <<- fifth_table(url)
  team6 <<- sixth_table(url)
  team7 <<- seventh_table(url)
  
  TEAM_STAT <<-  Reduce(function(x,y) merge(x = x, y = y, by = "Player"), 
                        list(team1, team2, team3, team4, team5, team6, team7))
}

##############################################################################LIST OF URL
url_teams <- c("https://fbref.com/en/squads/922493f3/Atalanta-Stats", 
               "https://fbref.com/en/squads/4fcb34fd/Benevento-Stats",
               "https://fbref.com/en/squads/1d8099f8/Bologna-Stats",
               "https://fbref.com/en/squads/c4260e09/Cagliari-Stats",
               "https://fbref.com/en/squads/3074d7b1/Crotone-Stats",
               "https://fbref.com/en/squads/421387cf/Fiorentina-Stats",
               "https://fbref.com/en/squads/658bf2de/Genoa-Stats",
               "https://fbref.com/en/squads/0e72edf2/Hellas-Verona-Stats",
               "https://fbref.com/en/squads/d609edc0/Internazionale-Stats",
               "https://fbref.com/en/squads/e0652b02/Juventus-Stats",
               "https://fbref.com/en/squads/7213da33/Lazio-Stats",
               "https://fbref.com/en/squads/dc56fe14/Milan-Stats",
               "https://fbref.com/en/squads/d48ad4ff/Napoli-Stats",
               "https://fbref.com/en/squads/eab4234c/Parma-Stats",
               "https://fbref.com/en/squads/cf74a709/Roma-Stats",
               "https://fbref.com/en/squads/8ff9e3b3/Sampdoria-Stats",
               "https://fbref.com/en/squads/e2befd26/Sassuolo-Stats",
               "https://fbref.com/en/squads/68449f6d/Spezia-Stats",
               "https://fbref.com/en/squads/105360fe/Torino-Stats",
               "https://fbref.com/en/squads/04eea015/Udinese-Stats")


#################################################FOR EACH LINK APPLY THE MERGING FUNCTION
teams_dataset <- lapply(url_teams, merging)


################################################FINAL DATASET FOR PERFOMANCE PLAYERS
Performance_player <- bind_rows(teams_dataset, .id= "id_footbal_team" )
Performance_player$Player <- iconv(Performance_player$Player,from="UTF-8",to="ASCII//TRANSLIT")

################################################SAVE A CSV FILE
write.csv(Performance_player, "Performance Players.csv")




