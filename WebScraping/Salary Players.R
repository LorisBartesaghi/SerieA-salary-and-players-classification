#import necessary libraries
library(rvest)
library(tidyverse)
library(stringr)


#url of WebSite in which are present data about serie A athletes' salaries
url_salary <- c("https://salarysport.com/football/serie-a/atalanta/",
               "https://salarysport.com/football/serie-a/benevento/",
               "https://salarysport.com/football/serie-a/bologna-fc-1909/",
               "https://salarysport.com/football/serie-a/cagliari/",
               "https://salarysport.com/football/serie-a/crotone/",
               "https://salarysport.com/football/serie-a/acf-fiorentina/",
               "https://salarysport.com/football/serie-a/genoa-cfc/",
               "https://salarysport.com/football/serie-a/fc-internazionale-milano/",
               "https://salarysport.com/football/serie-a/juventus/",
               "https://salarysport.com/football/serie-a/lazio/",
               "https://salarysport.com/football/serie-a/ac-milan/",
               "https://salarysport.com/football/serie-a/napoli/",
               "https://salarysport.com/football/serie-a/parma-calcio-1913/",
               "https://salarysport.com/football/serie-a/as-roma/",
               "https://salarysport.com/football/serie-a/u.c.-sampdoria/",
               "https://salarysport.com/football/serie-a/u.s.-sassuolo/",
               "https://salarysport.com/football/serie-a/spezia-calcio/",
               "https://salarysport.com/football/serie-a/torino/",
               "https://salarysport.com/football/serie-a/udinese/",
               "https://salarysport.com/football/serie-a/hellas-verona/"
              )

#script to extract table of interest from the WebSite
data_scraping <- function(url){
  link <- (url)
  page <- read_html(link)
  
  #read the first table in the HTML page
  tab <- page %>% html_nodes("table") %>% .[1] %>% html_table %>% .[[1]]
 
  #reshape the dataset
  tab <- tab[-c(12,23),-c(2)]
  
  #work on yearly.salary data
  tab$`Yearly Salary` <- gsub("£", "",tab$`Yearly Salary`)
  tab$`Yearly Salary` <- gsub(",", "",tab$`Yearly Salary`)
  tab$`Yearly Salary`<- as.numeric(tab$`Yearly Salary`)

  #work on tab.position data
  tab$Position <- gsub("/.*", "", tab$Position)
  tab$Position <- gsub("\\(.*","", tab$Position)
  tab$Position <- gsub(",.*", "", tab$Position)
  tab$Position <- gsub("AM", "ST", tab$Position)
  tab$Position <- gsub("DM", "M", tab$Position)
  tab$Position <- gsub("WB", "D", tab$Position)
  tab$Position <- gsub("LB", "D", tab$Position)
  tab$Position <- gsub("D ","D", tab$Position)
  tab$Position <- gsub("M ", "M", tab$Position)
  tab$Position <- gsub("ST ", "ST", tab$Position)
  tab$Position <- gsub("DC", "D", tab$Position)
  tab$Position <- gsub("STC", "M", tab$Position)
  tab$Position <- gsub("STRLC", "ST", tab$Position)
  tab$Position <- gsub("DLC", "M", tab$Position)
  tab$Position <- gsub("DRLC", "D", tab$Position)
  
  #remove parenthesis from player name
  tab$'Player Name' <- gsub("\\(.*","", tab$'Player Name') 
  tab$'Player Name' <- trimws(tab$'Player Name', 'r')
  #change encoding
  tab$'Player Name' <- iconv(tab$'Player Name',from="UTF-8",to="ASCII//TRANSLIT")
  
  #extract from the URL the name of the team
  tab$team <- str_split(url, "/")[[1]][6]
  tab <<- tab
}  

#apply previous function to all the url linked to each seria A tems
string_salary <- lapply(url_salary, data_scraping)

################################################FINAL DATASET FOR PERFOMANCE PLAYERS
Player_salary <- bind_rows(string_salary)


################################################SAVE A CSV FILE
write.csv(Player_salary, "Salary Players.csv")




