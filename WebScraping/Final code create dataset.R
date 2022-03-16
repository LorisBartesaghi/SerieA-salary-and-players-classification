salary <- read.csv("Salary Players.csv")
performance <- read.csv("Performance Players.csv")

#Same players with different names in the datasets
performance[1,3] <- salary[6,2] #Alexey Myranchuk
performance[7,3] <- salary[30,2] # Ghislandi
performance[37,3] <- salary[33,2] #Ionita
performance[55,3] <- salary[55,2] # Manfredini
performance[61,3] <- salary[59,2] # Sanogo
performance[69,3] <- salary[82,2] # Da costa junior 
performance[111,3] <- salary[107,2] # Lykogiannis
performance[166,3] <- salary[124,2] # Simy
performance[144,3] <- salary[137,2] # Dragus
performance[147,3] <- salary[147,2] # Crespi
performance[170,3] <- salary[153,2] # kokorin
performance[219,3] <- salary[202,2] # Cziborra
performance[331,3] <- salary[281,2] # Stefan Radu
performance[283,3] <- salary[233,2] # Iounut Radu
performance[304,3] <- salary[250,2] # Arthur
performance[368,3] <- salary[319,2] # Tatarusanu
performance[403,3] <- salary[348,2] # Fabian Ruiz
performance[401,3] <- salary[350,2] # Elmas
performance[460,3] <- salary[390,2] # Zagaritis
performance[484,3] <- salary[404,2] # Jesus
performance[494,3] <- salary[417,2] # Ibanez
performance[538,3] <- salary[473,2] # Kiriakopulos
performance[556,3] <- salary[458,2] # Chiriches
performance[88,3] <- salary[478,2] # Paz
performance[589,3] <- salary[505,2] # Ferrer
performance[603,3] <- salary[527,2] # Bremer
performance[248,3] <- salary[595,2] # Salcedo
performance[271,3] <- salary[575,2] # Cetin

#change code of the team to team name
for (i in 1:659){
  if(performance[i,2] == 1){performance[i,2] <- "atalanta"}
  if(performance[i,2] == 2){performance[i,2] <- "benevento"}
  if(performance[i,2] == 3){performance[i,2] <- "bologna-fc-1909"}
  if(performance[i,2] == 4){performance[i,2] <- "cagliari"}
  if(performance[i,2] == 5){performance[i,2] <- "crotone"}
  if(performance[i,2] == 6){performance[i,2] <- "acf-fiorentina"}
  if(performance[i,2] == 7){performance[i,2] <- "genoa-cfc"}
  if(performance[i,2] == 9){performance[i,2] <- "fc-internazionale-milano"}
  if(performance[i,2] == 10){performance[i,2] <- "juventus"}
  if(performance[i,2] == 11){performance[i,2] <- "lazio"}
  if(performance[i,2] == 12){performance[i,2] <- "ac-milan"}
  if(performance[i,2] == 13){performance[i,2] <- "napoli"}
  if(performance[i,2] == 14){performance[i,2] <- "parma-calcio-1913"}
  if(performance[i,2] == 15){performance[i,2] <- "as-roma"}
  if(performance[i,2] == 16){performance[i,2] <- "u.c.-sampdoria"}
  if(performance[i,2] == 17){performance[i,2] <- "u.s.-sassuolo"}
  if(performance[i,2] == 18){performance[i,2] <- "spezia-calcio"}
  if(performance[i,2] == 19){performance[i,2] <- "torino"}
  if(performance[i,2] == 20){performance[i,2] <- "udinese"}
  if(performance[i,2] == 8){performance[i,2] <- "hellas-verona"}
}

#rename col to ease merging
colnames(performance)[2:3] <- c("team", "Player.Name")

#merge tables
new_data <- merge(salary, performance, by=  "Player.Name")

#delete useless informations
new_data <- new_data[,-c(2,7,8)]

#create csv
write.csv(new_data, "Final_Dataset.csv")



