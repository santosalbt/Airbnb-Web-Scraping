
# Instalação e Carregamento dos Pacotes Necessários 

pacotes <- c("rvest","dplyr","pastecs","robotstxt","XML","stringr","ggmap", "data.table", "rstudioapi", "plotly", "ggplot2", "plyr","dplyr", "ggvis", "knitr", "httr", "tidyverse", "RSelenium", "netstat","tidyverse","sf","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "kableExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

