# carregando os pacotes
library(tidyverse)
library(RSelenium)
library(netstat)

# terminate the selenium server
system("taskkill /im java.exe /f")


# Inciando o server
# É necessário ter o JAVA JDK instalado
#https://www.youtube.com/watch?v=IJ-PJbvJBGs&ab_channel=ProgrammingKnowledge
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '104.0.5112.79', #Inserir versão do chrome instalado
                             verbose = FALSE)

# criando o client object
remDr <- rs_driver_object$client

#Encontrando padrão das páginas e criando url para cada página
#1 hospede
url_antes <- "https://www.airbnb.com.br/s/Campos-do-Jord%C3%A3o-~-SP/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&flexible_trip_lengths%5B%5D=one_week&place_id=ChIJff6EvbiIzJQRXrpQN8sjgDQ&date_picker_type=calendar&source=structured_search_input_header&search_type=filter_change&adults=5&query=Campos%20do%20Jord%C3%A3o%20-%20SP&federated_search_session_id=37839ae2-fc24-40d1-98d4-b9518a6410ed&pagination_search=true&items_offset="
url_depois <- "&section_offset=3"

#Criando vetores vazios para posteriormente criar o dataframe

superhost_vector <- preco_vector <- titulos_vector <- nomes_vector <- links_vector <- avaliacao_vector <- hospedes_vector <- quartos_vector <- camas_vector <- banheiros_vector <- lat_vector <- long_vector <- comentario_vector <- tipo_vector <- c()
#u=0
#Vou rodar o bloco separadamente para diminuir a chance de bloqueio do web scraping por parte do airbnb e também para minimizar o tempo de run caso falhe
#Primeiro Bloco 0-80 (5 primeiras páginas)
#Segundo Bloco 100-180 (6 à 10 página)
#Terceiro Bloco 200-280 (11 à 15 página)

for (u in seq(from=0,to=260,by=20))
#for (u in seq(from=100,to=180,by=20))
#for (u in seq(from=200,to=280,by=20))
{
  #navegando até o site]
  url_completo <- paste0(url_antes, u, url_depois)
  remDr$navigate(url_completo)
  #Função utilizada para evitar bloqueio e para permitir que a página carregue
  Sys.sleep(sample(2:4, 1))
  
  # ---------------------- Identificando o preço 
  preco_objects <- remDr$findElements(using = 'class name', '_tyxjp1')
  preco_values <- lapply(preco_objects, function (x) x$getElementText()) %>% 
    unlist() %>% 
    str_remove_all('[R$.]')
  # Convertendo de string para número
  preco_values = preco_values %>% 
    as.numeric()
  preco_vector <- append(preco_vector, preco_values)

  # ---------------------- Identificando os títulos
  titulos_object <- remDr$findElements(using = 'class name', 't1jojoys')
  titulos_values <- lapply(titulos_object, function (x) x$getElementText()) %>% 
    unlist()
  titulos_vector <- append(titulos_vector, titulos_values)
  
  # ---------------------- Identificando os nomes
  nomes_object <- remDr$findElements(using = 'class name', 'tjbvqj3')
  nomes_values <- lapply(nomes_object, function (x) x$getElementText()) %>% 
    unlist()
  nomes_vector <- append(nomes_vector, nomes_values)
  
  # ---------------------- Identificando os links
  links_object <- remDr$findElements(using = 'class name', 'ln2bl2p')
  links_values <- lapply(links_object, function (x) x$getElementAttribute('href')) %>% 
    unlist()
  links_vector <- append(links_vector, links_values)
  
  # ---------------------- Identificando a avaliação
  avaliacao_objects <- remDr$findElements(using = 'class name', 'ru0q88m')
  avaliacao_values <- lapply(avaliacao_objects, function (x) x$getElementText()) %>% 
    unlist() %>%
    str_split_fixed(" ", 2)
  avaliacao_values <- avaliacao_values[,-2]  
  avaliacao_values <-  str_replace_all(avaliacao_values, ",", ".")
  avaliacao_values <-  str_replace_all(avaliacao_values, "Novo", NA_character_)
  # Convertendo de string para número
  avaliacao_values = avaliacao_values %>% 
    as.numeric()
  avaliacao_vector <- append(avaliacao_vector, avaliacao_values)

  #i<-18
   #for(i in 1:5){
   for(i in 1:length(links_values)){
    remDr$navigate(remDr$navigate(links_values[i]))
    Sys.sleep(sample(2:3, 1))
    
    # ---------------------- Identificando tipo de acomodação
    flag <- 1
    tipo_object <- tryCatch(remDr$findElement(using = 'class name', '_cv5qq4'), error=function(err){flag <<- 0})
    if(flag==0){
      tipo_vector <- append(tipo_vector, NA)
    }else{
      tipo_values <- tipo_object$getElementText()
      tipo_values <- unlist(tipo_values)
      tipo_vector <- append(tipo_vector, tipo_values)
    }
    
    # ---------------------- Identificando informações gerais
    
    geral_object <- remDr$findElements(using = 'class name', 'lgx66tx')
    geral_values <- lapply(geral_object, function (x) x$getElementText()) %>% 
      unlist()
    geral_values <- str_split_fixed(geral_values, " · ",4)

    if(length(geral_values)==0){
      hospedes_vector <- append(hospedes_vector, NA)
      quartos_vector <- append(quartos_vector, NA)
      camas_vector <- append(camas_vector, NA)
      banheiros_vector <- append(banheiros_vector, NA)
    }else{
      hospedes_vector <- append(hospedes_vector, geral_values[1,1])
      quartos_vector <- append(quartos_vector, geral_values[1,2])
      camas_vector <- append(camas_vector, geral_values[1,3])
      banheiros_vector <- append(banheiros_vector, geral_values[1,4])
    }
    
    
    # ---------------------- Identificando os comentários
    flag <- 1
    comentario_object <- tryCatch(remDr$findElement(using = 'class name', '_s65ijh7'), error=function(err){flag <<- 0})
    if(flag==0){
      comentario_vector <- append(comentario_vector, NA)
    }else{
      comentario_value <- as.numeric(str_remove_all(comentario_object$getElementText(), "[ comentários]"))
      comentario_vector <- append(comentario_vector, comentario_value)
    }
    
    
    # ---------------------- Identificando se é superhost
    flag <- 1
    superhost_object <- tryCatch(remDr$findElement(using = 'class name', '_1mhorg9'), error=function(err){flag <<- 0})
    if(flag==0){
      #print(0)
      superhost_vector <- append(superhost_vector, 0)
    }else{
      #print(1)
      superhost_vector <- append(superhost_vector, 1)
    }
    
    # ---------------------- Identificando a latide e longitude
    remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
    Sys.sleep(sample(2:4, 1))
    
    flag <- 1
    scroll = tryCatch(remDr$findElement(using = 'class name', '_10u42wu'), error=function(err){flag <<- 0})
    if(flag==0){
      print("NAO CONSIGO FAZER SCROLL")
    }else{
      remDr$executeScript("arguments[0].scrollIntoView(true);", list(scroll))
      Sys.sleep(sample(2:4, 1))
    }
    
    
    flag <- 1
    local_object <- tryCatch(remDr$findElement(using = 'xpath', '//*[@aria-label="Abrir esta área no Google Maps (abre uma nova janela)"]'), error=function(err){flag <<- 0})
    if(flag==0){
      lat_vector <- append(lat_vector, NA)
      long_vector <- append(long_vector, NA)
      
    }else{
      local_values <- local_object$getElementAttribute('href')
      local_values <- str_split_fixed(local_values,",", 2)
      lat_values <- str_split_fixed(local_values[1,1],"=",2)
      lat_values <- lat_values[1,2]
      lat_values <- as.numeric(lat_values) 
      long_values <- str_split_fixed(local_values[1,2],"&",2)
      long_values <- long_values[1,1]
      long_values <- as.numeric(long_values)
      lat_vector <- append(lat_vector, lat_values)
      long_vector <- append(long_vector, long_values)
    }
    
    
    #print(tail(hospedes_vector, n=1))
    #print(tail(tipo_vector, n=1))
          
    #if((is.na(tail(hospedes_vector, n=1))==TRUE) | (is.na(tail(tipo_vector, n=1))==TRUE)){
    #  print("Vamos tentar de novo")
    #  i <- (i-1)
    #}else{
    #  print("-OK-")
    #}
    
    cat(" - Anúncio ", i, "da pág. ", u)
  }
}

# ---------------------- MOntando o data frame
my_data_5hosp <- data.frame(titulos_vector,nomes_vector,superhost_vector,preco_vector,links_vector,avaliacao_vector,hospedes_vector, banheiros_vector, quartos_vector, camas_vector,lat_vector, long_vector, comentario_vector, tipo_vector )    
save(my_data_5hosp, file = "my_data_5hosp.RData")


# terminate the selenium server
system("taskkill /im java.exe /f")


# ---------------------- Combinando diferentes DFs
load("my_data_1hosp.RData")
load("my_data_2hosp.RData")
load("my_data_3hosp.RData")
load("my_data_4hosp.RData")
load("my_data_5hosp.RData")
load("my_data_6hosp.RData")
load("my_data_7hosp.RData")
load("my_data_8hosp.RData")


completo <- rbind(my_data_1hosp,my_data_2hosp)
completo <- rbind(completo,my_data_3hosp)
completo <- rbind(completo,my_data_4hosp)
completo <- rbind(completo,my_data_5hosp)
completo

# ---------------------- Criando coluna ID
completo <- completo %>% mutate(
  id = str_extract(links_vector, "(?<=https://www.airbnb.com.br/rooms/).+(?=adults)") 
)
completo

# ---------------------- Apagando duplicados 
completo <- distinct(completo,lat_vector, .keep_all= TRUE)
completo
save(completo, file = "completo.RData")
