library(dplyr)

# ---------------------- Carregando arquivo do web scraping
load("completo.RData")

# ---------------------- Retirando colunas que naõ nos ajudarão nos próximos passos
completo$titulos_vector <- NULL
completo$nomes_vector <- NULL

# ---------------------- Tratando os missing values
colSums(is.na(completo))
#Corrigindo erro do codigo de webscraping...substituindo os "NA" da coluna comentários por 0
completo[["comentario_vector"]][is.na(completo[["comentario_vector"]])] <- 0
colSums(is.na(completo))

#Vamos retirar todos as linhas que contém NA na avaliação... listwise deletion
completo <- na.omit(completo) 
colSums(is.na(completo))

# ---------------------- Tratando o número de hospedes
aggregate(data.frame(count = completo$hospedes_vector), list(value = completo$hospedes_vector), length)
#Nada a fazer por aqui

# ---------------------- Tratando o número de banheiros
aggregate(data.frame(count = completo$banheiros_vector), list(value = completo$banheiros_vector), length)
#Vamos tratar o termo "meio" como "lavabo", ou seja, desconsiderado nessa variável (olhar seção "Apagando textos e convertendo em números")

# ---------------------- Tratando o número de quartos
aggregate(data.frame(count = completo$quartos_vector), list(value = completo$quartos_vector), length)
#Há uma coluna com dados de cama na coluna de quartos... vamos apagar essa linha para evitar erros
completo<-completo[!(completo$quartos_vector=="2 camas"),]
aggregate(data.frame(count = completo$quartos_vector), list(value = completo$quartos_vector), length)

#Tratando exceções... vamos considerar estudio como um quarto neste trabalho
completo$quartos_vector[completo$quartos_vector=="Estúdio"] <- "1 quarto"
aggregate(data.frame(count = completo$quartos_vector), list(value = completo$quartos_vector), length)

# ---------------------- Tratando o número de camas
aggregate(data.frame(count = completo$camas_vector), list(value = completo$camas_vector), length)
#Nada a fazer por aqui

# ---------------------- Tratando os tipos de acomodação
aggregate(data.frame(count = completo$tipo_vector), list(value = completo$tipo_vector), length)
#Primeiro vamos limpas o nome dos proprietários
completo$tipo_vector <- gsub("\\(.*","", completo$tipo_vector)
aggregate(data.frame(count = completo$tipo_vector), list(value = completo$tipo_vector), length)
#Agora vamos retirar as diferentes subcategorias (chale, loft, etc)
completo$tipo_vector <- gsub(":.*","", completo$tipo_vector)
aggregate(data.frame(count = completo$tipo_vector), list(value = completo$tipo_vector), length)
#Agora vamos retirar onde o quarto fica situado (pousada, casa, etc)
completo$tipo_vector <- gsub("em.*","", completo$tipo_vector)
aggregate(data.frame(count = completo$tipo_vector), list(value = completo$tipo_vector), length)
#Tratando exceções 
completo$tipo_vector[completo$tipo_vector=="Casa na árvore "] <- "Espaço inteiro"
completo$tipo_vector[completo$tipo_vector=="Casa na terra "] <- "Espaço inteiro"
completo$tipo_vector[completo$tipo_vector=="Casebre "] <- "Espaço inteiro"
completo$tipo_vector[completo$tipo_vector=="Microcasa "] <- "Espaço inteiro"
completo$tipo_vector[completo$tipo_vector=="O lugar inteiro "] <- "Espaço inteiro"
completo$tipo_vector[completo$tipo_vector=="Quarto "] <- "Quarto inteiro "
completo$tipo_vector[completo$tipo_vector=="Quarto inteiro "] <- "Quarto inteiro"
aggregate(data.frame(count = completo$tipo_vector), list(value = completo$tipo_vector), length)

# ---------------------- Apagando textos das colunas
completo <- completo %>% mutate(
  hospedes_vector = str_extract(hospedes_vector, "\\d"),
  banheiros_vector  = str_extract(banheiros_vector, "\\d"),
  quartos_vector   = str_extract(quartos_vector, "\\d"),
  camas_vector   = str_extract(camas_vector, "\\d")
  )

# ---------------------- Convertendo em números
str(completo)
completo$hospedes_vector <- as.numeric(as.character(completo$hospedes_vector))
completo$banheiros_vector <- as.numeric(as.character(completo$banheiros_vector))
completo$quartos_vector <- as.numeric(as.character(completo$quartos_vector))
completo$camas_vector <- as.numeric(as.character(completo$camas_vector))
str(completo)

# ---------------------- Renomeando colunas
str(completo)
colnames(completo) <- c("superhost","precos", "links", "avaliacoes", "hospedes", "banheiros","quartos","camas","latitude","longitude","comentarios","tipo","id")
str(completo)
df <- completo

# ---------------------- Selecionando apenas linhas que estejam localizadas em bairros
load('df.RData')
df <- df[df$precos < 1800, ]
# Criando um objeto do tipo sf a partir de um data frame:
sf_df <- st_as_sf(x = df, 
                  coords = c("longitude", "latitude"), 
                  crs = 4326)


# Carregando um shapefile dos bairros de Campos do Jordão
shp_bairros <- readOGR("SP_campos_do_jordao_bairros_2022", "BAIRROS")
class(shp_bairros)
#Visualiazação tabular
class(shp_bairros)
shp_bairros@data %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Visualização gráfica do objeto shp_bairros:
tm_shape(shp = shp_bairros) + 
  tm_borders()

# Combinando o objeto shp_bairros com o objeto sf_df:
tmap_mode("view")
tm_shape(shp = shp_bairros) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_df) + 
  tm_dots(col = "red", 
          border.col = "black", 
          size = 0.04, 
          alpha = 0.8)
# Pegando apenas observações que possuam um bairro atrelado
polygonSF_bairros <- read_sf(dsn = 'SP_campos_do_jordao_bairros_2022')
class(polygonSF_bairros)
df_bairros <- st_intersection(sf_df, polygonSF_bairros)
df_bairros
# Plotando sf e shp considerando novamente
sf_df_bairros <- st_as_sf(x = df_bairros, 
                  coords = c("longitude", "latitude"), 
                  crs = 4326)

tm_shape(shp = shp_bairros) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_df_bairros) + 
  tm_dots(col = "red", 
          border.col = "black", 
          size = 0.04, 
          alpha = 0.8)

df_bairros$longitude <- st_coordinates(df_bairros)[,1]
df_bairros$latitude <- st_coordinates(df_bairros)[,2]


# ---------------------- Plotando em relação aos preços

df_teste <- aggregate(df_bairros[,2], list(df_bairros$bairro), mean)
colnames(df_teste)[1] = "Name"
colnames(df_teste)[2] = "preco"
df_teste['count'] <- aggregate(df_bairros$bairro, by = list(df_bairros$bairro), FUN = length)[2]

#df_teste <- df_teste[ ,c('Name', 'count')]
#df_teste <- df_teste[ ,c('Name', 'preco')]


teste <- sp::merge(shp_bairros, df_teste, by = "Name")
tm_shape(teste) +
  tm_polygons("preco", 
              title = "Preços", contrast = 0.7, 
              breaks = c(200, 400, 600, 800, 1000, 1200 ),
              labels = c("R$200 - R$400", "R$400 - R$600", "R$600 - R$800", "R$800 - R$1000", "R$1000 - R$1200"),
              border.col = "gray30", id = "name") 

tm_shape(teste) +
  tm_polygons("count", 
              title = "Número de acomodações", contrast = 0.7, 
              breaks = c(1, 5, 10, 15, 20, 25,30,35 ),
              labels = c("1-5", "6-10", "11-15", "16-20", "21-25","26-30","31-35"),
              border.col = "gray30", id = "name") +
  



# Renomeando coluna dos bairros
names(df_bairros)[names(df_bairros) == "Name"] <- "bairro"
df_bairros[ ,c('descriptio', 'timestamp','begin','end','altitudeMo','tessellate','extrude','visibility','drawOrder','icon')] <- list(NULL)
length(df_bairros)


#df_bairros <- dummy_cols(df_bairros, 
#                            select_columns = "bairro")
#df_bairros[ ,c('geometry','bairro')] <- list(NULL)
#save(df_bairros, file = "df_bairros.RData")
class(df_bairros)
df_bairros <- df_bairros %>% st_drop_geometry()
class(df_bairros)
df_bairros[ ,c('id','links')] <- list(NULL)
df_bairros <- df_bairros[df_bairros$precos < 1800, ]
save(df_bairros, file = "df_bairros.RData")

df_bairros_correlacao <- df_bairros



# ---------------------- Salvando em um novo arquivo
#save(df, file = "df.RData")
