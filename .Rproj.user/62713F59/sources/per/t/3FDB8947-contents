library(dplyr)
library(pastecs)

load("completo.RData")

# ---------------------- Tratando os missing values
colSums(is.na(completo))
#Vamos retirar todos as linhas que contém NA na avaliação... no caso essas mesmas linhas possuem NA nos comentarios
completo <- na.omit(completo) 
colSums(is.na(completo))

# ---------------------- Retirando colunas que naõ nos ajudarão nos próximos passos
completo$titulos_vector <- NULL
completo$nomes_vector <- NULL

# ---------------------- Tratando o número de hospedes
aggregate(data.frame(count = completo$hospedes_vector), list(value = completo$hospedes_vector), length)
#Nada a fazer por aqui

# ---------------------- Tratando o número de banheiros
aggregate(data.frame(count = completo$banheiros_vector), list(value = completo$banheiros_vector), length)
#Vamos tratar o termo "meio" como "lavabo", ou seja, desconsiderado nessa variável (olhar seção "Apagando textos e convertendo em números")

# ---------------------- Tratando o número de quartos
aggregate(data.frame(count = completo$quartos_vector), list(value = completo$quartos_vector), length)
#Tratando exceções 
completo$quartos_vector[completo$quartos_vector=="2 camas"] <- "1 quarto"
completo$quartos_vector[completo$quartos_vector=="Estúdio"] <- "1 quarto"
aggregate(data.frame(count = completo$quartos_vector), list(value = completo$quartos_vector), length)

# ---------------------- Tratando o número de camas
aggregate(data.frame(count = completo$camas_vector), list(value = completo$camas_vector), length)
#Tratando exceções 
completo$camas_vector[completo$camas_vector=="1 banheiro"] <- "2 camas"
completo$camas_vector[completo$camas_vector=="1 banheiro privado"] <- "1 cama"
aggregate(data.frame(count = completo$camas_vector), list(value = completo$camas_vector), length)


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
#Estatistica basica
stat.desc(completo$preco_vector) 

# Scatter plot with the 2d density estimation
sp <- ggplot(completo, aes(x=long_vector, y=lat_vector)) +
  geom_point()
sp + geom_density_2d()

sp + geom_density_2d_filled(alpha = 0.5) +
  geom_density_2d(size = 0.25, colour = "black")
# ---------------------- Histograma dos preços

ggplot(completo) +
  aes(x=preco_vector) +
  geom_histogram(fill="darkblue",
                 col="black",
                 alpha=0.5,
                 bins=20,
                 aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = mean(completo$preco_vector), sd=sd(completo$preco_vector)))+
  labs(title = "Histograma de Preços")

# ---------------------- Boxplot do tipo de acomodação e preço
completo %>%
  ggplot(aes(reorder(tipo_vector,preco_vector),preco_vector))+
  geom_jitter(width=0.2)+
  geom_boxplot(fill="darkblue",
               alpha=0.5)

# ---------------------- Scatter das avaliacoes em relacao ao preço
ggplot(completo, aes(x=avaliacao_vector, y=preco_vector)) + 
  geom_point()

# ---------------------- Scatter dos comentarios em relacao ao preço
ggplot(completo, aes(x=comentario_vector, y=preco_vector)) + 
  geom_point()

ggplot(completo, aes(x=log(1+comentario_vector),  y=preco_vector)) + 
  geom_point()


# ---------------------- Boxplot do numero de quartos em relacao ao preço
completo %>%
  ggplot(aes(quartos_vector,preco_vector,group=quartos_vector))+
  geom_jitter(width=0.2)+
  geom_boxplot(fill="darkblue",
               alpha=0.5)

# ---------------------- Boxplot do numero de banheiros em relacao ao preço
completo %>%
  ggplot(aes(banheiros_vector,preco_vector,group=banheiros_vector))+
  geom_jitter(width=0.2)+
  geom_boxplot(fill="darkblue",
               alpha=0.5)

# ---------------------- Boxplot do numero de cama em relacao ao preço
completo %>%
  ggplot(aes(camas_vector,preco_vector,group=camas_vector))+
  geom_jitter(width=0.2)+
  geom_boxplot(fill="darkblue",
               alpha=0.5)

# ---------------------- Boxplot do numero de hospedes em relacao ao preço
completo %>%
  ggplot(aes(hospedes_vector,preco_vector,group=hospedes_vector))+
  geom_jitter(width=0.2)+
  geom_boxplot(fill="darkblue",
               alpha=0.5)

# ---------------------- LET'S GOOOOO

completo$comentario_log_vector <- log(1+completo$comentario_vector)
completo$quartos_por_hospedes_vector <- completo$quartos_vector/completo$hospedes_vector
