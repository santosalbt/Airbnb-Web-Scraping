library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")
library("raster")
library("corrplot")
library("fastDummies")
library("pastecs")
library("ggplot2")

#Estatistica basica
load("df_bairros.RData")


# ---------------------- Correlação
df_bairros_correlacao <- df_bairros
df_bairros_correlacao[ ,c('longitude','latitude','links','id','geometry','bairro')] <- list(NULL)

df_bairros_correlacao <- dummy_cols(df_bairros_correlacao,                             
                                    select_columns = "tipo")
df_bairros_correlacao$tipo <- NULL
m<- cor(df_bairros_correlacao)
corrplot(m,type="lower",method = "number")

# ----------------------  Gráfico de dispersão com a estimativa de densidade 2d

sp <- ggplot(df_bairros, aes(x=longitude, y=latitude)) +
  geom_point()
sp + geom_density_2d()

sp + geom_density_2d_filled(alpha = 0.5) +
  geom_density_2d(size = 0.5, colour = "black")



# ---------------------- Histograma dos preços
stat.desc(df_bairros$preco) 

ggplot(df_bairros) +
  aes(x=precos) +
  scale_x_continuous(breaks = c(50,150, 250, 350, 450,550,650,750,850,950,1050,1150,1250,1350,1450,1550,1650))+
  geom_histogram(fill="darkblue",
                 col="black",
                 alpha=0.5,
                 bins=20,
                 aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = mean(completo$preco_vector), sd=sd(completo$preco_vector)))+
  labs(title = "Histograma de Preços")

# ---------------------- Boxplot do tipo de acomodação e preço
df_bairros %>%
  ggplot(aes(tipo,precos))+
  #geom_jitter(width=0.2)+
  geom_boxplot(fill="darkblue",
               alpha=0.5)

# ---------------------- Scatter das avaliacoes em relacao ao preço
ggplot(df_bairros, aes(x=avaliacoes, y=precos)) + 
  geom_point()

# ---------------------- Scatter dos comentarios em relacao ao preço
#ggplot(df_bairros, aes(x=comentarios, y=precos)) + 
#  geom_point()

ggplot(df_bairros, aes(x=log(1+comentarios),  y=precos)) + 
  geom_point()


# ---------------------- Boxplot do numero de quartos em relacao ao preço
df_bairros %>%
  ggplot(aes(quartos,precos,group=quartos))+
  #geom_jitter(width=0.2)+
  geom_boxplot(fill="darkblue",
               alpha=0.5)

# ---------------------- Boxplot do numero de banheiros em relacao ao preço
df_bairros %>%
  ggplot(aes(banheiros,precos,group=banheiros))+
  #geom_jitter(width=0.2)+
  geom_boxplot(fill="darkblue",
               alpha=0.5)

# ---------------------- Boxplot do numero de cama em relacao ao preço
df_bairros %>%
  ggplot(aes(camas,precos,group=camas))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
  #geom_jitter(width=0.2)+
  geom_boxplot(fill="darkblue",
               outlier.shape = NA,
               alpha=0.5)

# ---------------------- Boxplot do numero de hospedes em relacao ao preço
df_bairros %>%
  ggplot(aes(hospedes,precos,group=hospedes))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
  #geom_jitter(width=0.2)+
  geom_boxplot(fill="darkblue",
               outlier.shape = NA,
               alpha=0.5)



# ---------------------- Densidade de Kernel


# x = Matriz com duas colunas que contém as observações da distribuição em a densidade será estimada
# bandwidth = vetor que contém a largura de banda que será usada em cada direção. Maior distância de influência de cada ponto, neste caso em graus 
# gridsize = número de linhas e colunas (resolução) do arquivo gerado

kde <- bkde2D(df_bairros[,c("longitude","latitude")],
              bandwidth=c(.006, .006), gridsize = c(1000,1000))

# Criando um Raster a partir da Densidade de Kernel criada
KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

# Criando uma função pal para colorir o Raster 
palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)

## Utilizando o mapa Leaflet com o raster
leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .6) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Densidade de Kernel")

# Atribuindo NA às células com densidade baixa (<10) para torná-las transparentes no mapa
KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] <- NA

# Utilizando a função palRaster para torná-las transparentes
palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")

# Plotando o mapa novamente
leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .6) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Densidade de Kernel")


# Caso queira um raster "binned"
palRaster <- colorBin("Spectral", bins = 7, domain = KernelDensityRaster@data@values, na.color = "transparent")
leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .5) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Densidade de Kernel")


