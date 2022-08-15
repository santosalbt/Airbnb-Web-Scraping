library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")
library("raster")

# ---------------------- Densidade de Kernel


# x = Matriz com duas colunas que contém as observações da distribuição em a densidade será estimada
# bandwidth = vetor que contém a largura de banda que será usada em cada direção. Maior distância de influência de cada ponto, neste caso em graus 
# gridsize = número de linhas e colunas (resolução) do arquivo gerado

kde <- bkde2D(completo[,c("long_vector","lat_vector")],
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

