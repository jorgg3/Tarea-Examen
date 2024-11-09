#Ejercicio 10
#NOTA : La documentación se realizó con Chat GPT (pq q flojera documentar a mano jeje)
#Alumnos:
#José Jorge Martínez de la Cruz
#Karla Naomi Pérez Silva
#Aury Stephanie Clara Rivera
# Definimos el vector de muestra original con 20 valores
# Definimos el vector de muestra original con 20 valores
muestra <- c(29.1, 207.6, 81.8, 0.8, 76.1, 108.9, 48.4, 108.1, 
             52.2, 272.8, 150.5, 80.3, 97.4, 11.5, 46.2, 144.1, 
             62.5, 262.9, 247.6, 4.1)

# Realizamos un remuestreo con reemplazo de la muestra para generar un vector del mismo tamaño
remuestreo <- sample(muestra, size = 20, replace = TRUE)

# Definimos el número de renglones y columnas de la matriz de remuestreos
renglones <- 1000
columnas <- 20

# Creamos una matriz de 1000 renglones y 20 columnas donde cada renglón es un remuestreo
# Cada remuestreo se realiza con reemplazo a partir del vector `muestra`
remuestreos <- replicate(renglones, sample(muestra, columnas, replace = TRUE))

# Transponemos la matriz para tener 1000 renglones y 20 columnas
remuestreos <- t(remuestreos)

# Calculamos la media de cada renglón (es decir, la media de cada remuestreo)
medias <- rowMeans(remuestreos)

# Instalamos y cargamos el paquete ggplot2 para realizar visualizaciones (solo necesario si aún no está instalado)
install.packages("ggplot2")  # Ejecutar solo una vez, luego puede comentarse
library(ggplot2)

# Creamos un data frame para facilitar la visualización de las medias
df <- data.frame(Media = medias)

# Generamos un histograma de las medias de los remuestreos
ggplot(df, aes(x = Media)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 20, 
                 fill = "skyblue", 
                 color = "black", 
                 alpha = 0.7) +  # alpha controla la transparencia de las barras
  geom_density(color = "red", size = 1.2) +  # Añade una línea de densidad en rojo
  labs(title = "Histograma de las Medias de los Remuestreos", 
       x = "Media", 
       y = "Densidad") +
  theme_minimal()  # Aplica un tema minimalista al gráfico

# Calculamos la media de todas las medias obtenidas de los remuestreos
media_medias <- mean(medias)

 # Ordenamos el vector de medias de mayor a menor
medias_ordenadas <- sort(medias, decreasing = TRUE)

# Seleccionamos el primer cuartil y el tercer cuartil para un intervalo de confianza aproximado
q1 <- medias_ordenadas[25]    # 2.5% inferior
q2 <- medias_ordenadas[975]   # 97.5% superior


