#Ejercicio 4
#NOTA : La documentación se realizó con Chat GPT (pq q flojera documentar a mano jeje)
#Alumnos:
#José Jorge Martínez de la Cruz
#Karla Naomi Pérez Silva
#Aury Stephanie Clara Rivera
# Definición de dos muestras de datos
muestra_x <- c(0.133, 0.0265, 0.0302, 0.0444, 0.0882, 0.1461, 0.1545, 0.1585, 0.2638)
muestra_y <- c(-0.1221, -0.1010, -0.0519, -0.0436, -0.0200, -0.0182, -0.0104, 0.0879, 0.1390, 0.1945)

# Cálculo de los promedios de cada muestra
promedio_x <- mean(muestra_x)  # Calcula el promedio de muestra_x
promedio_y <- mean(muestra_y)  # Calcula el promedio de muestra_y

# Cálculo de las desviaciones cuadradas para cada elemento en muestra_x
Sx <- ((muestra_x - promedio_x)^2) / 8  # Calcula la desviación cuadrada para cada valor en muestra_x
S_x <- sum(Sx)  # Suma las desviaciones cuadradas para obtener la suma total de muestra_x

# Cálculo de las desviaciones cuadradas para cada elemento en muestra_y
Sy <- ((muestra_y - promedio_y)^2) / 9  # Calcula la desviación cuadrada para cada valor en muestra_y
S_y <- sum(Sy)  # Suma las desviaciones cuadradas para obtener la suma total de muestra_y

# Definición de dos factores de escala (q1 y q2) para límites de confianza
q1 <- 1 / 5.91
q2 <- 5.467

# Cálculo de los límites inferior y superior basados en S_x y S_y
lim_inf <- (1 / q2) * (S_x / S_y)  # Límite inferior basado en S_x y S_y
lim_sup <- (1 / q1) * (S_x / S_y)  # Límite superior basado en S_x y S_y

#######

# Definición de una desviación estándar y un factor para el cálculo de límites
sig <- 0.1  # Desviación estándar dada
k <- sqrt(sig / 9 + sig / 10)  # Cálculo del factor k basado en sig y tamaños de muestra

# Límites de confianza para la diferencia de promedios utilizando k
lim_inf <- (promedio_x - promedio_y) - 2.58 * k  # Límite inferior usando el factor k y la diferencia de promedios
lim_sup <- (promedio_x - promedio_y) + 2.58 * k  # Límite superior usando el factor k y la diferencia de promedios

# Cálculo de una varianza combinada ponderada entre las dos muestras
Sp <- (8 * S_x + 9 * S_y) / 17  # Varianza combinada ponderada basada en S_x y S_y

# Límites de confianza basados en la varianza combinada y la diferencia de promedios
lim_inf <- (promedio_x - promedio_y) - 2.898 * sqrt((1 / 10 + 1 / 9) * Sp)  # Límite inferior con varianza combinada
lim_sup <- (promedio_x - promedio_y) + 2.898 * sqrt((1 / 10 + 1 / 9) * Sp)  # Límite superior con varianza combinada

# Otro cálculo de límites de confianza usando un valor predefinido
lim_inf <- (275 - (0.2475) * sqrt(500) * 2.58) / 500  # Límite inferior basado en valores predefinidos
lim_sup <- (275 + (0.2475) * sqrt(500) * 2.58) / 500  # Límite superior basado en valores predefinidos
