#Ejercicio 4
#NOTA : La documentación se realizó con Chat GPT (pq q flojera documentar a mano jeje)
#Alumnos:
#José Jorge Martínez de la Cruz
#Karla Naomi Pérez Silva
#Aury Stephanie Clara Rivera
# Fijar semilla para la reproducibilidad de los resultados
set.seed(678)

# Generar una muestra de tamaño 50 a partir de una distribución normal con media -1 y desviación estándar 5
muestra_x <- rnorm(50, mean = -1, sd = 5)

# Generar una muestra de tamaño 30 a partir de una distribución normal con media 1 y desviación estándar 5
muestra_y <- rnorm(30, mean = 1, sd = 5)

# Calcular los promedios de ambas muestras
promedio_x <- mean(muestra_x)
promedio_y <- mean(muestra_y)

# Cálculo de la varianza de la muestra X (corregida con el tamaño muestral)
S_x <- ((muestra_x - promedio_x) ** 2) / 50
S_x <- sum(S_x)

# Cálculo de la varianza de la muestra Y (corregida con el tamaño muestral)
S_y <- ((muestra_y - promedio_y) ** 2) / 30
S_y <- sum(S_y)

# Calcular el valor de 'a' como el inverso de las muestras combinadas
a = (1 / 50) + (1 / 30)

# Cálculo de una estimación combinada de la desviación estándar (sigmas) para las muestras
sigmas <- ((49 * S_x + 29 * S_y) / 78) * a
sigmas <- sqrt(sigmas)

# Crear un data frame para almacenar los datos de ambas muestras y su respectivo grupo
datos <- data.frame(
  Valores = c(muestra_x, muestra_y),
  Grupo = factor(c(rep("Muestra X", length(muestra_x)), rep("Muestra Y", length(muestra_y))))
)

# Graficar las densidades de ambas muestras
ggplot(datos, aes(x = Valores, fill = Grupo, color = Grupo)) +
  geom_density(alpha = 0.4) +  # Dibuja la densidad con una transparencia del 40%
  labs(title = "Densidades de Muestra X y Muestra Y",
       x = "Valor",
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(values = c("Muestra X" = "lightblue", "Muestra Y" = "pink")) +  
  scale_color_manual(values = c("Muestra X" = "blue", "Muestra Y" = "red"))

# Calcular los límites del intervalo de confianza para la diferencia de promedios
limite_inferior <- (promedio_x - promedio_y) - 2.64 * sigmas
limite_superior <- (promedio_x - promedio_y) + 2.65 * sigmas

# Crear un data frame para representar el intervalo de confianza
datos_intervalo <- data.frame(
  PuntoCentral = 0,
  LimiteInferior = limite_inferior,
  LimiteSuperior = limite_superior
)

# Graficar el intervalo de confianza centrado en 0 para la diferencia de promedios
ggplot(datos_intervalo, aes(x = 3, y = PuntoCentral)) +
  geom_point(color = "green", size = 1) +  # Representar el punto central en 0
  geom_errorbar(aes(ymin = LimiteInferior, ymax = LimiteSuperior), width = 0.1, color = "red") +  # Dibujar el intervalo de confianza
  labs(title = "Intervalo de confianza centrado en 0 para la diferencia de promedios",
       x = "",
       y = "Diferencia de promedios") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Quitar el texto del eje x
        axis.ticks.x = element_blank())  # Quitar las marcas del eje x
