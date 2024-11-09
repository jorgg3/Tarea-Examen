#Ejercicio 1
#NOTA : La documentación se realizó con Chat GPT (pq q flojera documentar a mano jeje)
#Alumnos:
#José Jorge Martínez de la Cruz
#Karla Naomi Pérez Silva
#Aury Stephanie Clara Rivera
# Fija la semilla para obtener resultados reproducibles al generar números aleatorios
set.seed(236)

# Genera una matriz de 100x100 con valores aleatorios normalmente distribuidos
# Cada valor tiene una media de 2 y una desviación estándar de 2
muestras <- matrix(rnorm(100 * 100, mean = 2, sd = 2), nrow = 100, ncol = 100)

# Muestra la matriz generada
print(muestras)

# Calcula el promedio de cada columna de la matriz 'muestras'
promedio_por_columnas <- colMeans(muestras)

# Muestra los promedios calculados por columna
print(promedio_por_columnas)

# Calcula la suma de cuadrados de las diferencias entre cada elemento y el promedio de su columna
# Luego divide cada suma por 100 para obtener el valor solicitado
S <- colSums(
  (muestras - matrix(promedio_por_columnas, nrow = nrow(muestras), ncol = ncol(muestras), byrow = TRUE))^2
) / 99

# Muestra el resultado final, que es un vector con la suma de cuadrados de las diferencias por columna
print(suma_cuadrados_diferencias)

# Instalar y cargar ggplot2 si no está instalado
install.packages("ggplot2")
library(ggplot2)

# Calcular los límites del intervalo (± 0.33 alrededor de cada promedio)
limite_inferior <- promedio_por_columnas - 0.33
limite_superior <- promedio_por_columnas + 0.33

# Crear un data frame para ggplot
datos <- data.frame(
  Columna = 1:length(promedio_por_columnas),
  PuntoCentral = rep(2, length(promedio_por_columnas)),  # Punto central en 2
  LimiteInferior = limite_inferior,
  LimiteSuperior = limite_superior
)

# Graficar
ggplot(datos, aes(x = Columna, y = PuntoCentral)) +
  geom_point(color = "green") +  # Cambia el color de los puntos
  geom_errorbar(aes(ymin = LimiteInferior, ymax = LimiteSuperior), width = 0.2, color = "lightblue") +  # Cambia el color de las barras de error
  labs(title = "Intervalo fijo de (x̄ - 0.33 ,x̄+ 0.33) para cada promedio",
       x = "Columna",
       y = "Intervalo alrededor del punto 2") +
  theme_minimal()



limite_inferior <- promedio_por_columnas - 0.392
limite_superior <- promedio_por_columnas + 0.392

# Crear un data frame para ggplot
datos <- data.frame(
  Columna = 1:length(promedio_por_columnas),
  PuntoCentral = rep(2, length(promedio_por_columnas)),  # Punto central en 2
  LimiteInferior = limite_inferior,
  LimiteSuperior = limite_superior
)

# Graficar
ggplot(datos, aes(x = Columna, y = PuntoCentral)) +
  geom_point(color = "green") +  # Cambia el color de los puntos
  geom_errorbar(aes(ymin = LimiteInferior, ymax = LimiteSuperior), width = 0.2, color = "yellow") +  # Cambia el color de las barras de error
  labs(title = "Intervalo fijo de (x̄ - 0.392 ,x̄+ 0.392) para cada promedio",
       x = "Columna",
       y = "Intervalo alrededor del punto 2") +
  theme_minimal()


limite_inferior <- promedio_por_columnas - 0.516
limite_superior <- promedio_por_columnas + 0.516

# Crear un data frame para ggplot
datos <- data.frame(
  Columna = 1:length(promedio_por_columnas),
  PuntoCentral = rep(2, length(promedio_por_columnas)),  # Punto central en 2
  LimiteInferior = limite_inferior,
  LimiteSuperior = limite_superior
)

# Graficar
ggplot(datos, aes(x = Columna, y = PuntoCentral)) +
  geom_point(color = "green") +  # Cambia el color de los puntos
  geom_errorbar(aes(ymin = LimiteInferior, ymax = LimiteSuperior), width = 0.2, color = "blue") +  # Cambia el color de las barras de error
  labs(title = "Intervalo fijo de (x̄ - 0.516,x̄+ 0.516) para cada promedio",
       x = "Columna",
       y = "Intervalo alrededor del punto 2") +
  theme_minimal()


# Calcula la suma de cuadrados de las diferencias entre cada valor y el promedio de su columna,
# luego divide entre 99 para obtener la varianza muestral
SS <- colSums(
  (muestras - matrix(promedio_por_columnas, nrow = nrow(muestras), ncol = ncol(muestras), byrow = TRUE))^2
) / 99

# Calcula la desviación estándar muestral tomando la raíz cuadrada de SS
S <- sqrt(SS)

# Define los límites del intervalo usando un factor de 2.6259/10 multiplicado por la desviación estándar
limite_inferior <- promedio_por_columnas - (2.6259 / 10) * S
limite_superior <- promedio_por_columnas + (2.6259 / 10) * S

# Crear un data frame para usar en ggplot
datos <- data.frame(
  Columna = 1:length(promedio_por_columnas),
  PuntoCentral = rep(2, length(promedio_por_columnas)),  # Punto central en 2
  LimiteInferior = limite_inferior,
  LimiteSuperior = limite_superior
)

# Graficar los intervalos de confianza para cada columna usando un punto central en 2
ggplot(datos, aes(x = Columna, y = PuntoCentral)) +
  geom_point(color = "green") +  # Color de los puntos en verde
  geom_errorbar(aes(ymin = LimiteInferior, ymax = LimiteSuperior), width = 0.2, color = "yellow") +  # Barras de error en amarillo
  labs(x = "Columna",
       y = "Intervalo alrededor del punto 2") +
  theme_minimal()

# Define nuevos límites del intervalo usando un factor diferente (1.984/10) multiplicado por la desviación estándar
limite_inferior <- promedio_por_columnas - (1.984 / 10) * S
limite_superior <- promedio_por_columnas + (1.984 / 10) * S

# Crear un nuevo data frame para usar en ggplot
datos <- data.frame(
  Columna = 1:length(promedio_por_columnas),
  PuntoCentral = rep(2, length(promedio_por_columnas)),  # Punto central en 2
  LimiteInferior = limite_inferior,
  LimiteSuperior = limite_superior
)

# Graficar los intervalos de confianza con el nuevo factor
ggplot(datos, aes(x = Columna, y = PuntoCentral)) +
  geom_point(color = "green") +  # Color de los puntos en verde
  geom_errorbar(aes(ymin = LimiteInferior, ymax = LimiteSuperior), width = 0.2, color = "pink") +  # Barras de error en rosa
  labs(x = "Columna",
       y = "Intervalo alrededor del punto 2") +
  theme_minimal()

