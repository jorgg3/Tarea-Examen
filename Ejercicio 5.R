#Ejercicio 5
#NOTA : La documentación se realizó con Chat GPT (pq q flojera documentar a mano jeje)
#Alumnos:
#José Jorge Martínez de la Cruz
#Karla Naomi Pérez Silva
#Aury Stephanie Clara Rivera
# Definición de un vector numérico llamado 'muestra' que contiene 20 valores
muestra <- c(29.1, 207.6, 81.8, 0.8, 76.1, 108.9, 48.4, 108.1, 
             52.2, 272.8, 150.5, 80.3, 97.4, 11.5, 46.2, 144.1, 
             62.5, 262.9, 247.6, 4.1)

# Cálculo del estimador de máxima verosimilitud (EMV) de la media de la muestra
EMV <- mean(muestra)

# Calcula la raíz cuadrada del tamaño de la muestra (20)
raiz <- sqrt(20)

# Establece el valor crítico 'q' de una distribución normal estándar para un nivel de confianza del 95%
q <- 1.96

# Cálculo del límite inferior del intervalo de confianza usando una fórmula alternativa
liminf_1 <- (raiz * EMV) / (q + raiz)

# Cálculo del límite superior del intervalo de confianza usando una fórmula alternativa
limsup_1 <- (raiz * EMV) / (-q + raiz)

# Cálculo de un segundo límite inferior del intervalo de confianza con una fórmula diferente
liminf_2 <- ((-q / raiz) + 1) * EMV

# Cálculo de un segundo límite superior del intervalo de confianza con una fórmula diferente
limsup_2 <- ((q / raiz) + 1) * EMV

# Cálculo de la suma de todos los elementos en el vector 'muestra'
Suma <- sum(muestra)
