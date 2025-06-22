# Grupo 5: Ariel Alzogaray Flores, Damian Cevallos Ortiz y Josefina Deserti Gotta

# Escenario 1, Ejercicio 1

# Creamos una muestra del peso total de los pasajeros en 1000 viajes de avión

muestra <- numeric(1000)
for (i in 1:1000) { # Cada dato en la muestra es la suma de los pesos de 81 pasajeros adultos, es decir, el peso total en un vuelo
  muestra[i] <- sum(rnorm(81, mean=70, sd = 7))  
}

# Contemos la cantidad de casos en donde superan los 5500 kg

cantRendNoOptimo <- sum(muestra>5500)

# Calculamos la probabilidad

probaRendNoOptimo <- cantRendNoOptimo/length(muestra)

