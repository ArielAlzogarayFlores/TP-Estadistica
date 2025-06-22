# Grupo 5: Ariel Alzogaray Flores, Damian Cevallos Ortiz y Josefina Deserti Gotta

# Escenario 1, Ejercicio 1

# Creamos una muestra del peso total de los pasajeros en 1000 viajes de avión

muestra <- numeric(10000)
for (i in 1:10000) { # Cada dato en la muestra es la suma de los pesos de 81 pasajeros adultos, es decir, el peso total en un vuelo
  muestra[i] <- sum(rnorm(81, mean=70, sd = 7))  
}

# Contemos la cantidad de casos en donde superan los 5500 kg

cantRendNoOptimo <- sum(muestra>5500)

# Calculamos la probabilidad

probaRendNoOptimo <- cantRendNoOptimo/length(muestra)

# Escenario 2, ejercicio 1

muestra <- numeric(100000)
for (i in 1:100000){
  # 1 corresponde a adultos y 0 a niños
  pasajeros <- rbinom(n=81, size=1, prob=0.95)
  adultos <- sum(pasajeros == 1)
  niños <- sum(pasajeros == 0)
  pesoAdultos <- sum(rnorm(adultos, mean=70, sd=7))
  pesoNiños <- sum(rnorm(niños, mean=20, sd=5))
  muestra[i] <- pesoAdultos + pesoNiños
}

cantRendNoOptimo <- sum(muestra > 5500)
probaRendNoOptimo <- cantRendNoOptimo/length(muestra)

