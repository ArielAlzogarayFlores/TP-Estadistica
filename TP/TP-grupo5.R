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

# Escenario 2, ejercicio 1

muestra <- numeric(1000)
for (i in 1:1000){
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

# Escenario 3, ejercicio 1

p <- c(0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)
vectorProbaRendNoOptimo <-numeric(10)

for (i in 1:10){
  muestra <- numeric(1000)
  numeroPasajeros <- rpois(1000, 70)
  for (j in 1:1000){
    if (numeroPasajeros[j] > 81){
      numeroPasajeros[j] <- 81
    }
    pasajeros <- rbinom(n=numeroPasajeros[j], size=1, prob=p[i])
    adultos <- sum(pasajeros == 1)
    niños <- sum(pasajeros == 0)
    pesoAdultos <- sum(rnorm(adultos, mean=70, sd=7))
    pesoNiños <- sum(rnorm(niños, mean=20, sd=5))
    muestra[j] <- pesoAdultos + pesoNiños
  }
  cantRendNoOptimo <- sum(muestra > 5500)
  probaRendNoOptimo <- cantRendNoOptimo/length(muestra)
  vectorProbaRendNoOptimo[i] <- probaRendNoOptimo
}

# Escenario 3, ejercicio 2

p <- c(0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)
lambdas <- c(70, 75, 80, 85, 90, 95, 100)
matrizProbaRendNoOptimo <- matrix(nrow=length(p), ncol=length(lambdas), dimnames=list(p,lambdas))
for (i in 1:length(lambdas)){
  for (j in 1:length(p)){
    muestra <- numeric(1000)
    numeroPasajeros <- rpois(1000, lambdas[i]) # Cuántos pasajeros habrá en cada vuelo simulado
    for (k in 1:1000){ 
      if (numeroPasajeros[k] > 81){ # Correjimos vector `numeroPasajeros` para evitar simulaciones con más de 81 pasajeros
        numeroPasajeros[k] <- 81
      }
      pasajeros <- rbinom(n=numeroPasajeros[k], size=1, prob=p[j])
      adultos <- sum(pasajeros == 1)
      niños <- sum(pasajeros == 0)
      pesoAdultos <- sum(rnorm(adultos, mean=70, sd=7))
      pesoNiños <- sum(rnorm(niños, mean=20, sd=5))
      muestra[k] <- pesoAdultos + pesoNiños
    }
    cantRendNoOptimo <- sum(muestra > 5500)
    probaRendNoOptimo <- cantRendNoOptimo/length(muestra)
    matrizProbaRendNoOptimo[j,i] <- probaRendNoOptimo
  }
}

matplot(t(matrizProbaRendNoOptimo), xaxt = "n", pch=15:22, col=c(2:7,7),  type="b", ylab = "Probabilidad de RNO (Rendimiento No Óptimo)", xlab= "Valor de Lambda (λ)", main="Probabilidad de RNO de diferentes P's en función de lambda (λ)")
axis(1, at=1:length(lambdas), labels=lambdas)
legend("topleft", inset=0.01, legend=p, pch=15:22, col=c(2:7,7), horiz=FALSE)
