# Grupo 5: Ariel Alzogaray Flores, Damian Cevallos Ortiz y Josefina Deserti Gotta

# Escenario 1, Ejercicio 1

escenario1_ej1 <- function() {
  
  # La cantidad de pasajeros por vuelo simulado es siempre de 81 pasajeros
  cantPasajeros <- 81
  
  # Creamos una muestra del peso total de los pasajeros en 1000 viajes de avión
  muestra <- numeric(1000)
  
  # Cada dato en la muestra es la suma de los pesos de 81 pasajeros (todos adultos), es decir, el peso total en un vuelo
  for (i in 1:1000) {
    muestra[i] <- sum(rnorm(cantPasajeros, mean = 70, sd = 7))
  }
  
  # Contemos la cantidad de casos en donde superan los 5500 kg
  cantRendNoOptimo <- sum(muestra>5500)
  
  # Calculemos la probabilidad comparando estos casos contra la totalidad de la muestra
  probaRendNoOptimo <- cantRendNoOptimo/length(muestra)
  return(probaRendNoOptimo)
}

r_es1_ej1 <- escenario1_ej1()

# Escenario 2, ejercicio 1

escenario2_ej1 <- function() {
  
  # La cantidad de pasajeros por vuelo simulado es siempre de 81 pasajeros
  cantPasajeros <- 81
  
  # Creamos una muestra del peso total de los pasajeros en 1000 viajes de avión
  muestra <- numeric(1000)
  
  # Cada dato en la muestra es la suma de los pesos de 81 pasajeros
  for (i in 1:1000) {
    
    # Entendemos a cada pasajero como una bernoulli 0.95 (respecto a si es adulto o niño)
    pasajeros <- rbinom(n=cantPasajeros, size=1, prob=0.95) # 1 corresponde a adultos y 0 a niños
    
    # Contamos a la cantidad de adultos y niños
    adultos <- sum(pasajeros == 1)
    niños <- sum(pasajeros == 0)
    
    # A partir de la cantidad de adultos y niños estimamos el peso total según la distribución correspondiente al segmento
    pesoAdultos <- sum(rnorm(adultos, mean=70, sd=7))
    pesoNiños <- sum(rnorm(niños, mean=20, sd=5))
    
    # El peso total de los pasajeros se conforma por el peso total de ambos segmentos
    muestra[i] <- pesoAdultos + pesoNiños
  }
  
  # Contemos la cantidad de simulaciones en donde superan los 5500 kg
  cantRendNoOptimo <- sum(muestra>5500)
  
  # Calculemos la probabilidad comparando estos casos contra la totalidad de simulaciones en la muestra
  probaRendNoOptimo <- cantRendNoOptimo/length(muestra)
  return(probaRendNoOptimo)
}

r_es2_ej1<-escenario2_ej1()

# Escenario 3, ejercicio 1

escenario3ej1 <- function() {
  
  # Vector con las diferentes probabilidades de un pasajero adulto
  p <- c(0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)
  
  # Vector que retornaremos con la probabilidad de RNO según la probabilidad de pasajero adulto
  vectorProbaRendNoOptimo <-numeric(length(p))

  # Para cada probabilidad de un pasajero adulto (del vector p)...
  for (i in 1:length(p)){ 
    
    # Creamos una muestra del peso total de los pasajeros en 1000 viajes de avión
    muestra <- numeric(1000)
    
    # Determinamos para cada una de las 1000 simulaciones de la muestra la cantidad de pasajeros que habrá en cada vuelo según la distribución poisson(λ=70)
    numeroPasajeros <- rpois(1000, 70)
    
    # Cada dato en la muestra es la suma de los pesos de 81 pasajeros
    for (j in 1:1000){
      
      # Correjimos vector `numeroPasajeros` para evitar simulaciones con más de 81 pasajeros
      if (numeroPasajeros[j] > 81){
        numeroPasajeros[j] <- 81
      }
      
      # Asignamos a cada pasajero del j-ésimo vuelo el segmento al que pertenece (niño o adulto)
      pasajeros <- rbinom(n=numeroPasajeros[j], size=1, prob=p[i])
      
      # Contamos a la cantidad de adultos y niños
      adultos <- sum(pasajeros == 1)
      niños <- sum(pasajeros == 0)
      
      # A partir de la cantidad de adultos y niños estimamos el peso total según la distribución correspondiente al segmento
      pesoAdultos <- sum(rnorm(adultos, mean=70, sd=7))
      pesoNiños <- sum(rnorm(niños, mean=20, sd=5))
      
      # El peso total de los pasajeros se conforma por el peso total de ambos segmentos
      muestra[j] <- pesoAdultos + pesoNiños
    }
    
    # Contemos la cantidad de simulaciones en donde superan los 5500 kg
    cantRendNoOptimo <- sum(muestra > 5500)
    
    # Calculemos la probabilidad comparando estos casos contra la totalidad de simulaciones en la muestra
    probaRendNoOptimo <- cantRendNoOptimo/length(muestra)
    
    # Asignamos la probabilidad de RNO a la i-ésima probabilidad (p) de ser adulto
    vectorProbaRendNoOptimo[i] <- probaRendNoOptimo
  }
  
  return(vectorProbaRendNoOptimo)
}

r_es3_ej1 <- escenario3ej1()

# Escenario 3, ejercicio 2

# Vector con las diferentes probabilidades de un pasajero adulto
p <- c(0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)

# Ahora cada caso de probabilidad (p) debe ser evaluado para cada uno de los siguientes lambdas (λ) de la poisson de cantidad de pasajeros
lambdas <- c(70, 75, 80, 85, 90, 95, 100)

# Se los pasamos como parámetro para simplificar luego la realización del gráfico por fuera de la función
escenario3ej2 <- function(p, lambdas) {
  
  # Matriz que retornamos con la probabilidad RNO para cada combinación lambda/p
  matrizProbaRendNoOptimo <- matrix(nrow=length(p), ncol=length(lambdas), dimnames=list(p,lambdas))
  
  # Para cada lambda como distribución de cantidad de pasajeros...
  for (i in 1:length(lambdas)){
    
    # Para cada probabilidad de un pasajero adulto (del vector p)...
    for (j in 1:length(p)){
      
      # Creamos una muestra del peso total de los pasajeros en 1000 viajes de avión
      muestra <- numeric(1000)
      
      # Determinamos la cantidad de pasajeros para el i-ésimo lambda
      numeroPasajeros <- rpois(1000, lambdas[i])
      
      # Para cada probabilidad de un pasajero adulto (del vector p)...
      for (k in 1:1000){
        
        # Correjimos vector `numeroPasajeros` para evitar simulaciones con más de 81 pasajeros
        if (numeroPasajeros[k] > 81){ 
          numeroPasajeros[k] <- 81
        }
        
        # Asignamos a cada pasajero del j-ésimo vuelo el segmento al que pertenece (niño o adulto)
        pasajeros <- rbinom(n=numeroPasajeros[k], size=1, prob=p[j])
        
        # Contamos a la cantidad de adultos y niños
        adultos <- sum(pasajeros == 1)
        niños <- sum(pasajeros == 0)
        
        # A partir de la cantidad de adultos y niños estimamos el peso total según la distribución correspondiente al segmento
        pesoAdultos <- sum(rnorm(adultos, mean=70, sd=7))
        pesoNiños <- sum(rnorm(niños, mean=20, sd=5))
        
        # El peso total de los pasajeros se conforma por el peso total de ambos segmentos
        muestra[k] <- pesoAdultos + pesoNiños
      }
      
      # Contemos la cantidad de simulaciones en donde superan los 5500 kg
      cantRendNoOptimo <- sum(muestra > 5500)
      
      # Calculemos la probabilidad comparando estos casos contra la totalidad de simulaciones en la muestra
      probaRendNoOptimo <- cantRendNoOptimo/length(muestra)
      
      # Asignamos la probabilidad de RNO a la matriz según la combinación lambda/p en la que estemos
      matrizProbaRendNoOptimo[j,i] <- probaRendNoOptimo
    }
  }
  
  return(matrizProbaRendNoOptimo)
}

r_es3_ej2 <- escenario3ej2(p, lambdas)

# Graficamos la transpuesta de la matriz que retorna nuestra función (queda mejor para la idea del gráfico en términos de ejes)
matplot(t(r_es3_ej2), xaxt = "n", pch=15:22, col=c(2:7,7),  type="b", ylab = "Probabilidad de RNO (Rendimiento No Óptimo)", xlab= "Valor de Lambda (λ)", main="Probabilidad de RNO de diferentes P's en función de lambda (λ)")

# Cambiamos los valores del eje X para que muestren los lambdas
axis(1, at=1:length(lambdas), labels=lambdas)

# Para la comprensión del gráfico añadimos una leyenda que indica los P's asociados a cada una de las lineas con marcadores 
legend("topleft", inset=0.01, legend=p, pch=15:22, col=c(2:7,7), horiz=FALSE)
