#SETEO DE FUNCIONES
seed <- 78999302
set.seed(seed)
source("essentials.r")
source("f_vecindario.r")

#SETEO DE INICIO DE META
data <- read_coords("./data/coords38.txt")
n <- nrow(data)
best_known <- 6656

#Limite de tiempo de busqueda
#time_limit <- 300
max_iter <- 100001

#SETEO DE ITERACIONES
iteracion <- 0

#SETEO DE LLENADOS DE LISTAS PARA GRAFICAR 21 ITERACIONES
bests_solutions_fit <- c()
bests_solutions_sol <- list()
gaps_bests <- c() #Para determinar luego de las 21 instancias, cual fue la mejor.
k_sums_1 <- c()
k_sums_2 <- c()

#21 iteraciones
while (iteracion < 21){
  print(iteracion)
  #INICIO DE VNS CON SU SOLUCIÓN INICIAL
  calls <- 0  
  sol <- sample(1:n)
  sol_fit <- fitness(sol)
  calls <- calls + 1 #Se tendrá en cuenta luego contar 100.001...
  k_sums <- c(0,0,0) #Para saber cual encuentra más mejoras.
  #(Hay que recordar que independiente de esto, posiblemente k1 se rellene más al inicio de VNS y k2)
  
  #SETEO DE LLENADOS DE LISTAS PARA GRAFICAR
  convergencia <- c(sol_fit)
  
  #stop <- Sys.time()
  while (calls < max_iter){
    k <- 1
    while (k < 4 && (calls < max_iter)){ #&& (calls < 100001) debería de estar solo arriba
      # pero así para el ciclo luego de que encuentra 100001 dedntro de firstImprovment sino podría pasarse.
      #SHAKE
      shaked_sol <- shake(sol,convergencia)
      shaked_fit <- fitness(shaked_sol)
      calls <- calls + 1
      
      #Entre la perturbación y la búsqueda del primer incremento se pueden perder
      #mejoras más grandes.
      
      #FIRST_IMPROVMENT
      retorno_improvment <- firstImprovment(shaked_sol,shaked_fit,calls,k,k_sums,max_iter)
      new_sol <- unlist(retorno_improvment[1])
      new_fit <- unlist(retorno_improvment[2])
      calls <- unlist(retorno_improvment[3])
      k_sums <- unlist(retorno_improvment[4])
      
      #NEIGHBORHOODCHANGE
      retorno_change <- neighborhoodChange(sol,sol_fit,new_sol,new_fit,k)
      sol <- unlist(retorno_change[1])
      sol_fit <- unlist(retorno_change[2])
      k <- unlist(retorno_change[3])
      
      #PARA GRAFICAR Y OBTENER GAP
      convergencia <- c(convergencia, sol_fit)
    }
    
  }
  #end <- as.numeric(difftime(Sys.time(), stop, units = "secs"))
  
  # Crear un gráfico de convergencia
  plot(convergencia, type = "o", pch = 16, col = "blue", xlab = "Mejora", ylab = "Fitness", main = "Gráfico de Convergencia"
       , ylim=c((best_known - 500),max(convergencia)))
  abline(h = best_known, col = "red")
  text(0.5, best_known + 0.2, "Límite", col = "red", adj = 0)
  # Agregar líneas conectando los puntos
  lines(convergencia, type = "o", pch = 16, col = "blue")
  
  
  gap_bests <- (-(best_known - sol_fit)/best_known) * 100 #-2.1257%, es decir 2.1257% peor.
  gap_medians <- ((best_known - median(convergencia))/best_known) * 100 #-2.1257%, es decir 2.1257% peor.
  gap_means <- ((best_known - mean(convergencia))/best_known) * 100 #-12.6411%, es decir 12.6411% peor.

  bests_solutions_fit <- c(bests_solutions_fit,sol_fit)
  bests_solutions_sol <- c(bests_solutions_sol,list(sol))
  gaps_bests <- c(gaps_bests,gap_bests)
  
  k_sums_1 <- c(k_sums_1, k_sums[1])
  k_sums_2 <- c(k_sums_2, k_sums[2])
  
  iteracion <- iteracion + 1
}

box38_fit <- boxplot(bests_solutions_fit)
box38_gap <- boxplot(gaps_bests,range=100)

k_sums_norm <- (k_sums_1 - k_sums_2)/k_sums_1
#box29_sums <- boxplot(k_sums_norm)


bests38 <- bests_solutions_fit