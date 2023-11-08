swap <- function(sol){
  n <- length(sol)
  pos <- sample(1:(n-1),2) #De este modo no se repiten las posiciones.
  temp <- sol[pos[1]]
  sol[pos[1]] <- sol[pos[2]]
  sol[pos[2]] <- temp
  
  return (sol)
}

shake <- function(sol,convergencia){
  #Se realiza una perturbación a la solución.
  #Esta perturbación cambiará un elemento aleatorio por otro aleatorio (swap);
  #Así mantendrá la mayoría de las instancias y modificará 4 direcciones.
  sol <- swap(sol)
  
  #Por otro lado, si la convergencia se queda estancada se debe de perturbar
  #más la solución para que así pueda escapar de mínimos locales.
  
  if (length(convergencia) > 300){
    if (var(convergencia[(length(convergencia) - 299):length(convergencia)]) == 0){
      #sol <- sample(1:n)
    }
  }
  
  return(sol)
}

two_opt <- function(sol){
  #Se debe tomar un segmento dentro de la solución e invertirlo.
  #  A → B → E → D → C → F → G → H 
  #  A → B →(C → D → E)→ F → G → H
  n <- length(sol)
  pos <- sort(sample(1:n,2)) #De este modo no se repiten las posiciones.
  sol[c(pos[1]:pos[2])] <- rev(sol[pos[1]:pos[2]])
  return(sol)
}

three_opt <- function(sol){
  # Se debe tomar 2 segmentos dentro de la solución intercambiarlos de lugar e invertir uno de los segmentos.
  #  A → B → F → G → H → E → D → C 
  #  A → B → (F → G → H) → (E → D → C) /Seleccionar
  #  A → B → (E → D → C) → (F → G → H)  /Intercambiar
  #  A → B →(C → D → E)→ F → G → H /Invertir
  # Cabe considerar que en ocasiones esto significa realizar varios pasos de 2-opt en solo 1
  
  n <- length(sol)
  pos <- sort(sample(1:(n-1),2))
  
  track1 <- sol[1:pos[1]]
  track2 <- sol[(pos[1]+1) : pos[2]]
  track3 <- sol[(pos[2]+1) : n]
  new_sol <- list(track1,track2,track3) # /Seleccionar
  
  
  intercambio <- sample(1:3,2)
  temp <- new_sol[intercambio[1]]
  new_sol[intercambio[1]] <- new_sol[intercambio[2]]
  new_sol[intercambio[2]] <- temp # /Intercambiar
  
  invertir <- sample(1:3,1)
  new_sol[invertir] <- list(rev(unlist(new_sol[invertir]))) # /Invertir
  
  return(unlist(new_sol))
}
