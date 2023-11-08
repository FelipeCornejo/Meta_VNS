read_coords <- function(name){
  data <- read.table(name);
  return(data);
}

fitness <- function(sol){
  total <- 0;
  n <- length(sol);
  for(i in 1:(n-1)){
    # Se calculan la distancia entre el los puntos consecutivos
    dist_last_points <- sqrt((data[sol[i+1],"V1"] - data[sol[i],"V1"])^2 + (data[sol[i+1],"V2"] - data[sol[i],"V2"])^2);
    # Se añaden a la suma total de distancia
    total <- total + dist_last_points;
  }
  dist_last_points <- sqrt((data[sol[1],"V1"] - data[sol[n],"V1"])^2 + (data[sol[1],"V2"] - data[sol[n],"V2"])^2);
  total <- total + dist_last_points;
  return(total);
}

firstImprovment <- function(sol,fitness,calls,k,k_sums,max_iter){
  calls <- calls + 1 
  new_fit <- Inf
  i <- 1
  n <- length(sol)
  new_sol <- sol
  
  #SI K = 1 entonces se realiza 2-opt
  if (k == 3){
    while ((new_fit > fitness) && (i < n**2) && (calls < max_iter)){
      new_sol <- two_opt(sol)
      new_fit <- fitness(new_sol)
      calls <- calls + 1
      i <- i + 1
    }
    if (new_fit < fitness){
      k_sums[3] = k_sums[3] + 1
    }
  }
  #SI K = 2 entonces se realiza 3-opt
  if (k == 2){
    while ((new_fit > fitness) && (i < n**2) && (calls < max_iter)){
      new_sol <- three_opt(sol)
      new_fit <- fitness(new_sol)
      calls <- calls + 1
      i <- i + 1
    }
    if (new_fit < fitness){
      k_sums[2] = k_sums[2] + 1
    }
  }
  #SI K=3 entonces se realiza swap (cambia 4 caminos)
  if (k == 1){
    while ((new_fit > fitness) && (i < n**2) && (calls < max_iter)){
      new_sol <- swap(sol)
      new_fit <- fitness(new_sol)
      calls <- calls + 1
      i <- i + 1
    }
    if (new_fit < fitness){
      k_sums[1] = k_sums[1] +1 
    }
  }
  
  return(list(new_sol,new_fit,calls,k_sums))
}

neighborhoodChange <- function(sol,fitness,new_sol,new_fit,k){
  if (new_fit <= fitness){
    sol <- new_sol
    k <- 1
    fitness <- new_fit
  }
  else {
    k <- k + 1
  }
  return(list(sol,fitness,k))
}
