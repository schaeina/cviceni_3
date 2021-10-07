VratitMince <- function(M, mince){
  
  # Zbyva vratit
  zbytek <- M
  
  # Inicializace poctu minci
  mincePocet <- integer(length(mince))
  
  # Vracim od nejvetsich minci
  for (i in 1:length(mince)){
    mincePocet[i] <- floor(zbytek/mince[i])
    zbytek <- zbytek - mince[i] * mincePocet[i]
  }
  
  return(mincePocet)
}


mince <- c(50, 20, 10, 5, 2, 1)
M <- 73
vraceno <- VratitMince(M, mince)


