VratitMince <- function(M, mince){
  
  # Zbyva vratit
  zbytek <- M
  
  # Inicializace poctu minci
  mincePocet <- integer(length(mince))
  
  # Vypis kolik bylo vraceno
  zprava <- c('Na ', as.character(M), ' korun bylo vraceno: ')
  
  # Vracim od nejvetsich minci
  for (i in 1:length(mince)){
    mincePocet[i] <- floor(zbytek/mince[i])
    zbytek <- zbytek - mince[i] * mincePocet[i]
    zprava <- c(zprava, as.character(mincePocet[i]), 'x ', as.character(mince[i]), ', ')
  }
  
  print(paste(unlist(zprava), collapse=''))
  return(mincePocet)
}


mince <- c(50, 20, 10, 5, 2, 1)
M <- 73
vraceno <- VratitMince(M, mince)



Cokolada <- function(M, r, s){
  if (r == nrow(M)){
    return(M[r,s])
  }
  else {
    C <- M[r,s]
    Cdolu <- Cokolada(M,r+1,s)
    Csikmo <- Cokolada(M,r+1,s+1)
    return(max(Cdolu,Csikmo) + C)
  }
  
}

M <- matrix(c(3,0,0,0,1,4,0,0,5,3,0,0,1,2,6,7), nrow=4, ncol=4, byrow=TRUE)
pocetDilku <- Cokolada(M,1,1)


CokoladaIter <- function(M){
  s = dim(M)
  k1 <- seq(from=s[1]-1, to=1, by=-1)
  
  for (r in k1){
    k2 <- seq(from=r, to=1, by=-1)
    for (s in k2){
      Cdolu <- M[r+1,s] + M[r,s]
      Csikmo <- M[r+1,s+1] + M[r,s]
      M[r,s] <- max(c(Cdolu,Csikmo))
      print(M)
    }
  }
  
  return(M[1,1])
}

pocetDilku <- CokoladaIter(M)
