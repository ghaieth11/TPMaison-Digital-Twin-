
##################################################################################

borehole <- function(x)
{
  ##########################################################################
  #
  # BOREHOLE FUNCTION
  #
  ##########################################################################
  #
  # OUTPUT AND INPUT:
  #
  # y  = water flow rate
  # x = c(rw, riw, r, Tu, Hu, Tum, Hum, Tlm, Hlm, Tl, Hl, L, Kw)
  #
  ##########################################################################
  
  xx <- matrix(x, ncol=13)
  
  rw  <- xx[,1]
  riw <- xx[,2]
  r   <- xx[,3]
  Tu  <- xx[,4]
  Hu  <- xx[,5]
  Tum <- xx[,6]
  Hum <- xx[,7]
  Tlm <- xx[,8]
  Hlm <- xx[,9]
  Tl  <- xx[,10]
  Hl  <- xx[,11]
  L   <- xx[,12]
  Kw  <- xx[,13]
  
  frac1 <- 2 * pi * Tu * (Hu-Hl)
  frac11 <- (Tum - Tlm) * (Hum - Hlm)
  frac2 <- frac1 / frac11
  
  frac2a <- 2*L*Tu / (log(r/rw)*rw^2*Kw)
  frac2b <- Tu / Tl
  frac2 <- log(r/rw) * (1+frac2a+frac2b)
  
  y <- frac1 / frac2
  return(y)
}

##################################################################################

EchantBorehole <- function(N){
  
  # Description de la fonction :
  # Cette fonction genere un échantillon Monte Carlo de taille N,
  # pour le modele borhole
  #
  # Entrées de la fonction :
  # - taille = taille de l'échantillon
  #
  # Description de la sortie de la fonctio : 
  # - X = matrice de taille N x 13 qui contient des valeurs des 13 entrées
  # (statistiquement independantes) du modele borehole
  
  X = matrix(NA, N, 13)
  
  X[,1] <- rnorm(N, 0.1, 0.015)
  X[,2] <- rnorm(N, 0.05, 0.01)
  X[,3] <- rlnorm(N, 7.71, 1.0056)
  X[,4] <- runif(N, 63100, 116000)
  X[,5] <- runif(N, 1000, 1100)
  X[,6] <- runif(N, 6310, 11600)
  X[,7] <- runif(N, 900, 1000)
  X[,8] <- runif(N, 631, 1160)
  X[,9] <- runif(N, 800, 900)
  X[,10] <- runif(N, 63.1, 116)
  X[,11] <- runif(N, 700, 800)
  X[,12] <- runif(N, 1120, 1680)
  X[,13] <- runif(N, 3000, 12000)
  
  colnames(X) <- c("rw","riw","r","Tu","Hu","Tum","Hum","Tlm","Hlm","Tl","Hl","L","Kw") # noms de variables aleatoires
  
  return(X)
  
}#end function


d <- 13 # nombre de variables incertaines
names <- c("rw","riw","r","Tu","Hu","Tum","Hum","Tlm","Hlm","Tl","Hl","L","Kw") # noms de variables aleatoires
