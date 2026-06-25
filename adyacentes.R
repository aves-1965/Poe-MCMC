# ============================================
#       ALGORITMO DE PASEO ALEATORIO
#               adjacentes.R
# ============================================
# OBJETIVO: usando MCMC estimar el número promedio de unos en una secuencia 
# buena elegida al azar (con distribución uniforme sobre todas las secuencias 
# buenas). 
# ============================================
# init: initial sequence
# n: number of steps to run the chain

adjacent <- function(init, n)
{ k <- length(init)     # k = 100, longitud de la secuencia. En cada extremo 
# se le agrega un limitador (p.ej el número 2, distino a 0 y 1) para 
# identificar las puntas cuando deba decidir si cambia o no un bit de las puntas
  tot <- 0              # acumulador del total de unos a lo largo del paseo
  new <-c(2, init, 2)   # secuencia con relleno (un 2 a cada extremo)
  for (i in 1:n) {
    index <- 1 + sample(1:k,1) # elige una coordenada al azar (2..k+1)
    newbit <- 0 + !new[index]  # propone invertir ese bit
    if (newbit==0) {           # Proponemos ¿cambiar a cero?
      new[index] <- 0          # Quitar un 1 nunca puede crear dos unos adyacentes
      tot <- tot + sum(new)    #actualizamos el acumulador
      next} 
    else {                     # Proponemos ¿cambiar a uno?
      if (new[index-1]==1 | new[index+1] ==1) { # ¿Los vecinos son 1? Se rechaza el cambio
        tot <-tot + sum(new)   #actualizamos el acumulador
   # En MCMC, cuando la cadena se queda quieta, ese estado igual cuenta
    # en el promedio temporal.
     next}
     else {
        new[index] <- 1}    # ¿Los vecinos no son 1? Se acepta el cambio
        tot <- tot + sum(new)}
     }
  tot/n- 4 } # 

m <- 100
init <- rep(0,m) # Start at sequence of all 0s
adjacent(init,100000)
  