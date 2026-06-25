# adjacentes.R
# Muestreo MCMC de secuencias binarias de longitud m SIN dos unos adyacentes
# ("secuencias buenas"). Estima el numero promedio de unos en una secuencia
# buena elegida con distribucion uniforme.
#
# init: secuencia inicial (vector de 0s y 1s; debe ser una secuencia buena)
# n   : numero de pasos de la cadena

adjacent <- function(init, n) {
  k   <- length(init)
  tot <- 0                     # acumulador del numero de unos a lo largo de la caminata
  new <- c(2, init, 2)         # amortiguadores (2) en cada punta: evitan salirse en los bordes

  for (i in 1:n) {
    index  <- 1 + sample(1:k, 1)   # coordenada al azar dentro de la zona real (2..k+1)
    newbit <- 0 + !new[index]      # valor propuesto al invertir ese bit

    if (newbit == 0) {
      new[index] <- 0              # quitar un 1: siempre valido
    } else if (new[index - 1] != 1 && new[index + 1] != 1) {
      new[index] <- 1              # poner un 1 sin vecinos en 1: valido
    }
    # caso restante (poner un 1 junto a otro 1): rechazo -> no se toca 'new'

    tot <- tot + sum(new)        # se cuenta el estado actual UNA sola vez por iteracion
  }

  tot / n - 4                    # cada sum(new) trae +4 de los amortiguadores (2 por punta);
                                 # al dividir por n queda "promedio real + 4", y el -4 lo limpia
}

set.seed(2026)                   # reproducibilidad
m    <- 100
init <- rep(0, m)                # arranca en la secuencia de todos ceros
adjacent(init, 100000)           # estimacion: deberia rondar 27.6
replicate(10, adjacent(init, 100000))
