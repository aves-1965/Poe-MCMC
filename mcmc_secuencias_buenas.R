# ============================================================
#  MCMC 5.1 - Caminata aleatoria sobre secuencias binarias
#  "buenas" (sin unos adyacentes)
#
#  Objetivo: estimar el numero esperado de 1s en una secuencia
#  binaria de longitud m elegida uniformemente entre todas las
#  secuencias sin dos 1s adyacentes.
#
#  Soporte para la presentacion - Capitulo 5, seccion 5.1
#  (basado en Dobrow, Introduction to Stochastic Processes with R)
# ============================================================

# ---- 1. La cadena MCMC -------------------------------------
# Caminata aleatoria sobre el grafo de secuencias buenas:
#   - se elige una posicion al azar
#   - si es 1 -> se cambia a 0 (siempre valido)
#   - si es 0 -> se cambia a 1 solo si no crea 1's adyacentes;
#                en caso contrario, la cadena se queda quieta.
# La distribucion limite es uniforme sobre las secuencias buenas.

mcmc_unos <- function(m, n_pasos, init = NULL) {
  # estado inicial: por defecto, todos ceros (siempre es "bueno")
  x <- if (is.null(init)) rep(0L, m) else init
  total_unos <- 0                      # acumulador para el promedio
  
  for (paso in seq_len(n_pasos)) {
    i <- sample.int(m, 1)              # posicion elegida al azar
    
    if (x[i] == 1L) {
      x[i] <- 0L                       # quitar un 1 nunca falla
    } else {
      # vecinos (tratando los bordes como 0)
      izq <- if (i > 1) x[i - 1] else 0L
      der <- if (i < m) x[i + 1] else 0L
      if (izq == 0L && der == 0L) x[i] <- 1L   # solo si sigue siendo buena
      # si no, la cadena permanece en su estado actual
    }
    
    total_unos <- total_unos + sum(x) # contamos 1s en cada paso
  }
  
  total_unos / n_pasos                 # estimacion de la esperanza
}

# ---- 2. Valor exacto (para comparar) -----------------------
# El numero de secuencias buenas de longitud m es F(m+2), el
# (m+2)-esimo numero de Fibonacci. La esperanza exacta del
# numero de 1s se puede obtener contando, por posicion, en
# cuantas secuencias buenas ese bit vale 1.
esperanza_exacta <- function(m) {
  fib <- numeric(m + 3)
  fib[1] <- 1; fib[2] <- 1
  for (k in 3:(m + 3)) fib[k] <- fib[k - 1] + fib[k - 2]
  good_len <- function(L) if (L <= 0) 1 else fib[L + 2]   # # buenas de long. L
  total_good <- good_len(m)
  # bit i = 1  =>  los dos lados deben ser 0; partes izq/der independientes y buenas
  unos <- 0
  for (i in 1:m) {
    izq <- good_len(i - 2)             # posiciones 1..i-2 libres y buenas
    der <- good_len(m - i - 1)         # posiciones i+2..m libres y buenas
    unos <- unos + izq * der
  }
  unos / total_good
}

# ---- 3. Ejecucion -----------------------------------------
set.seed(2016)
m       <- 100
n_pasos <- 100000

est <- mcmc_unos(m, n_pasos)
exa <- esperanza_exacta(m)

cat(sprintf("Longitud de la secuencia (m): %d\n", m))
cat(sprintf("Pasos de la cadena:           %d\n", n_pasos))
cat(sprintf("Estimacion MCMC:              %.4f\n", est))
cat(sprintf("Valor exacto:                 %.4f\n", exa))
cat(sprintf("Error absoluto:               %.4f\n", abs(est - exa)))

# ---- 4. (Opcional) Convergencia visual ---------------------
# Descomentar para ver como el promedio se estabiliza:
#
traza <- numeric(n_pasos); x <- rep(0L, m); tot <- 0
for (paso in seq_len(n_pasos)) {
   i <- sample.int(m, 1)
   if (x[i] == 1L) x[i] <- 0L else {
     izq <- if (i > 1) x[i-1] else 0L; der <- if (i < m) x[i+1] else 0L
     if (izq == 0L && der == 0L) x[i] <- 1L
   }
   tot <- tot + sum(x); traza[paso] <- tot / paso
}
plot(traza, type = "l", col = "#1C7293", lwd = 2,
      xlab = "Paso", ylab = "Estimacion del numero de 1s",
      main = "Convergencia del estimador MCMC")
abline(h = exa, col = "#F4A259", lty = 2, lwd = 2)
legend("topright", c("MCMC", "Exacto"),
        col = c("#1C7293", "#F4A259"), lty = c(1, 2), lwd = 2)
