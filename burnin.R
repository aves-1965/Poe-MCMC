# burnin.R
# Diagnostico de burn-in para el muestreador de "secuencias buenas".
# Registra el numero de unos del estado en CADA paso y lo grafica de tres formas.

adjacent_trace <- function(init, n) {
  k   <- length(init)
  new <- c(2, init, 2)
  ones <- numeric(n)                  # numero de unos reales en cada paso
  for (i in 1:n) {
    index  <- 1 + sample(1:k, 1)
    newbit <- 0 + !new[index]
    if (newbit == 0) {
      new[index] <- 0
    } else if (new[index - 1] != 1 && new[index + 1] != 1) {
      new[index] <- 1
    }
    ones[i] <- sum(new) - 4           # unos reales (sin los amortiguadores)
  }
  ones
}

set.seed(2024)
m    <- 100
ones <- adjacent_trace(rep(0, m), 10000)
mu   <- mean(ones)

op <- par(mfrow = c(1, 3))

# (a) Trace completo: a escala 1:100000 el transitorio casi no se nota
sel <- seq(1, length(ones), by = 50)          # adelgazado para que dibuje rapido
plot(sel, ones[sel], type = "l", col = "grey55",
     xlab = "iteracion", ylab = "n de unos", main = "Trace completo")
abline(h = mu, col = "firebrick", lwd = 2)

# (b) Zoom al arranque: AQUI se ve el burn-in (sube desde 0 hasta la banda ~27.6)
plot(ones[1:2000], type = "l", col = "grey30",
     xlab = "iteracion", ylab = "n de unos", main = "Zoom: primeras 2000 iter.")
abline(h = mu, col = "firebrick", lwd = 2)

# (c) Media acumulada: como se estabiliza la estimacion
run <- cumsum(ones) / seq_along(ones)
plot(run, type = "l", col = "steelblue",
     xlab = "iteracion", ylab = "estimacion acumulada", main = "Media acumulada")
abline(h = mu, col = "firebrick", lwd = 2, lty = 2)

par(op)

# --- Histograma del numero de unos (descartando el arranque) ---
burn <- 1000                       # descarta las primeras 1000 iteraciones (burn-in)
post <- ones[-(1:burn)]

hist(post, breaks = seq(min(post) - 0.5, max(post) + 0.5, by = 1),
     col = "grey80", border = "white",
     xlab = "numero de unos", ylab = "frecuencia",
     main = "Distribucion del numero de unos (post burn-in)",
     xlim = c(0, 50))
abline(v = mean(post), col = "firebrick",  lwd = 2)            # media ~27.6
abline(v = 50,         col = "steelblue", lwd = 2, lty = 2)    # maximo teorico (1010...10)
legend("topright",
       legend = c("media", "maximo posible (50)"),
       col = c("firebrick", "steelblue"), lwd = 2, lty = c(1, 2), bty = "n")
