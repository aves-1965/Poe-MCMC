# ==============================================================================
# TP MODELADO Y SIMULACIÓN - EJERCICIO 5.2 (EL ROBOT EN EL PASILLO)
# ==============================================================================

# --- PARTE 1: SIMULACIÓN ESTOCÁSTICA (Monte Carlo sobre Cadena de Markov) ------
k <- 10       # Longitud del pasillo (estados del 0 al k)
pasos <- 100000
coordenadas <- numeric(pasos)

# Estado inicial: El robot arranca en el medio del pasillo (metro 5)
coordenadas[1] <- 5 

balance <- 0
historial_financiero <- numeric(pasos)

set.seed(42)  # Semilla para que el azar sea reproducible

for(t in 2:pasos) {
  actual <- coordenadas[t-1]
  
  # LÓGICA DE MOVIMIENTO DE DOBROW (Matriz de Transición)
  if (actual == 0) {
    coordenadas[t] <- 1      # Si chocó la pared 0, el empujón lo manda al 1
  } else if (actual == k) {
    coordenadas[t] <- k - 1  # Si chocó la pared k, el empujón lo manda al k-1
  } else {
    # Si está en el medio, tira la moneda de 2 caras (Cara = +1, Ceca = -1)
    moneda <- sample(c(-1, 1), 1)
    coordenadas[t] <- actual + moneda
  }
  
  # SISTEMA CONTABLE (Costos y Recompensas)
  if (coordenadas[t] == 0 || coordenadas[t] == k) {
    balance <- balance + k   # Chocó la pared: activa sensor y recarga (+ $k)
  } else {
    balance <- balance - 1   # Pasillo interno: gasto de batería por avanzar (- $1)
  }
  historial_financiero[t] <- balance
}

ganancia_promedio_simulada <- balance / pasos
print(paste("Ganancia promedio por paso (Simulada): $", round(ganancia_promedio_simulada, 4)))


# --- PARTE 2: VALIDACIÓN TEÓRICA MACRO (Distribución Estacionaria) ------------
# La teoría matemática de Dobrow demuestra que en una caminata reflectante,
# la probabilidad a largo plazo de estar en los bordes es 1/(2k) y en el medio es 1/k.

pi_bordes <- 1 / (2 * k)
pi_internos <- 1 / k

# Esperanza Matemática de la ganancia por paso:
ganancia_teorica_por_paso <- (2 * pi_bordes * k) + ((k - 1) * pi_internos * (-1))
print(paste("Ganancia promedio por paso (Teórica): $", round(ganancia_teorica_por_paso, 4)))


# --- PARTE 3: GRÁFICOS PARA EL INFORME ----------------------------------------
# Configuramos la pantalla para ver dos gráficos juntos
par(mfrow = c(2, 1))

# Gráfico 1: El vaivén del robot (Primeros 150 pasos)
plot(coordenadas[1:150], type = "b", col = "darkblue", pch = 16, lty = 3,
     main = "Trayectoria Errática del Robot (Primeros 150 pasos)",
     xlab = "Tiempo (Pasos)", ylab = "Posición en el Pasillo (Metros)",
     yaxt = "n")
axis(2, at = 0:k, labels = 0:k)
abline(h = c(0, k), col = "red", lty = 2, lwd = 2) # Paredes físicas

# Gráfico 2: El orden financiero emergente (Los 10.000 pasos completos)
plot(historial_financiero, type = "l", col = "darkgreen", lwd = 2,
     main = "Evolución del Balance de Energía / Dinero Acumulado",
     xlab = "Tiempo (Pasos)", ylab = "Capital Acumulado ($)")
