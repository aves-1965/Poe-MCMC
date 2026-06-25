# ============================================================================
# decode_simple.R
# Descifrar un mensaje (cifrado por sustitucion) usando el metodo de
# Metropolis-Hastings. 
#
# IDEA GENERAL:
#   - Un "cifrado por sustitucion" reemplaza cada letra por otra fija.
#   - Para descifrarlo buscamos la correspondencia (que letra es cada una)
#     que produzca un texto que "suene a ingles".
#   - Medimos cuanto suena a ingles contando que tan frecuentes son los
#     pares de letras consecutivas en un libro de referencia.
#   - Probamos correspondencias al azar, mejorandolas de a poco, y repetimos
#     el proceso muchas veces para no quedar atrapados en una solucion mala.
# ============================================================================

set.seed(1)   # hace que cada corrida de resultados identicos (reproducible)

# ---------------------------------------------------------------------------
# 1) El alfabeto: 26 letras + el espacio = 27 simbolos
# ---------------------------------------------------------------------------
alfabeto    <- c(letters, " ")     # letters ya trae las 26 letras minusculas
n_simbolos  <- length(alfabeto)    # 27

# Convierte cada caracter en su numero de posicion dentro del alfabeto
# (por ejemplo: "a" -> 1, "b" -> 2, ..., " " -> 27)
a_indices <- function(caracteres) {
  match(caracteres, alfabeto)
}

# Deja un texto "limpio": todo en minusculas y solo con letras y espacios
limpiar <- function(texto) {
  texto <- tolower(paste(texto, collapse = " "))  # junta y pasa a minusculas
  texto <- gsub("[^a-z ]", " ", texto)            # borra todo lo que no sea letra o espacio
  texto <- gsub(" +", " ", texto)                 # varios espacios -> uno solo
  trimws(texto)                                   # saca espacios de los bordes
}

# ---------------------------------------------------------------------------
# 2) Texto de referencia (para aprender el "ingles tipico")
#    Usamos "Orgullo y prejuicio" de Jane Austen, desde Project Gutenberg.
#    Se descarga una sola vez y se guarda en corpus.txt.
# ---------------------------------------------------------------------------
if (!file.exists("corpus.txt")) {
  url <- "https://www.gutenberg.org/cache/epub/1342/pg1342.txt"
  download.file(url, "corpus.txt", quiet = TRUE)
}

texto_ref   <- limpiar(readLines("corpus.txt", warn = FALSE))
codigos_ref <- a_indices(strsplit(texto_ref, "")[[1]])  # texto -> vector de numeros 1..27

cat("Caracteres en el texto de referencia:", length(codigos_ref), "\n")

# ---------------------------------------------------------------------------
# 3) Contar pares de letras consecutivas
#    M[i, j] = cuantas veces la letra j aparece justo despues de la letra i.
#    Recorremos el texto de referencia de a un par por vez.
# ---------------------------------------------------------------------------
M <- matrix(0, n_simbolos, n_simbolos)   # arranca como una tabla 27x27 de ceros

for (k in 1:(length(codigos_ref) - 1)) {
  desde <- codigos_ref[k]       # letra actual
  hacia <- codigos_ref[k + 1]   # letra siguiente
  M[desde, hacia] <- M[desde, hacia] + 1
}

# Pasamos a logaritmos (sumar es mas estable que multiplicar) y sumamos 1
# para que ningun par tenga conteo 0 (log(0) daria problemas).
logM <- log(M + 1)

# ---------------------------------------------------------------------------
# 4) El mensaje cifrado que queremos descifrar
# ---------------------------------------------------------------------------
mensaje_cifrado <- paste0(
  "ahicainqcaqx ic zqcqwbl bwq zwqbj xjustlicz tlhamx ic jyq kbr ho jybj ",
  "albxx ho jyicmqwx kyh ybgq tqqc qnuabjqn jh mchk chjyicz ho jyq jyqhwr ",
  "ho dwhtbtilijiqx jybj jyqhwr jh kyiay jyq shxj zlhwihux htpqajx ho yusbc ",
  "wqxqbway bwq icnqtjqn ohw jyq shxj zlhwihux ho illuxjwbjihcx ",
  "qnzbw bllqc dhq jyq suwnqwx ic jyq wuq shwzuq")

codigos_cifrado <- a_indices(strsplit(limpiar(mensaje_cifrado), "")[[1]])

# ---------------------------------------------------------------------------
# 5) Puntaje de una correspondencia
#    'f' es una correspondencia: f[k] dice a que letra se traduce el simbolo k.
#    El puntaje suma, para cada par de letras consecutivas del texto
#    descifrado, que tan comun es ese par en el texto de referencia.
#    Cuanto mas alto el puntaje, mas "ingles" luce el resultado.
# ---------------------------------------------------------------------------
puntaje <- function(f) {
  descifrado <- f[codigos_cifrado]      # traduce cada simbolo del mensaje
  n <- length(descifrado)
  desde <- descifrado[1:(n - 1)]        # cada letra...
  hacia <- descifrado[2:n]              # ...y la que le sigue
  # logM[cbind(desde, hacia)] toma, para cada par (desde, hacia), el valor
  # correspondiente de la tabla logM. Luego sumamos todos esos valores.
  sum(logM[cbind(desde, hacia)])
}

# Traduce el mensaje a texto legible usando la correspondencia f
texto_descifrado <- function(f) {
  paste(alfabeto[f[codigos_cifrado]], collapse = "")
}

# ---------------------------------------------------------------------------
# 6) Una "cadena": empieza al azar y mejora la correspondencia de a poco
# ---------------------------------------------------------------------------
una_cadena <- function(n_pasos) {

  f  <- sample(n_simbolos)   # correspondencia inicial al azar (mezcla de 1..27)
  pf <- puntaje(f)           # su puntaje

  mejor_f  <- f              # guardamos la mejor correspondencia vista
  mejor_pf <- pf

  for (i in 1:n_pasos) {

    # --- Propuesta: intercambiar dos letras al azar ---
    candidato <- f
    dos <- sample(n_simbolos, 2)    # elige 2 posiciones distintas
    a <- dos[1]
    b <- dos[2]
    temporal     <- candidato[a]    # intercambio con una variable auxiliar
    candidato[a] <- candidato[b]
    candidato[b] <- temporal

    pc <- puntaje(candidato)        # puntaje del candidato

    # --- Decision: aceptar o no el candidato ---
    if (pc > pf) {
      aceptar <- TRUE               # si mejora, lo aceptamos siempre
    } else {
      # si empeora, lo aceptamos con probabilidad exp(pc - pf), que es un
      # numero entre 0 y 1. Esto evita quedar atrapado en soluciones malas.
      aceptar <- runif(1) < exp(pc - pf)
    }

    if (aceptar) {
      f  <- candidato
      pf <- pc
      if (pf > mejor_pf) {          # llevamos registro del mejor de la cadena
        mejor_pf <- pf
        mejor_f  <- f
      }
    }
  }

  list(f = mejor_f, puntaje = mejor_pf)
}

# ---------------------------------------------------------------------------
# 7) Muchos reinicios: corremos varias cadenas y nos quedamos con la mejor
#    Cada cadena arranca de una correspondencia distinta, asi que si una
#    queda atrapada, otra puede encontrar la solucion correcta.
# ---------------------------------------------------------------------------
n_reinicios <- 120      # cuantas cadenas correr
n_pasos     <- 6000     # pasos por cadena

mejor_global_f  <- NULL
mejor_global_pf <- -Inf

for (r in 1:n_reinicios) {
  resultado <- una_cadena(n_pasos)

  # si esta cadena supero el mejor puntaje hasta ahora, lo guardamos y avisamos
  if (resultado$puntaje > mejor_global_pf) {
    mejor_global_pf <- resultado$puntaje
    mejor_global_f  <- resultado$f
    cat("Reinicio", r, "- nuevo mejor texto:\n")
    cat("  ", texto_descifrado(mejor_global_f), "\n\n")
  }
}

cat("==============================\n")
cat("Mejor descifrado encontrado:\n")
cat(texto_descifrado(mejor_global_f), "\n")

