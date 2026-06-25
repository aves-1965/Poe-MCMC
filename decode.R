# ============================================================================
# decode.R
# Descifrado de un cifrado por sustitucion con Metropolis-Hastings (MCMC).
# Basado en el Ejemplo 5.3 de R. P. Dobrow,
# "Introduction to Stochastic Processes with R", y en Diaconis (2009).
#
# Idea: se puntua cada funcion de decodificacion segun cuan "ingles" luce el
# texto resultante, usando frecuencias de bigramas (pares de simbolos
# consecutivos) de un corpus de referencia grande. MCMC busca la funcion de
# maximo puntaje recorriendo el espacio de permutaciones mediante
# transposiciones aleatorias.
# ============================================================================

set.seed(2024)

# ---------------------------------------------------------------------------
# 1. Alfabeto: a-z mas el espacio (27 simbolos)
# ---------------------------------------------------------------------------
alphabet <- c(letters, " ")
n_sym    <- length(alphabet)            # 27
idx      <- function(ch) match(ch, alphabet)

# Limpia texto: pasa a minusculas, deja solo letras y espacios,
# colapsa espacios multiples.
clean <- function(s) {
  s <- tolower(paste(s, collapse = " "))
  s <- gsub("[^a-z ]", " ", s)
  s <- gsub(" +", " ", s)
  trimws(s)
}

# ---------------------------------------------------------------------------
# 2. Texto de referencia
#    Dobrow uso las obras completas de Jane Austen (~4 millones de caracteres).
#    Cuanto mas grande y mas parecido al texto cifrado, mejor.
#    Opcion A: leer un archivo local "austen.txt".
#    Opcion B: descargar de Project Gutenberg (descomentar las lineas).
# ---------------------------------------------------------------------------

# --- Opcion B (descarga; requiere conexion) -------------------------------
url <- "https://www.gutenberg.org/files/1342/1342-0.txt"  # Pride & Prejudice
download.file(url, "austen.txt", quiet = TRUE)

ref_raw  <- readLines("austen.txt", warn = FALSE, encoding = "UTF-8")
ref_text <- clean(ref_raw)

# ---------------------------------------------------------------------------
# 3. Matriz de transiciones M (27 x 27)
#    M[i, j] = numero de veces que el simbolo j sigue al simbolo i en el corpus.
# ---------------------------------------------------------------------------
ref_codes <- idx(strsplit(ref_text, "")[[1]])

M <- matrix(0L, n_sym, n_sym, dimnames = list(alphabet, alphabet))
pairs <- cbind(ref_codes[-length(ref_codes)], ref_codes[-1])  # (i, i+1)
# Acumula conteos de bigramas de forma vectorizada
tab <- table(factor(pairs[, 1], levels = 1:n_sym),
             factor(pairs[, 2], levels = 1:n_sym))
M[] <- as.integer(tab)

# Suavizado de Laplace + escala logaritmica:
# evita log(0) para pares ausentes y previene desbordamiento numerico.
logM <- log(M + 1)

# ---------------------------------------------------------------------------
# 4. Mensaje cifrado (el del Ejemplo 5.3)
# ---------------------------------------------------------------------------
cipher <- paste0(
  "ahicainqcaqx ic zqcqwbl bwq zwqbj xjustlicz tlhamx ic jyq kbr ho jybj ",
  "albxx ho jyicmqwx kyh ybgq tqqc qnuabjqn jh mchk chjyicz ho jyq jyqhwr ",
  "ho dwhtbtilijiqx jybj jyqhwr jh kyiay jyq shxj zlhwihux htpqajx ho yusbc ",
  "wqxqbway bwq icnqtjqn ohw jyq shxj zlhwihux ho illuxjwbjihcx ",
  "qnzbw bllqc dhq jyq suwnqwx ic jyq wuq shwzuq")

cipher_codes <- idx(strsplit(clean(cipher), "")[[1]])

# ---------------------------------------------------------------------------
# 5. Funcion de puntaje (log-score)
#    f es una permutacion de 1:27: f[k] = simbolo original al que se decodifica
#    el simbolo cifrado k. El puntaje suma log M sobre pares consecutivos del
#    texto descifrado.
# ---------------------------------------------------------------------------
score <- function(f) {
  decoded <- f[cipher_codes]                 # aplica f a cada simbolo cifrado
  n <- length(decoded)
  sum(logM[cbind(decoded[-n], decoded[-1])])
}

decode_text <- function(f) {
  paste(alphabet[f[cipher_codes]], collapse = "")
}

# ---------------------------------------------------------------------------
# 6. Algoritmo de Metropolis-Hastings
# ---------------------------------------------------------------------------
n_iter <- 10000
f             <- 1:n_sym                      # funcion identidad inicial
current_score <- score(f)
best_f        <- f
best_score    <- current_score

for (i in 1:n_iter) {

  # Propuesta: intercambiar los valores asignados a dos simbolos al azar.
  swap   <- sample(n_sym, 2)
  f_star <- f
  f_star[swap] <- f[rev(swap)]
  prop_score   <- score(f_star)

  # Aceptacion: a = score(f*)/score(f). En logs: aceptar si
  # log(U) < logscore(f*) - logscore(f). (Propuesta simetrica => T se cancela.)
  if (log(runif(1)) < prop_score - current_score) {
    f             <- f_star
    current_score <- prop_score
    if (current_score > best_score) {
      best_score <- current_score
      best_f     <- f
    }
  }

  # Mostrar el progreso cada 100 iteraciones (como en el libro).
  if (i %% 100 == 0)
    cat(sprintf("[%5d] %s\n", i, decode_text(f)))
}

cat("\n--- Mejor descifrado encontrado ---\n")
cat(decode_text(best_f), "\n")
cat(sprintf("log-score = %.2f\n", best_score))
