# ============================================================================
# decode_simple.R   (version con explicacion ampliada)
# Descifrar un mensaje cifrado por sustitucion usando Metropolis-Hastings.
#
# QUE ES UN CIFRADO POR SUSTITUCION
#   Cada letra del texto original se reemplaza SIEMPRE por la misma letra.
#   Ejemplo: si "a"->"q" y "t"->"j", entonces "tata" se cifra como "jqjq".
#   Descifrar = encontrar la correspondencia inversa (que "j" es "t", etc.).
#
# COMO LO RESUELVE ESTE PROGRAMA (mismo orden que los dos esquemas)
#   PREPARACION (una sola vez):
#     1. Limpiar un libro de referencia y convertirlo a numeros.
#     2. Contar pares de letras consecutivas -> tabla logM.
#     3. Convertir el mensaje cifrado a numeros.
#   BUSQUEDA (se repite muchas veces):
#     4. Arrancar con una correspondencia al azar.
#     5. Proponer un pequeno cambio (intercambiar dos letras).
#     6. Puntuarlo (que tan "ingles" luce el resultado, usando logM).
#     7. Aceptarlo o rechazarlo; repetir; quedarse con el mejor de todos.
# ============================================================================

set.seed(1)   # fija el azar -> cada corrida da el mismo resultado (reproducible)

# ---------------------------------------------------------------------------
# 1) El alfabeto y dos funciones auxiliares
# ---------------------------------------------------------------------------
# Trabajamos con 27 simbolos: las 26 letras minusculas + el espacio.
alfabeto    <- c(letters, " ")     # 'letters' ya trae "a","b",...,"z"
n_simbolos  <- length(alfabeto)    # 27

# a_indices: convierte caracteres en su POSICION dentro del alfabeto.
#   "a" -> 1, "b" -> 2, ..., "z" -> 26, " " -> 27
# Trabajar con numeros (en vez de letras) permite usarlos como coordenadas
# de una tabla mas adelante. 'match' busca cada caracter en 'alfabeto' y
# devuelve su posicion; es vectorizada, asi que procesa todo el vector junto.
a_indices <- function(caracteres) {
  match(caracteres, alfabeto)
}

# limpiar: normaliza un texto para que solo tenga letras minusculas y espacios.
#   Asi el alfabeto de 27 simbolos alcanza para representarlo todo.
limpiar <- function(texto) {
  texto <- tolower(paste(texto, collapse = " "))  # une las lineas y pasa a minusculas
  texto <- gsub("[^a-z ]", " ", texto)            # cambia por espacio todo lo que NO sea letra/espacio
  texto <- gsub(" +", " ", texto)                 # colapsa varios espacios en uno
  trimws(texto)                                   # recorta espacios sobrantes en los bordes
}

# ---------------------------------------------------------------------------
# 2) Texto de referencia: aprender como es el "ingles tipico"
#    Usamos "Pride and Prejudice" de Jane Austen (Project Gutenberg).
#    Se descarga UNA sola vez y se guarda en corpus.txt; en las corridas
#    siguientes se lee del disco (mas rapido).
# ---------------------------------------------------------------------------
if (!file.exists("corpus.txt")) {
  url <- "https://www.gutenberg.org/cache/epub/1342/pg1342.txt"
  download.file(url, "corpus.txt", quiet = TRUE)
}

# Leemos el archivo, lo limpiamos y lo convertimos en un vector larguisimo de
# numeros (uno por cada caracter del libro).
texto_ref   <- limpiar(readLines("corpus.txt", warn = FALSE))
codigos_ref <- a_indices(strsplit(texto_ref, "")[[1]])
# strsplit(texto_ref, "")  -> parte el texto en caracteres sueltos
# [[1]]                    -> saca ese vector de adentro de la lista que devuelve strsplit
# a_indices(...)           -> cambia cada caracter por su numero 1..27

cat("Caracteres en el texto de referencia:", length(codigos_ref), "\n")

# ---------------------------------------------------------------------------
# 3) La tabla de pares de letras (el corazon del metodo)
#    M[i, j] = cuantas veces la letra j aparecio JUSTO DESPUES de la letra i
#    en el texto de referencia. Por ejemplo, M["t","h"] sera enorme (en ingles
#    "th" es comun) y M["q","z"] sera casi cero.
# ---------------------------------------------------------------------------
M <- matrix(0, n_simbolos, n_simbolos)   # tabla 27x27 llena de ceros

# Recorremos el texto de a un par por vez y sumamos 1 en la casilla del par.
for (k in 1:(length(codigos_ref) - 1)) {
  desde <- codigos_ref[k]       # letra en la posicion k
  hacia <- codigos_ref[k + 1]   # letra en la posicion siguiente
  M[desde, hacia] <- M[desde, hacia] + 1
}

# Dos ajustes importantes antes de usar la tabla:
#   - "+ 1": evita que algun par tenga conteo 0 (su logaritmo daria -infinito).
#   - "log": permite SUMAR puntajes en vez de MULTIPLICAR conteos, lo que evita
#            numeros gigantes/diminutos que la computadora no puede manejar.
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

# Lo convertimos a numeros igual que al corpus.
codigos_cifrado <- a_indices(strsplit(limpiar(mensaje_cifrado), "")[[1]])

# ---------------------------------------------------------------------------
# 5) Puntaje de una correspondencia 'f'
#    'f' es la correspondencia candidata: un vector de largo 27 donde f[k] dice
#    a que letra se traduce el simbolo cifrado numero k.
#    Ejemplo: si f[10] = 20, entonces el simbolo cifrado 10 se lee como la
#    letra numero 20 ("t").
#
#    El puntaje recorre el texto YA descifrado y, por cada par de letras
#    consecutivas, busca en logM que tan comun es ese par. Suma todos esos
#    valores. Cuanto mas "ingles" luce el resultado, mas alto el puntaje.
# ---------------------------------------------------------------------------
puntaje <- function(f) {
  descifrado <- f[codigos_cifrado]      # traduce cada simbolo del mensaje con 'f'
  n <- length(descifrado)
  desde <- descifrado[1:(n - 1)]        # cada letra del texto descifrado...
  hacia <- descifrado[2:n]              # ...junto con la que le sigue
  # logM[cbind(desde, hacia)] toma, para cada par (desde, hacia), el valor de
  # esa casilla de logM. cbind arma una tabla de dos columnas (fila, columna)
  # y R devuelve un valor por fila. Luego los sumamos todos.
  sum(logM[cbind(desde, hacia)])
}

# texto_descifrado: pasa de la correspondencia 'f' al texto legible.
texto_descifrado <- function(f) {
  paste(alfabeto[f[codigos_cifrado]], collapse = "")
}

# ---------------------------------------------------------------------------
# 6) Una "cadena": empieza al azar y mejora la correspondencia de a poco
#    En cada paso propone un cambio chico y lo acepta o rechaza. A veces acepta
#    un cambio que empeora, a proposito, para no quedar atrapada en una mala
#    solucion (es la idea central de Metropolis-Hastings).
# ---------------------------------------------------------------------------
una_cadena <- function(n_pasos) {

  # --- Paso 4 del esquema: arranque al azar ---
  f  <- sample(n_simbolos)   # una mezcla aleatoria de los numeros 1..27
  pf <- puntaje(f)           # su puntaje

  mejor_f  <- f              # vamos guardando la mejor correspondencia vista
  mejor_pf <- pf

  for (i in 1:n_pasos) {

    # --- Paso 5: proponer un cambio (intercambiar dos letras) ---
    candidato <- f
    dos <- sample(n_simbolos, 2)    # elige 2 posiciones distintas, p. ej. 3 y 19
    a <- dos[1]
    b <- dos[2]
    temporal     <- candidato[a]    # intercambio clasico con variable auxiliar:
    candidato[a] <- candidato[b]    #   guardo a, piso a con b,
    candidato[b] <- temporal        #   y pongo el guardado en b

    # --- Paso 6: puntuar el candidato ---
    pc <- puntaje(candidato)

    # --- Paso 7: aceptar o rechazar ---
    if (pc > pf) {
      aceptar <- TRUE               # si el candidato es mejor, lo aceptamos siempre
    } else {
      # si es peor, lo aceptamos con probabilidad exp(pc - pf).
      # Como pc < pf, esa probabilidad es un numero entre 0 y 1: cuanto mas
      # empeora, menos probable es aceptarlo. Esto permite escapar de soluciones
      # "buenas pero no las mejores".
      aceptar <- runif(1) < exp(pc - pf)
    }

    if (aceptar) {
      f  <- candidato               # nos mudamos al candidato
      pf <- pc
      if (pf > mejor_pf) {          # si batimos el record de esta cadena, lo guardamos
        mejor_pf <- pf
        mejor_f  <- f
      }
    }
    # si no se acepta, no se toca nada y se prueba otro cambio en la vuelta siguiente
  }

  list(f = mejor_f, puntaje = mejor_pf)   # devolvemos lo mejor que vio esta cadena
}

# ---------------------------------------------------------------------------
# 7) Muchos reinicios: la clave para que funcione de verdad
#    Una sola cadena puede quedar atrapada en una correspondencia "casi buena"
#    pero incorrecta. Por eso corremos muchas cadenas, cada una desde un
#    arranque distinto, y nos quedamos con la mejor de todas. Basta con que UNA
#    encuentre la solucion correcta (que tiene un puntaje claramente mas alto).
# ---------------------------------------------------------------------------
n_reinicios <- 120      # cuantas cadenas correr
n_pasos     <- 6000     # pasos dentro de cada cadena

mejor_global_f  <- NULL
mejor_global_pf <- -Inf   # arranca en -infinito para que cualquier puntaje real lo supere

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

# Si el texto no queda del todo legible: subi n_reinicios (mas intentos) o
# agrega mas libros al texto de referencia para mejorar las estadisticas.
