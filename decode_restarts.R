# ============================================================================
# decode_restarts.R
# Descifrado de un cifrado por sustitucion con Metropolis-Hastings (MCMC).
# Mejoras respecto a la version basica:
#   (1) Corpus grande: concatena las seis novelas de Jane Austen (~4M chars),
#       como en el Ejemplo 5.3 de Dobrow, en vez de un solo libro.
#   (2) Reinicios multiples: corre muchas cadenas cortas desde permutaciones
#       iniciales aleatorias y conserva la de mayor puntaje global. Esto
#       evita quedar atrapado en un optimo local (el problema de una sola
#       cadena que se "congela" en un texto con aspecto ingles pero erroneo).
# ============================================================================

set.seed(1)

# ---------------------------------------------------------------------------
# 1. Alfabeto y utilidades
# ---------------------------------------------------------------------------
alphabet <- c(letters, " ")
n_sym    <- length(alphabet)            # 27
idx      <- function(ch) match(ch, alphabet)

clean <- function(s) {
  s <- tolower(paste(s, collapse = " "))
  s <- gsub("[^a-z ]", " ", s)
  s <- gsub(" +", " ", s)
  trimws(s)
}

# ---------------------------------------------------------------------------
# 2. Corpus grande: las seis novelas de Jane Austen
#    IDs de Project Gutenberg:
#      1342 Pride and Prejudice   161 Sense and Sensibility   158 Emma
#       141 Mansfield Park        105 Persuasion              121 Northanger Abbey
#    Alternativa mas comoda (descomentar):
#      install.packages("gutenbergr")
#      library(gutenbergr)
#      ref_text <- clean(gutenberg_download(c(1342,161,158,141,105,121))$text)
# ---------------------------------------------------------------------------
austen_ids <- c(1342, 161, 158, 141, 105, 121)

get_book <- function(id) {
  url <- sprintf("https://www.gutenberg.org/cache/epub/%d/pg%d.txt", id, id)
  con <- url(url)
  on.exit(close(con))
  paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = " ")
}

if (!file.exists("corpus.txt")) {
  message("Descargando corpus de Project Gutenberg (puede tardar) ...")
  books <- vapply(austen_ids, get_book, character(1))
  writeLines(paste(books, collapse = " "), "corpus.txt")
}

ref_text <- clean(readLines("corpus.txt", warn = FALSE, encoding = "UTF-8"))
cat(sprintf("Corpus de referencia: %d caracteres\n", nchar(ref_text)))

# ---------------------------------------------------------------------------
# 3. Matriz de transiciones M (27 x 27) + log con suavizado de Laplace
# ---------------------------------------------------------------------------
ref_codes <- idx(strsplit(ref_text, "")[[1]])
pairs <- cbind(ref_codes[-length(ref_codes)], ref_codes[-1])
tab <- table(factor(pairs[, 1], levels = 1:n_sym),
             factor(pairs[, 2], levels = 1:n_sym))
M <- matrix(as.integer(tab), n_sym, n_sym, dimnames = list(alphabet, alphabet))
logM <- log(M + 1)

# ---------------------------------------------------------------------------
# 4. Mensaje cifrado (Ejemplo 5.3)
# ---------------------------------------------------------------------------
cipher <- paste0(
  "ahicainqcaqx ic zqcqwbl bwq zwqbj xjustlicz tlhamx ic jyq kbr ho jybj ",
  "albxx ho jyicmqwx kyh ybgq tqqc qnuabjqn jh mchk chjyicz ho jyq jyqhwr ",
  "ho dwhtbtilijiqx jybj jyqhwr jh kyiay jyq shxj zlhwihux htpqajx ho yusbc ",
  "wqxqbway bwq icnqtjqn ohw jyq shxj zlhwihux ho illuxjwbjihcx ",
  "qnzbw bllqc dhq jyq suwnqwx ic jyq wuq shwzuq")

cipher_codes <- idx(strsplit(clean(cipher), "")[[1]])

# ---------------------------------------------------------------------------
# 5. Puntaje y decodificacion
# ---------------------------------------------------------------------------
score <- function(f) {
  decoded <- f[cipher_codes]
  n <- length(decoded)
  sum(logM[cbind(decoded[-n], decoded[-1])])
}

decode_text <- function(f) paste(alphabet[f[cipher_codes]], collapse = "")

# ---------------------------------------------------------------------------
# 6. Una cadena de Metropolis-Hastings (devuelve la MEJOR f que vio)
# ---------------------------------------------------------------------------
run_chain <- function(n_iter, start = sample(n_sym)) {
  f   <- start
  cur <- score(f)
  best_f <- f
  best   <- cur
  for (i in 1:n_iter) {
    swap <- sample(n_sym, 2)
    fs   <- f
    fs[swap] <- f[rev(swap)]
    ps   <- score(fs)
    if (log(runif(1)) < ps - cur) {        # propuesta simetrica => a = score*/score
      f <- fs; cur <- ps
      if (cur > best) { best <- cur; best_f <- f }
    }
  }
  list(f = best_f, score = best)
}

# ---------------------------------------------------------------------------
# 7. Reinicios multiples: muchas cadenas cortas, conservar la mejor global
# ---------------------------------------------------------------------------
n_restarts <- 100
n_iter     <- 5000

global_best_f <- NULL
global_best   <- -Inf

for (r in 1:n_restarts) {
  res <- run_chain(n_iter)                 # arranque aleatorio distinto cada vez
  if (res$score > global_best) {
    global_best   <- res$score
    global_best_f <- res$f
    cat(sprintf("[reinicio %3d] score=%.1f | %s\n",
                r, global_best, decode_text(global_best_f)))
  }
}

cat("\n--- Mejor descifrado encontrado ---\n")
cat(decode_text(global_best_f), "\n")
cat(sprintf("log-score = %.2f\n", global_best))
