# Descifrando Edgar Allan Poe con MCMC

> *"Las coincidencias en general son grandes obstáculos para esa clase de pensadores que han sido educados para no saber nada de la teoría de las probabilidades, esa teoría a la que los objetos más gloriosos de la investigación humana deben las más gloriosas ilustraciones."*
>
> — Edgar Allan Poe, *Los crímenes de la calle Morgue*

## Resumen

Este repositorio contiene el trabajo práctico para la materia **Modelado y Simulación** (TUIA) en el que exploramos el **Markov Chain Monte Carlo (MCMC)** a través del ejemplo clásico de desciframiento de cifrados sustitutos.

Utilizando el Capítulo 5 de *Introduction to Stochastic Processes with R* (Dobrow, 2016), implementamos el **algoritmo de Metropolis-Hastings** para resolver un problema criptográfico: recuperar el texto original de una novela de Jane Austen que fue cifrada con un códice aleatorio.

## Contenido del repositorio

### Archivos principales de código R

- **`decode.R`** — Implementación básica del descifrador MCMC
- **`decode_restarts.R`** — Versión mejorada con múltiples cadenas (restarts) para evitar óptimos locales
- **`decode_simple.R`** — Versión simplificada y comentada extensamente, ideal para aprendizaje
- **`burnin.R`** — Análisis de convergencia con diagnósticos visuales
- **`adyacentes.R`** — Aplicación secundaria: secuencias binarias sin unos adyacentes (Sección 5.1)
- **`5.2mcmc2.R`** — Notas y ejercicios adicionales de la Sección 5.2

### Datos y recursos

- **`corpus.txt`** — Corpus de seis novelas de Jane Austen (para frecuencias de bigramas)
- **`austen.txt`** — Versión alternativa del corpus

## El algoritmo: Metropolis-Hastings

El corazón de este trabajo es entender cómo funciona el algoritmo de Metropolis-Hastings en la práctica.

### Flujo de ejecución

```
run_chain() itera a través de estos pasos:

1. PROPOSAL (Propuesta)
   ↓ Intercambiar dos letras aleatoriamente
   
2. SCORE RATIO (Evaluación)
   ↓ Calcular ratio = score(f*) / score(f)
   
3. DECIDE (Decisión estocástica)
   ↓ ¿U ≤ ratio? (donde U ~ Uniform(0,1))
   
   ├─→ YES: Aceptar f* como nuevo estado (4. Accept)
   └─→ NO:  Rechazar, mantener f (4. Reject)
   
5. RECORD & REPEAT
   ↓ Guardar estado actual, siguiente iteración
```

### ¿Por qué funciona?

El algoritmo mantiene **equilibrio detallado** con la distribución objetivo. Aunque la cadena puede rechazar propuestas (a diferencia de muestreo de rechazo), la aceptación estocástica garantiza que:

- Transiciones frecuentes hacia estados mejores
- Ocasionales transiciones a estados peores (para exploración)
- Convergencia a la distribución estacionaria

## Datos de entrada

El cifrado se genera aplicando una permutación aleatoria (sustitución) al texto original. El desafío: recuperar la permutación original sin conocerla.

**Estrategia:**
1. Calcular matriz de frecuencias de bigramas del corpus de Austen
2. Para cada cifrado propuesto, asignar puntuación basada en qué tan bien los bigramas del texto descifrado coinciden con el corpus
3. La cadena de Markov converge hacia cifrados con puntuaciones altas
4. Con burn-in adecuado, el estado final es una buena aproximación al cifrado original

## Reproducibilidad

Este trabajo fue desarrollado y testeado con la siguiente configuración:

### Entorno de software

```
R version 4.6.0 (2026-04-24)
Platform: x86_64-pc-linux-gnu (64-bit)
RStudio: Compatible con versiones recientes
```

**Dependencias de R:**
- Funciones base de R solamente (no hay librerías externas)
- Compiladores: GCC (incluyendo BLAS/LAPACK)

**Sistema operativo:**
```
Ubuntu 24.04.4 LTS (Jammy Jellyfish)
Kernel: Linux 6.17.0-35-generic
Zona horaria: America/Argentina/Cordoba (UTC-3)
```

### Hardware de desarrollo

```
Procesador: Intel® Core™ i7-3770 (8 núcleos)
Memoria RAM: 16 GB
Almacenamiento: 512 GB SSD
GPU: Intel® HD Graphics 4000 (IVB GT2)
```

### Para reproducir

1. **Versión mínima recomendada:** R ≥ 3.5
2. **Sin dependencias de CRAN:** Los scripts solo usan funciones base
3. **Localización:** Recomendado `es_ES.UTF-8` para compatibilidad con textos en español

**Verificar tu entorno:**
```r
sessionInfo()
```

Si tu versión de R es 4.0+, todo debería funcionar sin cambios.

## Uso

### Ejemplo rápido: decodificar con reintentos

```r
source("decode_restarts.R")

# Cargar corpus
corpus <- scan("corpus.txt", what = character(), sep = " ", quiet = TRUE)

# Configurar parámetros
n_restarts <- 10
n_iterations <- 50000
burn_in <- 5000

# Ejecutar
resultados <- run_multiple_chains(n_restarts, n_iterations, burn_in)
```

### Diagnóstico de convergencia

```r
source("burnin.R")

# Visualizar trazas, zoom y media acumulativa
plot_diagnostics(cadena, burn_in = 5000)
```

## Conceptos clave

### Equilibrio detallado

Para que la cadena tenga distribución estacionaria π:

$$\pi(x) P(x \to y) = \pi(y) P(y \to x)$$

Metropolis-Hastings lo logra con la **ratio de aceptación**:

$$\alpha(x, y) = \min\left(1, \frac{\pi(y) q(x|y)}{\pi(x) q(y|x)}\right)$$

donde q es la distribución propuesta.

### Burn-in

Las primeras iteraciones dependen del estado inicial (arbitrario). El burn-in descarta estas para asegurar que trabajamos con muestras de la distribución estacionaria.

### Diagnósticos

- **Traza (trace plot):** Evolución temporal del estado
- **Media acumulativa:** Convergencia del promedio estimado
- **Histograma posterior:** Forma de la distribución final

## Referencias

- Dobrow, R. P. (2016). *Introduction to Stochastic Processes with R*. Wiley.
- Poe, E. A. (1841). *The Murders in the Rue Morgue*. 

## Autor

Alfredo Sanz  
Programa TUIA — Maestría en Informática Aplicada  
UADER, Rosario, Argentina

---

**Estado:** Aprobado ✓  
**Última actualización:** Junio 2026
