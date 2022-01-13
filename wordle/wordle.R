

# ----- LIBRERÍAS -----
library(tidyverse)
library(skimr)
library(purrr)

# ----- CARGA + PREPROCESADO -----
source("./carga_corpus.R")

# ----- RESUMEN NUMÉRICO INICIAL -----

# Resumen del corpus completo
# filas: 737 798 / columnas: 3
datos_CREA %>% skim()

# Filtrado palabras muy poco frecuentes (5 de cada 10 000)
datos_CREA_filtrado <- datos_CREA %>% filter(frec_norm > 0.5)

# Frecuencias absolutas
datos_CREA_filtrado %>%
  summarise(frec_min = min(frec_abs),
            frec_max = max(frec_abs),
            frec_mean = mean(frec_abs),
            frec_median = median(frec_abs),
            frec_sd = sd(frec_abs))
quantile(datos_CREA_filtrado$frec_abs,
         probs = c(0, 0.25, 0.5, 0.75, 0.85, 0.9, 0.95, 0.99))

# Frecuencias normalizadas
datos_CREA_filtrado %>%
  summarise(frec_min = min(frec_norm),
            frec_max = max(frec_norm),
            frec_mean = mean(frec_norm),
            frec_median = median(frec_norm),
            frec_sd = sd(frec_norm))
quantile(datos_CREA_filtrado$frec_norm,
         probs = c(0, 0.25, 0.5, 0.75, 0.85, 0.9, 0.95, 0.99))


# ----- FUNCIONES AUXILIARES ------

# Matriz letras tokenizadas
matriz_letras <- function(corpus, n = 5) {
  
  # Filtramos
  corpus_filtrado <- corpus %>% filter(nletras == n)
  
  # Creamos matriz de letras
  matriz_letras <-
    matrix(unlist(strsplit(corpus_filtrado$palabra, "")),
             ncol = nrow(corpus_filtrado))
  
  # Frecuencia de letras en las palabras de wordle
  frecuencia_letras <-
    as_tibble(as.character(matriz_letras)) %>%
    group_by(value) %>% count() %>%
    ungroup %>%
    mutate(porc = 100 * n / sum(n))
  
  # Output
  return(list("corpus_filtrado" = corpus_filtrado,
              "matriz_letras" = matriz_letras,
              "frecuencia_letras" = frecuencia_letras))
}
tokens_n7 <- matriz_letras(datos_CREA_wordle, n = 7)
tokens_n9 <- matriz_letras(datos_CREA_wordle, n = 9)

# Puntuamos palabras
puntuar_palabras <- function(palabras, matriz_letras,
                             frecuencia_letras) { 
  
  # Puntuar palabras
  palabras_puntuadas <-
    palabras %>%
    mutate(punt_freq_letras =
             apply(matriz_letras, MARGIN = 2,
                   FUN = function(x) {
                     sum(frecuencia_letras$porc[frecuencia_letras$value %in% x] / 100)
                   }),
           punt_freq_norm_letras =
             punt_freq_letras / max(punt_freq_letras),
           ind_blau =
             apply(matriz_letras, MARGIN = 2,
                   FUN = function(x) { 1 - sum((table(x) / sum(table(x)))^2)}),
           ind_blau_norm = ind_blau / max(ind_blau),
           punt_total = punt_freq_letras * ind_blau_norm,
           punt_total_w = punt_total * (log_frec_abs / sum(log_frec_abs)),
           punt_total_w_norm = punt_total_w / max(punt_total_w))
  
  # Output
  return(palabras_puntuadas)
  
}

# Iteración del juego
iteracion <- function(inicial, clave) {
  
  # Jugada
  bien_colocadas <-
    unlist(map2(strsplit(inicial, ""), strsplit(clave, ""),
                function(x, y) { x == y }))
  mal_colocadas <-
    unlist(map2(strsplit(inicial, ""), strsplit(clave, ""),
                function(x, y) { x %in% y })) &
    !bien_colocadas
  errores <- !(bien_colocadas | mal_colocadas)
  
  # Output
  return(list("bien_colocadas" = bien_colocadas,
              "mal_colocadas" = mal_colocadas,
              "errores" = errores))
}

# Simulación
simular_wordle <-
  function(intentos = 1, corpus, matriz_corpus, generar_equi = TRUE,
           iniciar_equi = TRUE) {
    
    # probabilidades de salir la palabra
    # * si generar_equi = TRUE --> equiprobables
    # * si generar_equi = FALSE --> en función de log-frecuencias
    if (generar_equi) {
      
      probs_gen <- rep(1 / nrow(corpus), nrow(corpus))
      
    } else {
      
      probs_gen <- corpus$punt_total_w / sum(corpus$punt_total_w)
      
    }
    
    # Palabra a adivinar
    clave <- sample(corpus$palabra, size = 1, prob = probs_gen)
    propiedades_clave <- corpus %>% filter(palabra == clave)
    
    # Palabra inicial
    if (iniciar_equi) {
      
      inicial <- sample(corpus$palabra, size = 1)
      
    } else {
      
      inicial <- corpus %>%
        arrange(desc(punt_total_w_norm)) %>%
        slice(1) %>% pull(palabra)
      
    }
    propiedades_inicial <- corpus %>% filter(palabra == inicial)
    
    # Inicialización
    palabra_0 <- inicial
    candidatas <- corpus
    matriz_candidatas <- matriz_corpus
    salida <- list()
    for (i in 1:intentos) {
      
      salida[[i]] <- iteracion(palabra_0, clave)
      
      idx_palabras <-
        apply(matriz_candidatas, MARGIN = 2,
              FUN = function(x) {
                all(x[salida[[i]]$bien_colocadas] ==
                      unlist(strsplit(palabra_0, ""))[salida[[i]]$bien_colocadas]) }) &
        apply(matriz_candidatas, MARGIN = 2,
              FUN = function(x) {
                all(!(x %in% unlist(strsplit(palabra_0, ""))[salida[[i]]$errores])) })
      
      if (any(salida[[i]]$mal_colocadas)) {
        
        idx_palabras <- idx_palabras &
          apply(matriz_candidatas, MARGIN = 2,
                FUN = function(x) {
                  all(unlist(strsplit(palabra_0, ""))[salida[[i]]$mal_colocadas] %in% x) &
                    all(!mapply(x[which(salida[[i]]$mal_colocadas)],
                                unlist(strsplit(palabra_0, ""))[salida[[i]]$mal_colocadas],
                                FUN = function(x, y) { x == y})) } )
      }
      
      # Seleccionamos
      if (any(idx_palabras)) {
        
        matriz_candidatas <- matriz_candidatas[, idx_palabras]
        candidatas <- candidatas[idx_palabras, ]
        palabra_0 <-
          candidatas %>% arrange(desc(punt_total_w_norm)) %>%
          slice(1) %>% pull(palabra)
        
      }
      
      if (nrow(candidatas) <= 1) {
        
        break
      } 
    }
    
    intentos <- ifelse(nrow(candidatas) == 1, i, intentos + 1)
    
    # Output
    return(list("palabra_clave" = clave, "inicial" = inicial,
                "salida" = salida, "candidatas" = candidatas,
                "palabra_0" = palabra_0,
                "matriz_candidatas" = matriz_candidatas,
                "intentos" = intentos,
                "propiedades_clave" = propiedades_clave,
                "propiedades_inicial" = propiedades_inicial))
  }



# ----- SIMULACIONES -----

simulacion_wordle <-
  function(corpus, umbral_frec_norm = 5, n_letras = 5, 
           simulaciones = 1e3, nintentos = 6,
           generar_equi = generar_equi,
           iniciar_equi = iniciar_equi) {
    
    # Filtramos por frecuencia normalizada
    # el corpus total para el corpus del wordle
    corpus_wordle <-
      corpus %>% filter(frec_norm > umbral_frec_norm)
    
    # Solo queremos las que tengan 5 letras
    tokens_wordle <- matriz_letras(corpus_wordle, n = n_letras)
    palabras_wordle <- tokens_wordle$corpus_filtrado
    matriz_letras_wordle <- tokens_wordle$matriz_letras
    frec_letras_wordle <- tokens_wordle$frecuencia_letras
    corpus_wordle_puntuado <-
      puntuar_palabras(palabras_wordle, matriz_letras_wordle,
                       frec_letras_wordle)
  
    # Simulación
    resultados <- 
      replicate(simulaciones,
                simular_wordle(intentos = nintentos,
                               corpus_wordle_puntuado,
                               matriz_letras_wordle,
                               generar_equi = generar_equi,
                               iniciar_equi = iniciar_equi))
    # Output
    return(list("corpus_wordle" = corpus_wordle,
                "palabras_wordle" = palabras_wordle,
                "matriz_letras_wordle" = matriz_letras_wordle,
                "frec_letras_wordle" = frec_letras_wordle,
                "corpus_wordle_puntuado" = corpus_wordle_puntuado,
                "resultados" = resultados))
  }

# Filtramos solo aquellas que aparecen > umbral_frec_norm
# de cada 1000 teniendo en cuenta que 
# norm = abs / (152560 / 1000) 
# entradas RAE: 93 111 entradas
# con americanismos: 112 111 vocablos


# * 6 intentos y 5 letras
# * 3000 simulaciones
# * con palabra inicial y clave equiprobables
# * solo palabras con frec_norm > 5 (5 en cada 1000)
umbral_frec_norm <- 5
simulaciones <- 3e3
generar_equi <- TRUE
iniciar_equi <- TRUE
simulacion1 <-
  simulacion_wordle(datos_CREA_filtrado,
                    umbral_frec_norm = umbral_frec_norm,
                    simulaciones = simulaciones,
                    generar_equi = generar_equi,
                    iniciar_equi = iniciar_equi)
distrib_intentos1 <-
  100 * table(unlist(simulacion1$resultados["intentos", ])) / simulaciones
media_intentos1 <- mean(unlist(simulacion1$resultados["intentos", ]))

# Lista palabras clave
lista_palabras_clave <-
  map_dfr(simulacion1$resultados["propiedades_clave", ],
                as_tibble) %>%
  mutate("n_intentos" = unlist(simulacion1$resultados["intentos", ]))
lista_palabras_clave %>%
  group_by(n_intentos) %>%
  summarise(punt_freq_mean = mean(punt_freq_norm_letras),
            ind_blau_mean = mean(ind_blau_norm))

# Lista palabras inicial
lista_palabras_inicial <-
  map_dfr(simulacion1$resultados["propiedades_inicial", ],
          as_tibble) %>%
  mutate("n_intentos" = unlist(simulacion1$resultados["intentos", ]))
lista_palabras_inicial %>%
  group_by(n_intentos) %>%
  summarise(punt_freq_mean = mean(punt_freq_norm_letras),
            ind_blau_mean = mean(ind_blau_norm))

# * 6 intentos y 5 letras
# * 3000 simulaciones
# * con palabra inicial y clave equiprobables
# * solo palabras con frec_norm > 8 (8 en cada 1000)
umbral_frec_norm <- 8
simulaciones <- 3e3
generar_equi <- TRUE
iniciar_equi <- TRUE
simulacion2 <-
  simulacion_wordle(datos_CREA_filtrado,
                    umbral_frec_norm = umbral_frec_norm,
                    simulaciones = simulaciones,
                    generar_equi = generar_equi,
                    iniciar_equi = iniciar_equi)
distrib_intentos <-
  100 * table(unlist(simulacion2$resultados["intentos", ])) / simulaciones
media_intentos <- mean(unlist(simulacion2$resultados["intentos", ]))

# Lista palabras clave
lista_palabras_clave2 <-
  map_dfr(simulacion2$resultados["propiedades_clave", ],
          as_tibble) %>%
  mutate("n_intentos" = unlist(simulacion2$resultados["intentos", ]))
lista_palabras_clave2 %>%
  group_by(n_intentos) %>%
  summarise(punt_freq_mean = mean(punt_freq_norm_letras),
            ind_blau_mean = mean(ind_blau_norm))

# Lista palabras inicial
lista_palabras_inicial2 <-
  map_dfr(simulacion2$resultados["propiedades_inicial", ],
          as_tibble) %>%
  mutate("n_intentos" = unlist(simulacion2$resultados["intentos", ]))
lista_palabras_inicial2 %>%
  group_by(n_intentos) %>%
  summarise(punt_freq_mean = mean(punt_freq_norm_letras),
            ind_blau_mean = mean(ind_blau_norm))


