library(tidyverse)
library(purrr)

# Lectura CREA
datos_CREA <- read_csv(file = "./CREA_procesado.csv")

# Quitamos tildes pero no queremos eliminar la ñ
datos_CREA <- datos_CREA %>%
  mutate(palabra =
           gsub("ö", "o",
                gsub("ä", "a",
                     gsub("ò", "o",
                          gsub("ï", "i",
                               gsub("ô", "o",
                                    gsub("â", "a",
                                         gsub("ë", "e",
                                              gsub("ê", "e",
                                                   gsub("ã", "a",
                                                        gsub("î", "i",
                                                             gsub("ù", "u",
                                                                  gsub("¢", "c",
                                                                       gsub("ì", "i",
                                                                            gsub("è", "e",
                                                                                 gsub("à", "a", gsub("ç", "c",
                                                                                                     gsub("á", "a",
                                                                                                          gsub("é", "e",
                                                                                                               gsub("í", "i",
                                                                                                                    gsub("ó", "o",
                                                                                                                         gsub("ú", "u",
                                                                                                                              gsub("ü", "u",
                                                                                                                                   as.character(palabra)))))))))))))))))))))))) %>%
  # eliminamos duplicados
  distinct(palabra, .keep_all = TRUE) %>%
  # Eliminamos palabras con '
  filter(!grepl("'", palabra) & !grepl("ø", palabra))

# Estadísticas
datos_CREA <- datos_CREA %>%
  mutate(# frec. relativa
    frec_relativa = frec_abs / sum(frec_abs),
    # log(frec. absolutas)
    log_frec_abs = log(frec_abs), 
    # log(frec. normalizadas)
    log_frec_rel = log_frec_abs / sum(log_frec_abs),
    # distribución de frec_norm
    int_frec_norm =
      cut(frec_norm,
          breaks = c(-Inf, 0.01, 0.05, 0.1, 0.5, 1:5,
                     10, 20, 40, 60, 80, Inf)),
    # número de letras
    nletras = nchar(palabra))

# Palabras CREA filtradas por frecuencia
datos_CREA_filtrado <- datos_CREA %>% filter(frec_norm >= 1)

# Palabras wordle
palabras_wordle <- read_csv(file = "./palabras_wordle.csv")

# Completamos CREA
palabras_ausentes <- 
  setdiff(palabras_wordle %>% pull(palabra),
          datos_CREA_filtrado %>% filter(nletras == 5) %>%
            pull(palabra))
datos_CREA_filtrado <-
  rbind(datos_CREA_filtrado,
        datos_CREA %>% filter(palabra %in% palabras_ausentes)) %>%
  add_row(palabra = "cotar", frec_abs = 10,
          log_frec_abs = log(10), nletras = 5) %>%
  add_row(palabra = "titar", frec_abs = 10,
          log_frec_abs = log(10), nletras = 5) %>%
  add_row(palabra = "kopek", frec_abs = 10,
          log_frec_abs = log(10), nletras = 5)

datos_palabras_wordle <-
  datos_CREA_filtrado %>%
  filter(palabra %in% palabras_wordle$palabra) %>%
  arrange(desc(frec_relativa))

# Matriz letras tokenizadas
matriz_letras <- function(corpus, n = 5) {
  
  if (!is.null(n)) {
    
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
    
  } else {
    
    corpus_filtrado <- corpus
    
    # Creamos matriz de letras
    matriz_letras <- unlist(strsplit(corpus_filtrado$palabra, ""))
    
    # Frecuencia de letras en las palabras de wordle
    frecuencia_letras <-
      as_tibble(as.character(matriz_letras)) %>%
      group_by(value) %>% count() %>%
      ungroup %>%
      mutate(porc = 100 * n / sum(n))
  }
  
  # Output
  return(list("corpus_filtrado" = corpus_filtrado,
              "matriz_letras" = matriz_letras,
              "frecuencia_letras" = frecuencia_letras))
}

# Puntuar letras
puntuar_letras <- function(corpus, n = 5) {
  
  if (!is.null(n)) {
    
    # Filtramos
    corpus_filtrado <- corpus %>% filter(nletras == n)
    
    # Creamos matriz de letras
    matriz_letras <-
      matrix(unlist(strsplit(corpus_filtrado$palabra, "")),
             ncol = nrow(corpus_filtrado))
    pesos <- rep(corpus_filtrado$frec_relativa, each = n)
    matriz_letras_pesos <-
      tibble("matriz_letras" = 
               unlist(strsplit(corpus_filtrado$palabra, "")),
             pesos)
    
    # Ponderación de letras
    frecuencia_letras <-
      matriz_letras_pesos %>%
      group_by(matriz_letras) %>%
      summarise(peso_promediado = sum(pesos, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(peso_promediado_rel =
               peso_promediado / sum(peso_promediado, na.rm = TRUE))
    
  } else {
    
    corpus_filtrado <- corpus
    
    # Creamos matriz de letras
    matriz_letras <- unlist(strsplit(corpus_filtrado$palabra, ""))
    pesos <-
      unlist(mapply(corpus_filtrado$frec_relativa,
                    corpus_filtrado$nletras,
                    FUN = function(x, y) { rep(x, y)}))
    matriz_letras_pesos <- tibble(matriz_letras, pesos)
    
    # Ponderación de letras
    frecuencia_letras <-
      matriz_letras_pesos %>%
      group_by(matriz_letras) %>%
      summarise(peso_promediado = sum(pesos, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(peso_promediado_rel =
               peso_promediado /
               sum(peso_promediado, na.rm = TRUE))
  }
  
  # Output
  return(frecuencia_letras)
}
puntuacion_letras_global <-
  puntuar_letras(datos_CREA_filtrado, n = NULL)
puntuacion_letras_5 <-
  puntuar_letras(datos_CREA_filtrado, n = 5)

# Letras iniciales/finales
letras_iniciales <-
  tibble("letras_iniciales" =
           map_chr(strsplit(datos_CREA_filtrado %>%
                              filter(nletras == 5) %>%
                              pull(palabra), ""),
                   function(x) { x[1] })) %>%
  group_by(letras_iniciales) %>% count() %>% ungroup() %>%
  mutate(porc = 100 * n / sum(n))
letras_finales <-
  tibble("letras_finales" =
           map_chr(strsplit(datos_CREA_filtrado %>%
                              filter(nletras == 5) %>%
                              pull(palabra), ""),
                   function(x) { rev(x)[1] })) %>%
  group_by(letras_finales) %>% count() %>% ungroup() %>%
  mutate(porc = 100 * n / sum(n))


# Puntuar palabras
puntuar_palabras <-
  function(palabras, letras_puntuadas, letras_iniciales,
           letras_finales, nletras = 5) { 
    
    # Matriz letras
    matriz_letras_corpus <- matriz_letras(palabras, n = nletras)
    matriz_letras_corpus <- matriz_letras_corpus$matriz_letras
    
    # palabras	peso promediado	peso relativo
    # Puntuar palabras
    palabras_puntuadas <-
      palabras %>% 
      mutate(punt_letras =
               apply(matriz_letras_corpus, MARGIN = 2,
                     FUN = function(x) { sum(letras_puntuadas$peso_promediado_rel[
                       letras_puntuadas$matriz_letras %in% x] * 
                         c((letras_iniciales %>%
                             filter(letras_iniciales == x[1]) %>%
                              pull(porc)) / 100, 
                           1/5, 1/5, 1/5, (letras_finales %>%
                                       filter(letras_finales ==
                                                rev(x)[1]) %>%
                                       pull(porc)) / 100))}),
             ind_blau =
               apply(matriz_letras_corpus, MARGIN = 2,
                     FUN = function(x) { 1 - sum((table(x) / sum(table(x)))^2)}),
             ind_blau_norm = ind_blau / max(ind_blau),
             punt_letras_total = punt_letras * ind_blau_norm,
             punt_total_w = punt_letras_total * log_frec_abs)
    
    # Iniciales y finales
    
    # Output
    return(list("palabras_puntuadas" = palabras_puntuadas,
                "matriz_letras" = matriz_letras_corpus))
    
  }
CREA_puntuado <-
  puntuar_palabras(datos_CREA_filtrado %>%
                     filter(nletras == 5),
                   puntuacion_letras_5, letras_iniciales,
                   letras_finales)
WORDLE_puntuado <-
  puntuar_palabras(datos_palabras_wordle,
                   puntuacion_letras_5, letras_iniciales,
                   letras_finales)
CREA_puntuado$palabras_puntuadas
WORDLE_puntuado$palabras_puntuadas


# Función que dado un resultado en wordle
# nos lo transforma en tres variables lógicas
resultado_wordle <- function(resultado = rep("gris", 5)) {
  
  # Bien colocadas
  bien_colocadas <- resultado == "verde"
  
  # Mal colocadas
  mal_colocadas <- resultado == "amarillo"
  
  # Errores
  errores <- (resultado != "verde" & resultado != "amarillo")
  
  # Output
  return(list("bien_colocadas" = bien_colocadas,
              "mal_colocadas" = mal_colocadas,
              "errores" = errores))
}

# Simulación
jugar_wordle <-
  function(palabras, corpus, palabras_candidatas,
           resultados = list(rep("gris", 5)),
           n_palabras = 7) {
    
    candidatas <- corpus$palabras_puntuadas
    matriz_candidatas <- corpus$matriz_letras
    
    for (i in 1:length(resultados)) {
      
      # Salida de la iteración
      salida <- resultado_wordle(resultados[[i]])
        
      idx_palabras <-
        apply(matriz_candidatas, MARGIN = 2,
              FUN = function(x) {
                all(x[salida$bien_colocadas] ==
                      unlist(strsplit(palabras[[i]], ""))[salida$bien_colocadas]) }) &
        apply(matriz_candidatas, MARGIN = 2,
              FUN = function(x) {
                all(!(x %in% unlist(strsplit(palabras[[i]], ""))[salida$errores])) })
      
        if (any(salida$mal_colocadas)) {
          
          idx_palabras <- idx_palabras &
            apply(matriz_candidatas, MARGIN = 2,
                  FUN = function(x) {
                    all(unlist(strsplit(palabras[[i]], ""))[salida$mal_colocadas] %in% x) &
                      all(!mapply(x[which(salida$mal_colocadas)],
                                  unlist(strsplit(palabras[[i]], ""))[salida$mal_colocadas],
                                  FUN = function(x, y) { x == y})) } )
        }
        
        # Seleccionamos
        matriz_candidatas <- matriz_candidatas[, idx_palabras]
        candidatas <- candidatas[idx_palabras, ]
        
        if (nrow(candidatas) <= 1) {
          
          break
        } 
    }
    
    palabra_siguiente <-
      candidatas %>% arrange(desc(punt_total_w)) %>%
            slice_head(n = n_palabras) %>% pull(palabra)
        
    # Output
    return(glue("Las {min(7, length(palabra_siguiente))} mejores palabras para continuar (ordenadas de mejor peor) son: 
                {paste(palabra_siguiente, collapse = ',')}"))
  }

# Ejemplo de uso
jugar_wordle(c("secta", "habia"), CREA_puntuado, WORDLE_puntuado,
             resultados =
               list(c("gris", "gris", "gris", "gris", "verde"),
                    c("amarillo", "amarillo", "gris", "gris", "verde")))
