# ----- carga de datos -----

# Fuente: CREA (152560 documentos analizados)
# Corpus de Referencia del Español Actual (CREA)
# https://corpus.rae.es/lfrecuencias.html
datos_brutos_CREA <-
  read_delim(file = "./CREA_bruto.txt",
             delim = "\t", )

# Eliminamos columna de orden y separamos última columna en dos
datos_CREA <-
  datos_brutos_CREA[, -1] %>%
  separate(col = 2, sep = "\t",
           into = c("frec_abs", "frec_norm"))

# Renombramos columnas
names(datos_CREA) <- c("palabra", "frec_abs", "frec_norm")

# Convertimos a número que vienen como cadenas de texto
datos_CREA <- datos_CREA %>%
  mutate(frec_abs = as.numeric(gsub(",", "", frec_abs)),
         frec_norm = as.numeric(frec_norm))

# Eliminamos posibles duplicados y convertimos tildes
datos_CREA <- datos_CREA  %>%
  mutate(palabra = gsub(" ", "", iconv(palabra, "latin1")))

# Quitamos tildes pero no queremos eliminar la ñ
datos_CREA <- datos_CREA %>%
  mutate(palabra = 
           gsub("á", "a",
                gsub("é", "e",
                     gsub("í", "i",
                          gsub("ó", "o",
                               gsub("ú", "u",
                                    gsub("ü", "u",
                                         as.character(palabra))))))),
         # log(frec. absolutas)
         log_frec_abs = log(frec_abs), 
         # log(frec. normalizadas)
         log_frec_norm = log(frec_norm),
         # distribución de frec_norm
         int_frec_norm =
           cut(frec_norm,
               breaks = c(0.01, 0.05, 0.1, 0.5, 1:5,
                          10, 20, 40, 60, 80, Inf)),
         # número de letras
         nletras = nchar(palabra)) %>%
  # eliminamos duplicados
  distinct(palabra, .keep_all = TRUE) 

