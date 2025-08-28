#----Text mining Discursos presidenciales 2010 - 2025----

library(pacman)

#install.packages(c("pdftools", "stringr", "stringi", "tm", "wordcloud", "gridExtra", "RColorBrewer", "forcats", "ggraph", "igraph"), dependencies = T)

p_load(pdftools, stringr, dplyr, tidytext, tidyverse, readr, purrr, stringi, tm, magrittr, ggplot2, wordcloud, gridExtra, RColorBrewer, topicmodels, forcats)

install.packages("igraph")
library(igraph)

# Directorio con los PDFs
directorio <- "C:/Users/vaare/Downloads/textos"
archivos_pdf <- list.files(directorio, pattern = "\\.pdf$", full.names = TRUE)

# Stopwords en español
stopwords_es <- stopwords(kind = "es")
stopwords_en <- stopwords(kind = "en")
re_url <- "(?i)\\b((?:https?://)?(?:www\\.)?(?:[a-z0-9-]+\\.)+[a-z]{2,})(?::\\d+)?(?:/[^\n\\s\"'<>)]*)?\\b"
sw <- c(stopwords_es, stopwords_en, re_url,"aprobado.indd", "argentina.gob.ar", "www.argentina.gob.ar", "www.casarosada.gob.ar", "www.presidencia.gov.co", "q", "max", "quo", "derogase", "deroganse", "https", "si", "n°","nº", "petro", "gustavo", "javier", "milei", "presidente", "anr","vas", "voy", "vamos", "b", "ahi", "dos", "sino", "mismo", "vez", "va", "cada", "hoy", "argentina", "colombia", "n", "p.m", "traves", "cristina", "presidenta", "fernandez", "macri", "santos", "argentinos", "mas", "articulo", "ley", "siguiente", "así", "tal", "ahí", "nacional", "pais", "p", "m", "asi", "aqui", "ademas", "tambien", "nunca", "sustituyese", "país", "ano", "año", "anos", "años", "aquí", "san", "entonces", "decreto", "decir", "final.indd", "además", "describa", "dnp", "día", "sé", "bnp", "atp", "sepyme", "t.o")

# Función para procesar cada PDF
procesar_pdf <- function(ruta_pdf) {
  texto_paginas <- pdf_text(ruta_pdf)
  texto_completo <- paste(texto_paginas, collapse = " ") %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII")
  
  # Extraer año
  fecha <- str_extract(texto_completo, "[A-ZÁÉÍÓÚÑa-záéíóúñ]+,? \\d{1,2} de [A-ZÁÉÍÓÚÑa-záéíóúñ]+ de (19|20)\\d{2}")
  
  # Tokenizar y remover stopwords
  texto_tokens <- tibble(text = texto_completo) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% sw) %>%
    filter(str_length(word) > 2) %>% 
    summarise(texto_limpio = paste(word, collapse = " ")) %>%
    pull(texto_limpio)
  
  tibble(
    archivo = basename(ruta_pdf),
    fecha = fecha,
    texto = texto_tokens
  )
}

# Procesar todos los PDFs
datos_final <- map_dfr(archivos_pdf, procesar_pdf)


# Guardar en CSV
#write_csv(datos_final, "resultado.csv")

datos_final$año <- str_extract(datos_final$fecha, "(19|20)\\d{2}")
datos_final$autor <- str_extract(datos_final$archivo, "Petro|Milei|Duque|Fernandez|Cristina|Macri|Santos")
datos_final$texto <- str_remove_all(datos_final$texto, "[0-9]+")
datos_final <- rename(datos_final, discurso = archivo)
datos_final <- datos_final[,-2]

discursos_cristina <- read.csv2("C:/Users/vaare/Downloads/textos/Data/discursos_cfk_2007_2015.csv", sep = ",")
sw_df <- tibble(rank = seq_along(sw), sw)
sw_df$sw <- as.character(sw_df$sw)

discursos_cristina_limpio <- discursos_cristina %>%
  unnest_tokens(palabra, texto) %>% 
  anti_join(sw_df, by = c("palabra" = "sw")) %>%
  group_by(fecha) %>%
  summarise(texto_limpio = paste(palabra, collapse = " ")) %>%
  ungroup()
discursos_cristina_limpio$autor <- c("Cristina")
discursos_cristina_limpio$año <- str_extract(discursos_cristina_limpio$fecha, "(19|20)\\d{2}")
discursos_cristina_limpio$texto_limpio <- str_remove_all(discursos_cristina_limpio$texto_limpio, "[0-9]+")
discursos_cristina_limpio <- rename(discursos_cristina_limpio, texto = texto_limpio)
discursos_cristina_limpio <- rename(discursos_cristina_limpio,  discurso = fecha)

df_final <- full_join(discursos_cristina_limpio, datos_final)

freq_terminos <- df_final %>%
  unnest_tokens(palabra, texto) %>% 
  count(palabra, autor, sort = TRUE)

total_freq_terms <- freq_terminos %>% 
  group_by(autor) %>% 
  summarise(total = n())

freq_terminos <- left_join(freq_terminos, total_freq_terms)

freq_terminos <- mutate(freq_terminos, ranking = row_number())
freq_terminos <- mutate(freq_terminos, Porcentaje = n/total*100)

ggplot(freq_terminos, aes(n/total, fill = autor)) +
  geom_histogram(show.legend = T) +
  xlim(NA, 0.0009) +
  facet_wrap(~autor, ncol = 2, scales = "free_y")


p1 <- freq_terminos %>%
  group_by(autor) %>% 
  filter(autor == "Petro") %>%
  filter(row_number()<=10) %>% 
  ggplot(aes(n, reorder(palabra, n))) +
  geom_col(show.legend = F, fill = "Yellow2", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de palabras Petro", y= "Palabra", x="Frecuencia")

p2 <- freq_terminos %>%
  group_by(autor) %>%
  filter(autor == "Milei") %>%
  filter(row_number()<=10) %>% 
  ggplot(aes(n, reorder(palabra, n))) +
  geom_col(show.legend = F, fill = "Orange", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de palabras Milei", y= "Palabra", x="Frecuencia")

p3 <- freq_terminos %>%
  group_by(autor) %>%
  filter(autor == "Macri") %>%
  filter(row_number()<=10) %>% 
  ggplot(aes(n, reorder(palabra, n))) +
  geom_col(show.legend = F, fill = "Light Blue", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de palabras Macri", y= "Palabra", x="Frecuencia")

p4 <- freq_terminos %>%
  group_by(autor) %>%
  filter(autor == "Fernandez") %>%
  filter(row_number()<=10) %>% 
  ggplot(aes(n, reorder(palabra, n))) +
  geom_col(show.legend = F, fill = "Red2", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de palabras Fernandez", y= "Palabra", x="Frecuencia")

p5 <- freq_terminos %>%
  group_by(autor) %>%
  filter(autor == "Cristina") %>%
  filter(row_number()<=10) %>% 
  ggplot(aes(n, reorder(palabra, n))) +
  geom_col(show.legend = F, fill = "Purple2", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de palabras Cristina", y= "Palabra", x="Frecuencia")

p6 <- freq_terminos %>%
  group_by(autor) %>%
  filter(autor == "Santos") %>%
  filter(row_number()<=10) %>% 
  ggplot(aes(n, reorder(palabra, n))) +
  geom_col(show.legend = F, fill = "Green2", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de palabras Santos", y= "Palabra", x="Frecuencia")

p7 <- freq_terminos %>%
  group_by(autor) %>% 
  filter(autor == "Duque") %>%
  filter(row_number()<=10) %>% 
  ggplot(aes(n, reorder(palabra, n))) +
  geom_col(show.legend = F, fill = "Blue2", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de palabras Duque", y= "Palabra", x="Frecuencia")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol = 2)


freq_terminos %>%
  filter(autor == "Duque") %>%
  with(wordcloud(palabra, n, max.words = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale = c(3, 0.5))) %>% 
  title(main = "Duque", outer = T)

freq_terminos %>%
  filter(autor == "Petro") %>%
  with(wordcloud(palabra, n, max.words = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale = c(2, 0.5))) %>% 
  title(main = "Petro", outer = T)

freq_terminos %>%
  filter(autor == "Milei") %>%
  with(wordcloud(palabra, n, max.words = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale = c(2, 0.5))) %>% 
  title(main = "Milei", outer = T)

freq_terminos %>%
  filter(autor == "Cristina") %>%
  with(wordcloud(palabra, n, max.words = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale = c(2, 0.5))) %>% 
  title(main = "Cristina", outer = T)

freq_terminos %>%
  filter(autor == "Macri") %>%
  with(wordcloud(palabra, n, max.words = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale = c(2.5, 0.3)))%>% 
  title(main = "Macri", outer = T)

freq_terminos %>%
  filter(autor == "Fernandez") %>%
  with(wordcloud(palabra, n, max.words = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale = c(2, 0.4)))%>% 
  title(main = "Fernandez", outer = T)

freq_terminos %>%
  filter(autor == "Santos") %>%
  with(wordcloud(palabra, n, max.words = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale = c(2, 0.4)))%>% 
  title(main = "Santos", outer = T)

#tf-idf

discursos_tf_idf <- freq_terminos %>%
  bind_tf_idf(palabra, autor, n)

discursos_tf_idf %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

discursos_tf_idf %>%
  group_by(autor) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(palabra, tf_idf), fill = autor)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~autor, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# Bigramas

discursos_bigrams <- df_final %>%
  unnest_tokens(bigram, texto, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  count(bigram, autor, sort = TRUE)

freq_bigramas_autor <- discursos_bigrams %>% 
  group_by(autor) %>% 
  summarise(total = n())

freq_bigrams <- left_join(discursos_bigrams, freq_bigramas_autor)


b1 <- freq_bigrams %>%
  group_by(autor) %>% 
  filter(autor == "Petro") %>%
  filter(row_number()<=20) %>% 
  ggplot(aes(n, reorder(bigram, n))) +
  geom_col(show.legend = F, fill = "Yellow2", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de bigramas Petro", y= "Bigrama", x="Frecuencia")

b2 <- freq_bigrams %>%
  group_by(autor) %>%
  filter(autor == "Milei") %>%
  filter(row_number()<=20) %>% 
  ggplot(aes(n, reorder(bigram, n))) +
  geom_col(show.legend = F, fill = "Orange", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de bigramas Milei", y= "Bigrama", x="Frecuencia")

b3 <- freq_bigrams %>%
  group_by(autor) %>%
  filter(autor == "Macri") %>%
  filter(row_number()<=20) %>% 
  ggplot(aes(n, reorder(bigram, n))) +
  geom_col(show.legend = F, fill = "Light Blue", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de bigramas Macri", y= "Bigrama", x="Frecuencia")

b4 <- freq_bigrams %>%
  group_by(autor) %>%
  filter(autor == "Fernandez") %>%
  filter(row_number()<=20) %>% 
  ggplot(aes(n, reorder(bigram, n))) +
  geom_col(show.legend = F, fill = "Red2", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de bigramas Fernandez", y= "Bigrama", x="Frecuencia")

b5 <- freq_bigrams %>%
  group_by(autor) %>%
  filter(autor == "Cristina") %>%
  filter(row_number()<=20) %>% 
  ggplot(aes(n, reorder(bigram, n))) +
  geom_col(show.legend = F, fill = "Purple2", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de bigramas Cristina", y= "Bigrama", x="Frecuencia")

b6 <- freq_bigrams %>%
  group_by(autor) %>%
  filter(autor == "Santos") %>%
  filter(row_number()<=20) %>% 
  ggplot(aes(n, reorder(bigram, n))) +
  geom_col(show.legend = F, fill = "Green2", colour = "Dark Grey") +
  theme_minimal() +
  labs(title= "Frecuencia de bigramas Santos", y= "Bigrama", x="Frecuencia")


grid.arrange(b1, b2, b3, b4, b5, b6, ncol = 2)

#tf-idf bigramas

discursos_tf_idf_bigram <- freq_bigrams %>%
  bind_tf_idf(bigram, autor, n)

discursos_tf_idf_bigram %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

discursos_tf_idf_bigram %>%
  group_by(autor) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = autor)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~autor, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#red semántica

grafica_Petro <- freq_bigrams %>%
  filter(n > 100) %>%
  #filter(autor == "Petro") %>% 
  graph_from_data_frame(directed = F)

grafica_Petro <- igraph::simplify(grafica_Petro)

ggraph(grafica_Petro, layout = "stress") + 
  geom_edge_link(angle_calc = "along", label_dodge = unit(2, 'mm'), arrow = arrow(length = unit(3, 'mm')), end_cap = circle(3, 'mm'), linejoin = "round", label_colour = "Green", label_alpha = 1) +
  geom_node_point(size = 1) +
  geom_node_text(aes(label = name), vjust = 0.7, hjust = 1.1, size = 2)


# Topic modeling

# 1. Tokenización y limpieza
tokens <- df_final %>%
  #mutate(doc_id = row_number()) %>%   # ID único por discurso
  unnest_tokens(word, texto) %>%
  # quitar stopwords en español
  filter(!word %in% sw) %>%
  # quitar números y palabras muy cortas
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(str_length(word) > 2)

# 3. Construir Document-Term Matrix
dtm <- tokens %>%
  count(autor, word) %>%
  cast_dtm(document = autor, term = word, value = n)

# 4. Modelo LDA (ejemplo con 4 tópicos, ajusta k según necesidad)
set.seed(123)
lda_model <- LDA(dtm, k = 6, control = list(seed = 123))

# 5. Palabras más representativas por tópico
topics <- tidy(lda_model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  arrange(topic, -beta)

# 6. Graficar
ggplot(top_terms, aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Términos más representativos por tema. Presidentes de Argentina y Colombia",
       x = "Término", y = "Probabilidad (β)")

# 7. (Opcional) Ver a qué tema pertenece cada discurso
doc_topics <- tidy(lda_model, matrix = "gamma")
doc_topics %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  arrange(as.integer(document)) %>%
  left_join(discursos %>% mutate(doc_id = row_number()), by = c("document" = "doc_id")) %>%
  select(fecha, autor, topic) %>%
  head(10)

xMacri <- freq_bigrams %>% 
  filter(autor == "Macri") %>% 
  filter(row_number()<=500)

# Bigramas de búsqueda
bigrams_izquierda <- c("desarrollo sostenible", "bienestar social", "desigualdad social", "inversión pública", "justicia social", "redistribucion riqueza", "Estado presente", "inclusion social", "Estado bienestar", "Derechos sociales", "Intervencion estatal", "seguridad economica", "interes publico", "fallas mercado", "soberanía económica", "gasto social")
bigrams_derecha <- c("eficiencia mercado", "libertad economica", "reduccion Estado", "libre mercado", "propiedad privada", "austeridad fiscal", "libre empresa", "Estado minimo", "privatizar empresas", "inversion extranjera", "reduccion impuestos", "estabilidad economica", "flexibilidad laboral")

# Procesar
resultado_Fernandez <- xFernandez %>%
  mutate(match = if_else(bigram %in% bigrams_derecha, 1, 0))

# Calcular métrica
total_matches <- sum(resultado_Fernandez$match)
total_bigrams_busqueda <- length(bigrams_derecha)
score_fernandez_dr <- total_matches/total_bigrams_busqueda

izq <- c(score_cristina_iz, score_duque_iz, score_fernandez_iz, score_macri, score_milei_iz, score_petro_iz, score_santos_iz)
der <- c(score_cristina_dr, score_duque_dr, score_fernandez_dr, score_macri_dr, score_milei_dr, score_petro_dr, score_santos_dr)
nom_presi <- c("Cristina", "Duque", "Fernandez", "Macri", "Milei", "Petro", "Santos")
dummy_tm <- tibble(nom_presi, izq, der)

write_excel_csv(dummy_tm, "C:/Users/vaare/Downloads/textos/Data/dummy.xls")


