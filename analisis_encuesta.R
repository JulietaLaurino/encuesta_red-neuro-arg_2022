library(tidyverse)
library(ggwordcloud)
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(wordcloud)
library(syuzhet)
library(magick)
library(cowplot)

# Cargo data----

## Cambio nombres columnas
data <- read.csv(here::here('datos.csv'), encoding = 'UTF-8')
demografia <- c('Timestamp',
                'asistencia_reunion',
                'edad',
                'genero',
                'diversidad',
                'ciudad_provincia',
                'posicion')
investigadores <-c('inv_publico_busqueda',
                   'inv_entrevistas_concretadas',
                   'inv_personas_tomadas',
                   'inv_interes',
                   'inv_impresiones')
estudiantes <- c('grado_carrera_universidad',
                 'grado_trabajo_neuro',
                 'grado_tiempo_trabajo_neuro',
                 'grado_ingreso_por_tesis',
                 'grado_intencion_doc',
                 'grado_motivos_baja_intencion_doc',
                 'grado_alternativa_baja_intencion_doc',
                 'grado_alternativa_alta_intencion_doc',
                 'grado_donde_alta_intencion_doc')
doctorandos <- c('doc_doctorado_universidad',
                 'doc_tipo_beca',
                 'doc_estadio',
                 'doc_intencion_posdoc',
                 'doc_motivos_baja_intencion_posdoc',
                 'doc_alternativa_baja_intencion_posdoc',
                 'doc_alternativa_alta_intencion_posdoc',
                 'doc_donde_alta_intencion_posdoc')
posdoctorandos <- c('posdoc_tipo_beca',
                    'posdoc_estadio',
                    'posdoc_intencion_inv',
                    'posdoc_motivos_baja_intencion_inv',
                    'posdoc_alternativa_baja_intencion_inv',
                    'posdoc_alternativa_alta_intencion_inv',
                    'posdoc_donde_alta_intencion_inv')
redes <- c('redes_futuro_profesional',
           'redes_pensamientos')
names(data) <- c(demografia, 
                 investigadores,
                 estudiantes,
                 doctorandos, 
                 posdoctorandos, 
                 redes)

## Cambio nombres respuestas
data$posicion <- factor(data$posicion, 
                        levels = unique(data$posicion),
                        labels =  c('Grado',
                                    'Doctorado',
                                    'Posdoc',
                                    'Investigadorx asistente/adjunto',
                                    'Investigadorx principal/superior',
                                    'Investigadorx independiente'
                                    )
)

data$genero <- factor(data$genero, 
                      levels = unique(data$genero),
                      labels =  c('Varón cis',
                                  'Mujer cis',
                                  'No responde',
                                  'Ninguna',
                                  'No binario'
                      )
)

data$diversidad <- factor(data$diversidad, 
                      levels = unique(data$diversidad),
                      labels =  c('No corresponde',
                                  '1° generación universitaria',
                                  '1° generación universitaria;LGBTIQ+',
                                  'LGBTIQ+',
                                  'LGBTIQ+;Afrodescendiente',
                                  'Dificultad sensorial/comunicacional',
                                  'LGBTIQ+;Dificultad sensorial/comunicacional',
                                  'Afrodescendiente',
                                  '1° generación universitaria;Indígena/descendiente de pueblos originarios',
                                  '1° generación universitaria;Afrodescendiente'
                      )
)

data$doc_intencion_posdoc <- factor(data$doc_intencion_posdoc, 
                                       levels = unique(data$doc_intencion_posdoc),
                                       labels =  c('',
                                                   'Algunas intenciones',
                                                   'Muchas intenciones',
                                                   'Certeza (sí)',
                                                   'Pocas intenciones',
                                                   'Certeza (no)'
                                       )
)

data$posdoc_intencion_inv <- factor(data$posdoc_intencion_inv, 
                                    levels = unique(data$posdoc_intencion_inv),
                                    labels =  c('',
                                                'Certeza (no)',
                                                'Algunas intenciones',
                                                'Certeza (sí)',
                                                'Pocas intenciones',
                                                'Muchas intenciones'
                                    )
)

data$doc_motivos_baja_intencion_posdoc <- factor(data$doc_motivos_baja_intencion_posdoc, 
                                   levels = unique(data$doc_motivos_baja_intencion_posdoc),
                                   labels =  c('',
                                               'Económicos;Relacionados al sist. científico;Personales',
                                               'Económicos;Relacionados al sist. científico',
                                               'Posible cambio de área;Económicos; Relacionados al sist. científico',
                                               'Posible cambio de área',
                                               'Relacionados al sist. científico',
                                               'Personales'
                                   )
)

data$posdoc_motivos_baja_intencion_inv <- factor(data$posdoc_motivos_baja_intencion_inv, 
                                                 levels = unique(data$posdoc_motivos_baja_intencion_inv),
                                                 labels =  c('',
                                                             'Económicos;Relacionados al sist. científico',
                                                             'Económicos',
                                                             'Posible cambio de área'
                                                 )
)

data$doc_donde_alta_intencion_posdoc <- factor(data$doc_donde_alta_intencion_posdoc, 
                                               levels = unique(data$doc_donde_alta_intencion_posdoc),
                                               labels =  c('',
                                                           'Argentina misma ciudad',
                                                           'Exterior',
                                                           'Exterior',
                                                           'Argentina otra ciudad'
                                               )
)

data$posdoc_donde_alta_intencion_inv <- factor(data$posdoc_donde_alta_intencion_inv, 
                                                 levels = unique(data$posdoc_donde_alta_intencion_inv),
                                                 labels =  c('',
                                                             'Exterior',
                                                             'Argentina misma ciudad',
                                                             'Argentina otra ciudad'
                                                 )
)


# Demografía----

## Corrijo ciudad y provincia
data$ciudad_provincia <- gsub('[C/c]aba|CABA, BA|CABA, Bs As|Ciudad de Buenos Aires', 'CABA', data$ciudad_provincia)
data$ciudad_provincia <- gsub('Cordoba|Cordoca', 'Córdoba', data$ciudad_provincia)
data$ciudad_provincia <- gsub('\\sy\\s|/', ',', data$ciudad_provincia)

data <- data %>% 
  separate(ciudad_provincia, c('ciudad', 'provincia'),',')
data$provincia <- str_trim(data$provincia)
data$ciudad <- str_trim(data$ciudad)
data$provincia <- str_remove(data$provincia, '\\.')
data$provincia <- gsub('^Buenos [A/a]ires$|^Bs As$|^Pcia. de Buenos Aires$|^Pcia. Buenos Aires$|^Gran Buenos Aires$|^Gran Bs. As.$', 'Provincia de Buenos Aires', data$provincia)
data$ciudad <- gsub('^Buenos [A/a]ires$|^Bs As$|^Pcia. de Buenos Aires$|^Pcia. Buenos Aires$|^Gran Buenos Aires$|^Gran Bs. As.$', 'Provincia de Buenos Aires', data$ciudad)
data$provincia <- gsub('Río', 'Rio', data$provincia)
data <- data %>% 
  mutate(provincia_2 = ifelse(ciudad == 'Provincia de Buenos Aires' ,
                              'Provincia de Buenos Aires', 
                              provincia)) %>% 
  mutate(ciudad = ifelse(ciudad == 'Provincia de Buenos Aires',
                         provincia, 
                         ciudad)) %>% 
  select(-provincia) %>% 
  rename('provincia' = provincia_2) %>% 
  filter(is.na(ciudad) | (ciudad != 'Montevideo' & 
           ciudad != 'Santiago de Chile' & 
           ciudad != 'San Diego'))

demografia <- c('Timestamp',
                'asistencia_reunion',
                'edad',
                'genero',
                'diversidad',
                'ciudad',
                'provincia',
                'posicion')

# Gráficos----

posicion_n <- data %>% count(posicion)
img <- image_read("./logo.png")

## Genero
p_genero <- data %>%
  mutate(genero = fct_relevel(genero, 
                              "No responde", 
                              "Ninguna", 
                              "No binario", 
                              "Varón cis", 
                              "Mujer cis")) %>%
  ggplot(aes(genero)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), 
           fill = 'blue4',
           width = 0.6) +
  coord_flip(ylim = c(0, 0.65))+
  labs(x = "", y = "Frecuencia relativa",
       title ="Demografía",
       subtitle = "Género") +
  geom_text(stat='count', 
            aes(label=..count..,
                y = (..count..)/sum(..count..)), 
            hjust=-1,
            size = 3) +
  theme_classic() +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15,
                                     margin = margin(0.1,0,0.8,0, "cm")),
        plot.margin = margin(0.5, 0, 0, 0, "cm")) +
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.2))

p_genero

ggdraw() +
  draw_plot(p_genero)+
  draw_image(img,x = 0.84, y = 0.8, width = 0.18, height = 0.18) 

ggsave('./figuras/genero.png', width = 7, height = 5)

## Diversidad
p_diversidad <- data %>%
  separate(diversidad, into = c('diversidad1', 'diversidad2'), sep = ';') %>% 
  pivot_longer(names_to = 'n_diversidad', 
               values_to = 'diversidad', 
               cols = c('diversidad1', 'diversidad2')) %>% 
  select(-n_diversidad) %>% 
  filter(complete.cases(diversidad)) %>% 
  mutate(diversidad = fct_relevel(diversidad, 
                                  'No corresponde',
                                  'LGBTIQ+',
                                  '1° generación universitaria',
                                  'Afrodescendiente',
                                  'Indígena/descendiente de pueblos originarios',
                                  'Dificultad sensorial/comunicacional'
                                  )) %>%
  ggplot(aes(diversidad)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), 
           fill = 'blue4',
           width = 0.6) +
  coord_flip(ylim = c(0, 0.8))+
  labs(x = "", y = "Frecuencia relativa",
       title ="Demografía",
       subtitle = "Diversidades y minorías")+
  geom_text(stat='count', 
            aes(label=..count..,
                y = (..count..)/sum(..count..)), 
            hjust=-1,
            size = 3) +
  theme_classic() +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15,
                                     margin = margin(0.1,0,0.8,0, "cm")),
        plot.margin = margin(0.5, 0, 0, 0, "cm")) +
  scale_x_discrete(labels=c('No corresponde',
                            'LGBTIQ+',
                            '1° generación universitaria' = '1° generación\nuniversitaria',
                            'Afrodescendiente',
                            'Indígena/descendiente de pueblos originarios' = 'Indígena/descendiente\nde pueblos originarios',
                            'Dificultad sensorial/comunicacional' = 'Dificultad sensorial/\ncomunicacional')) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2))

p_diversidad

ggdraw() +
  draw_plot(p_diversidad)+
  draw_image(img,x = 0.84, y = 0.8, width = 0.18, height = 0.18) 

ggsave('./figuras/diversidad.png', width = 7, height = 5)


## Intencion de seguir
data_intencion <- data %>%
  pivot_longer(names_to = 'intencion_posicion', 
               values_to = 'intencion',
               cols = c(doc_intencion_posdoc,
                        posdoc_intencion_inv)) %>% 
  filter(intencion != '') %>% 
  mutate(intencion = fct_relevel(intencion, 
                              "Certeza (no)", 
                              "Pocas intenciones", 
                              "Algunas intenciones", 
                              "Muchas intenciones", 
                              "Certeza (sí)"))

n_intencion <- data_intencion %>% 
  count(intencion_posicion)

p_intencion <- data_intencion %>% 
  ggplot(aes(intencion, fill = intencion_posicion)) +
  geom_bar(aes(y = ..prop..,
               group = intencion_posicion),
           position = position_dodge2(preserve = 'single',
                                      padding = 0),
           width = 0.6) +
  coord_flip(ylim = c(0, 0.6))+
  labs(x = "", y = "Frecuencia relativa",
       title ="Continuidad en la academia",
       subtitle = '(en área neurociencias)') +
  geom_text(stat='count', 
            aes(label=..count..,
                y = ..prop..,
                group = intencion_posicion), 
            hjust=-1,
            position = position_dodge(width = 0.6),
            size = 3) +
  theme_classic() +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15,
                                     margin = margin(0.1,0,0.8,0, "cm")),
        plot.margin = margin(0.5, 0, 0, 0, "cm"),
        legend.title = NULL) +
  scale_fill_manual(name = '',
                    labels = c(paste0('Doctorado\n(n = ', 
                                     n_intencion[n_intencion$intencion_posicion == "doc_intencion_posdoc",]$n, ')'), 
                               paste0('Posdoc\n(n = ', 
                                      n_intencion[n_intencion$intencion_posicion == "posdoc_intencion_inv",]$n, ')')),
                    values = c("#E6AB02", "#D95F02"))+
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.2))

p_intencion

ggdraw() +
  draw_plot(p_intencion)+
  draw_image(img,x = 0.84, y = 0.8, width = 0.18, height = 0.18) 

ggsave('./figuras/intencion.png', width = 7, height = 5)

## Motivos de discontinuidad
data_motivo <- data %>%
  pivot_longer(names_to = 'motivo_posicion', 
               values_to = 'motivo',
               cols = c(doc_motivos_baja_intencion_posdoc,
                        posdoc_motivos_baja_intencion_inv)) %>% 
  filter(motivo != '') %>%
  separate(motivo, into = c('motivo1', 'motivo2'), sep = ';') %>% 
  pivot_longer(names_to = 'n_motivo', 
               values_to = 'motivo', 
               cols = c('motivo1', 'motivo2')) %>% 
  select(-n_motivo) %>% 
  filter(complete.cases(motivo)) 
  # mutate(motivos = fct_relevel(intencion, 
  #                                "Certeza (no)", 
  #                                "Pocas intenciones", 
  #                                "Algunas intenciones", 
  #                                "Muchas intenciones", 
  #                                "Certeza (sí)")) 

n_motivo <- data_motivo %>% 
  count(motivo_posicion)

p_motivo <- data_motivo %>% 
  ggplot(aes(motivo, fill = motivo_posicion)) +
  geom_bar(aes(y = ..prop.., 
               group = motivo_posicion),
           position = position_dodge2(preserve = 'single',
                                      padding = 0),
           width = 0.6) +
  coord_flip(ylim = c(0, 0.65))+
  labs(x = "", y = "Frecuencia relativa",
       title ="Motivos de discontinuidad") +
  geom_text(stat='count', 
            aes(label=..count..,
                y = ..prop..,
                group = motivo_posicion), 
            hjust=-1,
            position = position_dodge2(width = 0.6),
            size = 3) +
  theme_classic() +
  theme(plot.title = element_text(size = 20,
                                  margin = margin(0,0,0.8,0, "cm")),
        plot.margin = margin(0.5, 0, 0, 0, "cm"),
        legend.title = NULL)  +
  scale_fill_manual(name = '',
                    labels = c(paste0('Doctorado\n(n = ', 
                                      n_motivo[n_motivo$motivo_posicion == "doc_motivos_baja_intencion_posdoc",]$n, ')'), 
                               paste0('Posdoc\n(n = ', 
                                      n_motivo[n_motivo$motivo_posicion == "posdoc_motivos_baja_intencion_inv",]$n, ')')),
                    values = c("#E6AB02", "#D95F02"))+
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.2))

p_motivo

ggdraw() +
  draw_plot(p_motivo)+
  draw_image(img,x = 0.84, y = 0.8, width = 0.18, height = 0.18) 

ggsave('./figuras/motivos.png', width = 7, height = 5)

## Dónde seguir
data_donde <- data %>%
  pivot_longer(names_to = 'donde_posicion', 
               values_to = 'donde',
               cols = c(doc_donde_alta_intencion_posdoc,
                        posdoc_donde_alta_intencion_inv)) %>% 
  filter(donde != '') %>% 
  mutate(donde = fct_relevel(donde,
                                 "Argentina misma ciudad",
                                 "Argentina otra ciudad",
                                 "Exterior")) 
n_donde <- data_donde %>% 
  count(donde_posicion)

p_donde <- data_donde %>% 
  ggplot(aes(donde, fill = donde_posicion)) +
  geom_bar(aes(y = ..prop.., 
               group = donde_posicion),
           position = position_dodge2(preserve = 'single',
                                      padding = 0),
           width = 0.6) +
  coord_flip(ylim = c(0, 0.8))+
  labs(x = "", y = "Frecuencia relativa",
       title ="Proyección")+
  geom_text(stat='count', 
            aes(label=..count..,
                y = ..prop..,
                group = donde_posicion), 
            hjust=-1,
            position = position_dodge(width = 0.6),
            size = 3) +
  theme_classic() +
  theme(plot.title = element_text(size = 20,
                                  margin = margin(0.1,0,0.8,0, "cm")),
        plot.margin = margin(0.5, 0, 0, 0, "cm"),
        legend.title = NULL) +
  scale_fill_manual(name = '',
                    labels = c(paste0('Doctorado\n(n = ', 
                                      n_donde[n_donde$donde_posicion == "doc_donde_alta_intencion_posdoc",]$n, ')'), 
                               paste0('Posdoc\n(n = ', 
                                      n_donde[n_donde$donde_posicion == "posdoc_donde_alta_intencion_inv",]$n, ')')),
                    values = c("#E6AB02", "#D95F02")) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2))

p_donde

ggdraw() +
  draw_plot(p_donde)+
  draw_image(img,x = 0.84, y = 0.8, width = 0.18, height = 0.18) 

ggsave('./figuras/donde.png', width = 7, height = 5)

## Alternativas
data_alternativas <- data %>%
  pivot_longer(names_to = 'alternativas_posicion', 
               values_to = 'alternativas',
               cols = c(doc_alternativa_baja_intencion_posdoc,
                        posdoc_alternativa_baja_intencion_inv)) %>% 
  filter(alternativas != '') %>%
  separate(alternativas, into = c('alternativas1', 'alternativas2'), sep = ';') %>% 
  pivot_longer(names_to = 'n_alternativas', 
               values_to = 'alternativas', 
               cols = c('alternativas1', 'alternativas2')) %>% 
  select(-n_alternativas) %>% 
  mutate(alternativas = ifelse(alternativas == 'Ingresar a carrera científica en un área distinta' |
                                 alternativas == "Realizar un posdoctorado en un área distinta" |
                                 alternativas == 'Ejercer mi profesion como psicologa clinica ' |
                                 alternativas == "En este momento no estoy considerando alternativas laborales" |
                                 alternativas == "Dedicarle tiempo a mi familia" |
                                 is.na(alternativas), 'Otras', alternativas)) %>% 
  mutate(alternativas = ifelse(str_detect(alternativas, "Tareas científicas"),                               
                               'Tareas científicas en\námbito público/privado', 
                               alternativas)) %>% 
  mutate(alternativas = fct_relevel(alternativas,
                             "Otras",
                             "Docencia",
                             "Comunicación científica",
                             "Tareas científicas en\námbito público/privado",
                             "Análisis de datos / programación"))

n_alternativas <- data_alternativas %>% 
  count(alternativas_posicion)

p_alternativas <- data_alternativas %>% 
  ggplot(aes(alternativas, fill = alternativas_posicion)) +
  geom_bar(aes(y = ..prop.., 
               group = alternativas_posicion),
           position = position_dodge2(preserve = 'single',
                                      padding = 0),
           width = 0.6) +
  coord_flip(ylim = c(0, 0.8))+
  labs(x = "", y = "Frecuencia relativa",
       title ="Alternativas")+
  geom_text(stat='count', 
            aes(label=..count..,
                y = ..prop..,
                group = alternativas_posicion), 
            hjust=-1,
            position = position_dodge(width = 0.6),
            size = 3) +
  theme_classic() +
  theme(plot.title = element_text(size = 20,
                                  margin = margin(0.1,0,0.8,0, "cm")),
        plot.margin = margin(0.5, 0, 0, 0, "cm"),
        legend.title = NULL) +
  scale_fill_manual(name = '',
                    labels = c(paste0('Doctorado\n(n = ', 
                                      n_alternativas[n_alternativas$alternativas_posicion == "doc_alternativa_baja_intencion_posdoc",]$n, ')'), 
                               paste0('Posdoc\n(n = ', 
                                      n_alternativas[n_alternativas$alternativas_posicion == "posdoc_alternativa_baja_intencion_inv",]$n, ')')),
                    values = c("#E6AB02", "#D95F02")) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2))

p_alternativas

ggdraw() +
  draw_plot(p_alternativas)+
  draw_image(img,x = 0.84, y = 0.8, width = 0.18, height = 0.18) 

ggsave('./figuras/alternativas.png', width = 7, height = 5)

# Redes----

translation <- read.csv('./translated.csv', sep = ";")
data_redes <- data %>% 
  select(all_of(c(demografia, redes))) 

instancias = unique(data_redes$posicion)

df = data.frame(emocion = NA,
                valor = NA,
                #dimension = NA,
                posicion = NA)

for(instancia in instancias){
  
  this_data_redes <- data_redes %>% 
    filter(posicion == instancia)
  
  texto_palabras <- get_tokens(this_data_redes$redes_pensamientos) 
  
  # Elimino palabras que no me interesan
  stop_words = read.table('stop_words_spanish.txt',
                          encoding = 'UTF-8') %>%
    rename('word' = V1)
  
  manually_remove_words = read.table('words_removed.txt',
                            encoding = 'UTF-8') %>%
    rename('word' = V1)
  
  removed_words = stop_words %>% 
    bind_rows(manually_remove_words)
  
  texto_palabras <- texto_palabras[!(texto_palabras %in% removed_words$word)]
  #texto_palabras_count <- texto_palabras %>% plyr::count() %>% arrange(-freq)
  
  this_sentimientos_df <- get_nrc_sentiment(texto_palabras, lang="spanish")
  
  names(this_sentimientos_df) <- c('enfado', 'anticipación',	'disgusto',	'miedo',	'alegría',	'tristeza',	'sorpresa',	'confianza',	'negativo',	'positivo')
  
  #this_sentimientos_df <- prop.table(this_sentimientos_df) 
  this_sentimientos_df$word = texto_palabras
  this_sentimientos_df <- this_sentimientos_df %>% 
    pivot_longer(names_to = 'emocion', values_to = 'valor', cols = -word)
  
  this_sentimientos_df = this_sentimientos_df %>% 
    mutate(posicion = instancia)
  df = df %>% bind_rows(this_sentimientos_df)
}

sentimientos_df = df %>% 
  filter(emocion != 'anticipación',
         emocion != 'sorpresa',
         emocion != 'disgusto',
         emocion != 'enfado') %>% 
  filter(posicion == 'Grado' | posicion == 'Doctorado' | posicion == 'Posdoc') %>% 
  mutate(dimension = ifelse(emocion == 'positivo' | emocion == 'negativo', 'valencia', 
                            ifelse(emocion == 'confianza' | emocion == 'miedo', 'seguridad', 
                                   'felicidad')))

sentimientos_df$emocion <- factor(sentimientos_df$emocion, levels=c('alegría', 'tristeza', 'confianza', 'miedo', 'positivo', 'negativo'))

sentimientos_df$posicion = factor(sentimientos_df$posicion, 
                                  levels = c('Grado', 
                                             'Doctorado',
                                             'Posdoc')) 
sentimientos_df = sentimientos_df %>% 
  mutate(n_pos = ifelse(posicion == 'Grado', 
                    sum(data$posicion == 'Grado'),
                    ifelse(posicion == 'Doctorado', 
                           sum(data$posicion == 'Doctorado'),
                           sum(data$posicion == 'Posdoc')))) %>% 
  drop_na()

## Grafico pals emotivas más frecuentes
sentimientos_df_count <- sentimientos_df %>% 
  filter(valor > 0) %>% 
  filter(emocion == 'positivo' | emocion == 'negativo') %>% 
  group_by(word, emocion) %>% 
  dplyr::summarise(n = dplyr::n(), 
                   valor = max(valor),
                   .groups = "drop") %>% 
  distinct(word, .keep_all = TRUE) %>% 
  mutate(valor_total = n*valor) %>% 
  left_join(translation)

sentimientos_df_plot <- sentimientos_df_count %>% 
  slice_max(valor_total, n = 50) %>%
  ggplot(aes(label = word, size = n, color = emocion, x = emocion)) +
  geom_text_wordcloud(seed = 150) +
  scale_size_area(max_size = 10) +
  scale_color_manual(values = c("#D95F02", 'blue4')) +
  scale_x_discrete(breaks = NULL) +
  theme_void() +
  theme(#panel.background = element_rect(fill = 'white', color = 'white'),
        plot.margin = margin(0.1, 0.4, 0.1, 0.4, "cm"),
        plot.background = element_rect(fill = 'white', color = 'white')
        )

sentimientos_df_plot

ggsave('./figuras/emociones_frecuentes.png', width = 2.5, height = 2)

# Grafico valor de cada variable emocional
sentimientos_plot <- ggplot(sentimientos_df, aes(emocion, valor, fill = dimension)) +
  geom_bar(position = "dodge", stat = "summary", fun = mean) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = .1,
    size = 0.2,
    color = 'black'
  ) +
  theme_classic(base_size = 6) +
  scale_fill_manual(values = brewer.pal(3,"Dark2"))+
  theme(legend.position = 'none',
        plot.margin = margin(1, 1.5, 1, 1.5, "cm")) +
  ylab('valor emocional')

ggdraw() +
  draw_plot(sentimientos_plot)+
  draw_image(img,x = 0.83, y = 0.8, width = 0.18, height = 0.18) 

ggsave('./figuras/sentiment_analysis.png', height = 3,
       width = 3.5)

## Grafico valor de cada variable emocional por posición
ggplot(sentimientos_df, aes(emocion, valor, fill = dimension)) +
  geom_bar(position = "dodge", stat = "summary", fun = mean) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = .1,
    size = 0.2,
    color = 'black'
  ) +
  #annotate("text", label = n)+
  theme_classic(base_size = 6) +
  theme(legend.position = 'none') +
  scale_fill_manual(values = brewer.pal(3,"Dark2"))+
  facet_wrap(~posicion, nrow=3, scales = 'free') +ylab('valor emocional')

ggsave('./figuras/sentiment_analysis_por_instancia.png', height = 4,
       width = 2)


# Grafico pals que más aporta a cada variable emocional

sentimientos_df_nonzero = sentimientos_df %>% filter(valor > 0) 

nube_emociones_vector <- c(
  # paste(sentimientos_df_nonzero[sentimientos_df_nonzero$emocion == 'tristeza', 4], collapse = " "),
  paste(sentimientos_df_nonzero[sentimientos_df_nonzero$emocion == 'negativo', 4], collapse = " "),
  paste(sentimientos_df_nonzero[sentimientos_df_nonzero$emocion == 'positivo', 4], collapse = " ")
  # paste(sentimientos_df_nonzero[sentimientos_df_nonzero$emocion == 'alegría', 4], collapse = " ")
)

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))

nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)

colnames(nube_tdm) <- c('negativo', 'positivo')
set.seed(730) # puede ser cualquier número
comparison.cloud(nube_tdm, 
                 random.order = FALSE,
                 scale = c(3.3, .55),
                 title.size = 1,
                 rot.per = 0)







