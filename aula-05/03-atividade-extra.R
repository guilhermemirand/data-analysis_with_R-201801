#' ATIVIDADE
#' 1. Estude o material abaixo que explica a construção de histogramas
#'   • http://flowingdata.com/2017/06/07/how-histograms-work/
#'   • http://tinlizzie.org/histograms/
#' 2. Estude o help da função geom_histogram
#' 3. Crie um histograma da quantidade de visualizações multifacetado por ano de publicação, restrito aos
#' anos entre 2012 e 2017.
#' O resultado desta atividade deve ser um arquivo chamado “03-atividade-extra.R” dentro do diretório aula-05.
#' O script em R deve carregar em um Data Frame o conteúdo do arquivo de dados das TED Talks e criar os
#' histogramas de forma multifacetada, conforme apresentado neste material de aula.
#' Você deve publicar o arquivo .R no Github para avaliação.

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, out.width = "600px", out.height="400px")
Sys.setlocale("LC_ALL", "pt_BR")

#' 
## ------------------------------------------------------------------------
# bibliotecas utilizadas
if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")
if (!"ggcorrplot" %in% installed.packages()) install.packages("ggcorrplot")

library(tidyverse)
library(lubridate)
library(magrittr)
library(Hmisc)


ted_talks <- read_csv("aula-05/data/ted_main.csv.gz") %>%
  mutate( duration  = duration(duration, units = "seconds")
          , film_date = as_datetime(film_date) %>% as_date()
          , published_date = as_datetime(published_date)) %>%
  mutate( event = factor(event)
          , speaker_occupation = factor(speaker_occupation)) %>%
  select(title, views, comments, duration:main_speaker, num_speaker:published_date, speaker_occupation)


ted_talks %>%
  mutate( year = year( published_date )) %>%
  filter(year >= 2012 & year <= 2017) %>%
  ggplot(aes( x = year )) +
  geom_histogram(aes(weight = views/1000000), binwidth = 0.5) +
  scale_x_continuous( breaks = 2012:2017 ) +
  scale_y_continuous( breaks = seq(from = 0, to = 550, by = 50 )) +
  #scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark=",", scientific = FALSE)) +
  ylab("visualizações (em milhões)") +
  xlab("Ano de publicação") +
  theme_bw()

#' FIM ATIVIDADE