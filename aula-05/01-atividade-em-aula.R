# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)
library(tibble)
library(lubridate)



# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
ted_main <- read.csv("aula-05/data/ted_main.csv.gz")



# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
summary(ted_main)




# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..
ted_main %>%
  mutate(duration = duration(duration, units = "seconds"),
         film_date = as_datetime(film_date), 
         published_date = as_datetime(published_date)) -> ted_main_final



# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

ted_main_final %>% 
  mutate(event = factor(event),
         speaker_occupation = factor(speaker_occupation)) -> ted_main_final




# Retire do dataframe a variável name

ted_main_final %>% 
  select(-name)


# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas

summary(ted_main_final)


# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.

ted_main_final %>% 
  filter(languages <= 0) %>%
  mutate(languages = 1) %>%
  select(languages)
  
  


# Verifique os 15 registros com menor data de filmagem. 
ted_main_final %>%
  arrange(film_date) %>%
  head(15) %>%
  View()



# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
cont_apresentacoes <- 
  ted_main_final %>% 
  mutate(ano_filmagem = year(film_date)) %>%
  group_by(ano_filmagem) %>%
    summarise(qtd = n()) %>%
  ungroup()
  
cont_apresentacoes %>% View()



# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.

quarto_quantile <- quantile(cont_apresentacoes$ano_filmagem, probs = seq(0, 1, 0.1))[4]

ted_main_final %>%
  filter(year(film_date) > quarto_quantile) -> ted_main_final
  


# Verifique novamente o resumo dos dados do dataframe
summary(ted_main_final)



# Verifique os 10 registros com maior duração.
ted_main_final %>%
  arrange(desc(duration)) %>%
  head(10) %>% 
  View()



# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas
ted_main_final %>% 
  mutate(corte = mean(duration) + (3 * sd(duration))) %>%
  filter(as.numeric(duration) > corte) %>%
  View()



# Calcule os 4 quartis e o IQR da duração das apresentações. 
# Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil

terceiroQ <- quantile(as.numeric(ted_main_final$duration, "seconds"))[3]
iqrDuracao <- IQR(as.numeric(ted_main_final$duration, "seconds"))

ted_main_final %>%
  filter(as.numeric(duration, "seconds") > (1.5 * iqrDuracao + terceiroQ)) %>%
  View()

# Visualize os 10 quantis da quantidade de visualizações

quantile(ted_main_final$views, probs = seq(0, 1, 0.1))


# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?

media_views <- mean(ted_main_final$views)
mediana_views <- median(ted_main_final$views)
desv_padrao_views <- sd(ted_main_final$views)
desv_abs_med_views <- median(abs(ted_main_final$views - median(ted_main_final$views)))
iqr_views <- IQR(ted_main_final$views)

cat("A média (", media_views, ") é maior que a mediana (", mediana_views, ")")
cat("O desvio padrão (", desv_padrao_views, ") é maior que o desvio absoluto da mediana (", desv_abs_med_views, ")")
cat("O IQR é ", iqr_views/desv_abs_med_views, " vezes maior que o desvio absolto da mediana")
print("Há mais quantidades de visualizações que estão abaixo da média.")


# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações

dez_por_cento <- ted_main_final %>% count() * 0.1

ted_main_final %>% 
  arrange(views) %>%
  head(dez_por_cento) -> mais_visualizacoes

cat("Média de línguas dos mais visualizados: ", mean(mais_visualizacoes$languages)) 
cat("Desvio padrão de línguas dos mais visualizados: ", sd(mais_visualizacoes$languages))
cat("Mediana de línguas dos mais visualizados: ", median(mais_visualizacoes$languages))
cat("IQR de línguas dos mais visualizados: ", IQR(mais_visualizacoes$languages)) 


ted_main_final %>% 
  arrange(desc(views)) %>%
  head(dez_por_cento) -> menos_visualizacoes

cat("Média de línguas dos menos visualizados: ", mean(menos_visualizacoes$languages)) 
cat("Desvio padrão de línguas dos menos visualizados: ", sd(menos_visualizacoes$languages))
cat("Mediana de línguas dos menos visualizados: ", median(menos_visualizacoes$languages))
cat("IQR de línguas dos menos visualizados: ", IQR(menos_visualizacoes$languages)) 

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. 
# Utilize a função str_detect para este filtro

ted_main_final %>%
  group_by(event) %>%
  filter(str_detect(event, "TED.*")) %>%
  summarise(qtd = n()) %>%
  ungroup() %>%
  View()


# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior 
# que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES

ted_main_final %>%
  group_by(event) %>%
  filter(str_detect(event, "TED.*")) %>%
  filter(views > media_views) %>%
  summarise(qtd = n(), 
            ano = min(published_date), 
            media_linguas = mean(languages),
            desv_padrao_linguas = sd(languages),
            coef_var_linguas = sd(languages)/mean(languages)) %>%
  ungroup() %>%
  View()


# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas




# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado




