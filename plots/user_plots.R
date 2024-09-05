# Limpar a memória
rm(list=ls())

# Bibliotecas necessárias
library(ggplot2)
library(readr)
library(dplyr)

# Carregar o arquivo CSV
file_path <- "./data/brazil_2024_09_05.csv"  # Substitua pelo nome correto do arquivo
posts_df <- read_csv(file_path)

# Verifique os dados carregados
head(posts_df)

# Converter a coluna de data para o formato correto
posts_df$created_at <- as.POSIXct(posts_df$created_at, format="%Y-%m-%dT%H:%M:%S", tz="UTC")

# Gráfico de curtidas ao longo do tempo
p1 <- ggplot(posts_df, aes(x = created_at, y = like_count)) +
  geom_line(color = "blue") +
  labs(title = "Curtidas ao longo do tempo de skeets com termo Brazil",
       x = "Data da Postagem",
       y = "Número de Curtidas") +
  theme_minimal()

# Gráfico de repostagens ao longo do tempo
p2 <- ggplot(posts_df, aes(x = created_at, y = repost_count)) +
  geom_line(color = "green") +
  labs(title = "Repostagens ao longo do tempo de skeets com termo Brazil",
       x = "Data da Postagem",
       y = "Número de Repostagens") +
  theme_minimal()

# Gráfico de respostas ao longo do tempo
p3 <- ggplot(posts_df, aes(x = created_at, y = reply_count)) +
  geom_line(color = "red") +
  labs(title = "Respostas ao longo do tempo de skeets com termo Brazil",
       x = "",
       y = "Número de Respostas") +
  theme_minimal()

# Gráfico de distribuição de curtidas (like_count)
p4 <- ggplot(posts_df, aes(x = like_count)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  scale_x_log10() +
  labs(title = "Distribuição de Curtidas (Like Count)", x = "Curtidas", y = "Frequência")

# Gráfico de distribuição de repostagens (repost_count)
p5 <- ggplot(posts_df, aes(x = repost_count)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  scale_x_log10() +
  labs(title = "Distribuição de Repostagens (Repost Count)", x = "Repostagens", y = "Frequência")

# Gráfico de distribuição de respostas (reply_count) com escala logarítmica
p6 <- ggplot(posts_df, aes(x = reply_count)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  scale_x_log10() +
  labs(title = "Distribuição de Respostas (Reply Count) com Escala Logarítmica", 
       x = "Respostas (Escala Logarítmica)", y = "Frequência") +
  theme_minimal()

# Converter 'created_at' para data
posts_df <- posts_df %>%
  mutate(post_date = as.Date(created_at))

# Contagem de postagens por dia
posts_per_day <- posts_df %>%
  group_by(post_date) %>%
  summarise(total_posts = n())

# Gráfico de postagens ao longo do tempo
p7 <- ggplot(posts_per_day, aes(x = post_date, y = total_posts)) +
  geom_line(color = "blue") +
  labs(title = "Postagens ao Longo do Tempo", x = "Data", y = "Número de Postagens")

# save plot

ggsave("./plots/p1.jpeg", plot = p1, width = 11, height = 06, device = "jpeg")
ggsave("./plots/p2.jpeg", plot = p2, width = 11, height = 06, device = "jpeg")
ggsave("./plots/p3.jpeg", plot = p3, width = 11, height = 06, device = "jpeg")
ggsave("./plots/p4.jpeg", plot = p4, width = 11, height = 06, device = "jpeg")
ggsave("./plots/p5.jpeg", plot = p5, width = 11, height = 06, device = "jpeg")
ggsave("./plots/p6.jpeg", plot = p6, width = 11, height = 06, device = "jpeg")
ggsave("./plots/p7.jpeg", plot = p7, width = 11, height = 06, device = "jpeg")




