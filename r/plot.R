# Carregar as bibliotecas necessárias
library(ggplot2)
library(dplyr)
library(readr)

# Função para calcular as porcentagens de violência e sem violência para cada arquivo
calculate_violence_percentage <- function(file_path) {
  # Carregar o arquivo classificado
  replies_df <- read_csv(file_path)
  
  # Contar o número de mensagens com e sem violência
  violence_counts <- replies_df %>%
    group_by(violence_detected) %>%
    summarise(count = n())
  
  # Calcular a porcentagem
  violence_percentage <- violence_counts %>%
    mutate(percentage = count / sum(count) * 100)
  
  # Adicionar o nome do arquivo como coluna
  violence_percentage <- violence_percentage %>%
    mutate(file = basename(file_path))
  
  return(violence_percentage)
}

# Processar todos os arquivos classificados e calcular as porcentagens
process_files_for_plot <- function(directory_path) {
  # Listar todos os arquivos classificados que seguem o padrão "_classified.csv"
  files <- list.files(directory_path, pattern = "_classified.csv", full.names = TRUE)
  
  # Aplicar a função para cada arquivo e combinar os resultados
  violence_data <- bind_rows(lapply(files, calculate_violence_percentage))
  
  return(violence_data)
}

# Diretório contendo os arquivos classificados
directory_path <- "data/"

# Obter os dados de porcentagem de todos os arquivos
violence_data <- process_files_for_plot(directory_path)

# Plotar o gráfico de barras empilhadas horizontais
ggplot(violence_data, aes(x = file, y = percentage, fill = violence_detected)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Porcentagem de Mensagens com e sem Violência por Arquivo",
       x = "Arquivo",
       y = "Proporção",
       fill = "Classificação de Violência") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +  # Girar o gráfico para barras horizontais
  theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Melhorar a legibilidade do eixo Y (nomes dos arquivos)
