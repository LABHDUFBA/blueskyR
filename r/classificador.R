# Instalar pacotes R
install.packages("reticulate")

# Carregar o pacote reticulate para conectar com Python
library(reticulate)
# Carregar a biblioteca necessária para leitura e gravação de CSV
library(readr)

# Importar bibliotecas necessárias do Python via reticulate
transformers <- import("transformers")
torch <- import("torch")

# Carregar o modelo pré-treinado de classificação de toxicidade/violência
model_name <- "unitary/toxic-bert"  # Modelo de classificação de toxicidade
tokenizer <- transformers$AutoTokenizer$from_pretrained(model_name)
model <- transformers$AutoModelForSequenceClassification$from_pretrained(model_name)

# Função para carregar um arquivo CSV de respostas
load_replies <- function(file_path) {
  replies_df <- read_csv(file_path)
  return(replies_df)
}

# Função para salvar o data.frame com os resultados classificados
save_classified_replies <- function(replies_df, file_path) {
  output_file <- sub(".csv", "_classified.csv", file_path)
  write_csv(replies_df, output_file)
  cat("Resultados salvos em:", output_file, "\n")
}

# Função para classificar as respostas usando um modelo de detecção de violência (previamente configurado)
classify_response <- function(text) {
  # Verifica se o texto contém as palavras relacionadas a violência ou ódio
  if (grepl("violência|agressão|ódio", text, ignore.case = TRUE)) {
    return("violência/ódio detectado")  # Retorna a string se for detectada violência ou ódio
  } else {
    return("sem violência detectada")  # Retorna essa string se não for detectada violência
  }
}

# Função para classificar todas as respostas de um data.frame
classify_responses <- function(replies_df) {
  replies_df$violence_detected <- sapply(replies_df$text, classify_response)
  return(replies_df)
}

# Função para processar todos os arquivos que seguem o padrão "_replies_"
process_matching_files <- function(directory_path) {
  # Listar todos os arquivos que correspondem ao padrão "_replies_"
  files <- list.files(directory_path, pattern = "_replies_", full.names = TRUE)
  
  for (file in files) {
    cat("Processando:", file, "\n")
    
    # Carregar as respostas do arquivo
    replies_df <- load_replies(file)
    
    # Aplicar a classificação de violência/toxicidade
    replies_with_classification <- classify_responses(replies_df)
    
    # Salvar o arquivo classificado
    save_classified_replies(replies_with_classification, file)
  }
}

# Chamar a função para processar todos os arquivos que seguem o padrão
process_matching_files("data/")





