# Limpar a memória
rm(list=ls())

# Bibliotecas
library(httr)
library(jsonlite)
library(readr)
library(ggplot2)

# Definir as credenciais da API
bluesky_username <- Sys.getenv("BLUESKY_APP_USER")  # Defina seu nome de usuário do Bluesky nas variáveis de ambiente
bluesky_password <- Sys.getenv("BLUESKY_APP_PASS")  # Defina sua senha do Bluesky nas variáveis de ambiente

# Verificar se as variáveis de ambiente estão definidas
if (bluesky_username == "" || bluesky_password == "") {
  stop("Error
