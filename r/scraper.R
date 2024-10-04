# Limpar a memória
rm(list=ls())
gc()
# Bibliotecas
library(httr)
library(jsonlite)
library(readr)

# Definir as credenciais da API
bluesky_username <- Sys.getenv("BLUESKY_APP_USER")  # Defina seu nome de usuário do Bluesky nas variáveis de ambiente
bluesky_password <- Sys.getenv("BLUESKY_APP_PASS")  # Defina sua senha do Bluesky nas variáveis de ambiente

# Verificar se as variáveis de ambiente estão definidas
if (bluesky_username == "" || bluesky_password == "") {
  stop("Error: Variáveis de ambiente BLUESKY_APP_USER e BLUESKY_APP_PASS devem ser definidas.")
}

# Função para autenticar-se no Bluesky
authenticate_bluesky <- function(username, password) {
  url <- "https://bsky.social/xrpc/com.atproto.server.createSession"
  response <- POST(
    url,
    body = list(
      identifier = username,
      password = password
    ),
    encode = "json"
  )
  
  if (response$status_code != 200) {
    stop("Erro ao tentar fazer login no Bluesky. Verifique suas credenciais.")
  }
  
  content(response)$accessJwt  # Retorna o token de autenticação
}

# Autenticar-se e obter o token
token <- authenticate_bluesky(bluesky_username, bluesky_password)

# Função para obter os posts de um perfil com paginação
get_all_posts <- function(username, token) {
  posts <- list()
  cursor <- NULL
  repeat {
    url <- "https://bsky.social/xrpc/app.bsky.feed.getAuthorFeed"
    
    params <- list(
      actor = username,
      limit = 100  # O limite máximo de posts por requisição (ajuste conforme necessário)
    )
    
    if (!is.null(cursor)) {
      params$cursor <- cursor  # Adiciona a paginação
    }
    
    response <- GET(url, add_headers(Authorization = paste("Bearer", token)), query = params)
    
    if (response$status_code < 200 || response$status_code >= 300) {
      warning("Aviso: Código de status inesperado ao obter posts: ", response$status_code)
      break
    }
    
    data <- content(response, as = "parsed")
    
    if (is.null(data$feed)) {
      break  # Para se não houver mais posts
    }
    
    posts <- append(posts, data$feed)
    
    cursor <- data$cursor  # Atualiza o cursor para a próxima página
    
    if (is.null(cursor)) {
      break  # Para se não houver mais páginas
    }
  }
  
  return(posts)
}

# Função para obter o perfil de um usuário
get_profile_data <- function(username, token) {
  url <- paste0("https://bsky.social/xrpc/app.bsky.actor.getProfile?actor=", username)
  response <- GET(url, add_headers(Authorization = paste("Bearer", token)))
  
  if (response$status_code < 200 || response$status_code >= 300) {
    warning("Erro ao obter perfil: ", response$status_code)
    return(NULL)
  }
  
  return(content(response, as = "parsed"))
}

# Função para converter os dados dos posts em um data.frame
convert_posts_to_df <- function(posts_data) {
  posts_df <- do.call(rbind, lapply(posts_data, function(post) {
    data.frame(
      uri = post$post$uri,
      cid = post$post$cid,
      text = post$post$record$text,
      created_at = post$post$record$createdAt,
      reply_count = post$post$replyCount,
      repost_count = post$post$repostCount,
      like_count = post$post$likeCount,
      indexed_at = post$post$indexedAt,
      author_handle = post$post$author$handle,
      author_display_name = post$post$author$displayName,
      author_did = post$post$author$did,
      stringsAsFactors = FALSE
    )
  }))
  
  return(posts_df)
}

# Função para converter o perfil em data.frame
convert_profile_to_df <- function(profile_data) {
  profile_df <- data.frame(
    did = profile_data$did,
    handle = profile_data$handle,
    display_name = profile_data$displayName,
    description = profile_data$description,
    followers_count = profile_data$followersCount,
    follows_count = profile_data$followsCount,
    posts_count = profile_data$postsCount,
    created_at = profile_data$createdAt,
    indexed_at = profile_data$indexedAt,
    stringsAsFactors = FALSE
  )
  
  return(profile_df)
}

# Função para salvar os posts e perfil em CSV com o nome simples e a data de coleta
save_data_to_csv <- function(profile_df, posts_df, username) {
  # Extrair a primeira parte do nome de usuário
  simple_username <- strsplit(username, "\\.")[[1]][1]
  
  # Obter a data atual no formato AAAA_mm_dd
  current_date <- format(Sys.Date(), "%Y_%m_%d")
  
  # Definir os nomes dos arquivos
  profile_file <- paste0("./data/", simple_username, "_profile_", current_date, ".csv")
  posts_file <- paste0("./data/", simple_username, "_posts_", current_date, ".csv")
  
  # Salvar os arquivos CSV
  write_csv(profile_df, profile_file)
  write_csv(posts_df, posts_file)
  
  cat("Perfil salvo em", profile_file, "\n")
  cat("Postagens salvas em", posts_file, "\n")
}

#############################################################################
############################# FINAL DAS FUNÇÕES #############################
############################################################################

# Exemplo de uso: buscar perfil e posts e salvar como CSV

# Substitua pelo nome de usuário no Bluesky sem @
username <- "siueiris.bsky.social" # Exemplo de nome de usuário

# Obter dados do perfil e posts
profile_data <- get_profile_data(username, token)
posts_data <- get_all_posts(username, token)

# Verifique se os dados do perfil e posts foram obtidos corretamente
if (!is.null(profile_data) && length(posts_data) > 0) {
  cat("Dados obtidos com sucesso para:", username, "\n")
  
  # Converter perfil e posts para data.frames
  profile_df <- convert_profile_to_df(profile_data)
  posts_df <- convert_posts_to_df(posts_data)
  
  # Salvar os dados em arquivos CSV
  save_data_to_csv(profile_df, posts_df, username)
} else {
  cat("Erro ao obter perfil ou posts para o perfil:", username, "\n")
}
