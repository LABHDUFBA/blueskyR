# Limpar a memória
rm(list=ls())

# Bibliotecas
library(httr)
library(jsonlite)
library(readr)
library(lubridate)

# Definir as credenciais da API
bluesky_username <- Sys.getenv("BLUESKY_APP_USER")  # Defina seu nome de usuário do Bluesky nas variáveis de ambiente
bluesky_password <- Sys.getenv("BLUESKY_APP_PASS")  # Defina sua senha do Bluesky nas variáveis de ambiente

# Função para autenticar-se no Bluesky e obter o token
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
  
  content(response)$accessJwt
}

# Função para buscar posts com um termo na rede Bluesky, usando paginação
search_term_in_network <- function(term, token, limit = 100) {
  url <- "https://bsky.social/xrpc/app.bsky.feed.searchPosts"
  
  posts <- list()
  cursor <- NULL
  
  repeat {
    # Parâmetros de consulta
    params <- list(
      q = term,  # Termo de busca
      limit = limit  # Limite de resultados por página (máximo permitido é 100)
    )
    
    if (!is.null(cursor)) {
      params$cursor <- cursor  # Adicionar paginação
    }
    
    # Enviar solicitação à API
    response <- tryCatch({
      GET(url, add_headers(Authorization = paste("Bearer", token)), query = params)
    }, error = function(e) {
      warning("Erro ao enviar solicitação para a API:", conditionMessage(e))
      return(NULL)
    })
    
    # Verificar se a resposta é válida
    if (is.null(response) || response$status_code != 200) {
      warning("Erro ao buscar posts com o termo:", term, "Código de status:", response$status_code)
      break
    }
    
    # Extrair dados da resposta
    data <- content(response, as = "parsed")
    
    if (is.null(data$posts)) {
      break  # Interromper se não houver mais posts
    }
    
    # Armazenar os posts obtidos
    posts <- append(posts, data$posts)
    
    # Atualizar o cursor para a próxima página
    cursor <- data$cursor
    
    # Interromper se não houver mais páginas
    if (is.null(cursor)) {
      break
    }
  }
  
  return(posts)
}

# Função para converter posts em data.frame
convert_posts_to_df <- function(posts_data) {
  posts_df <- do.call(rbind, lapply(posts_data, function(post) {
    data.frame(
      uri = ifelse(!is.null(post$uri), post$uri, NA),
      cid = ifelse(!is.null(post$cid), post$cid, NA),
      text = ifelse(!is.null(post$record$text), post$record$text, NA),
      created_at = ifelse(!is.null(post$record$createdAt), post$record$createdAt, NA),
      reply_count = ifelse(!is.null(post$replyCount), post$replyCount, NA),
      repost_count = ifelse(!is.null(post$repostCount), post$repostCount, NA),
      like_count = ifelse(!is.null(post$likeCount), post$likeCount, NA),
      author_handle = ifelse(!is.null(post$author$handle), post$author$handle, NA),
      author_display_name = ifelse(!is.null(post$author$displayName), post$author$displayName, NA),
      author_avatar = ifelse(!is.null(post$author$avatar), post$author$avatar, NA),
      author_did = ifelse(!is.null(post$author$did), post$author$did, NA),
      stringsAsFactors = FALSE
    )
  }))
  return(posts_df)
}

# Função para salvar o data.frame em CSV com o nome formatado
save_posts_to_csv <- function(posts_df, search_term) {
  # Obter a data atual
  current_date <- format(Sys.Date(), "%Y_%m_%d")
  
  # Formatar o nome do arquivo
  file_name <- paste0("data/", search_term, "_", current_date, ".csv")
  
  # Salvar o data.frame em CSV
  write_csv(posts_df, file_name)
  cat("Arquivo salvo em:", file_name, "\n")
}

# Termo para buscar
search_term <- "esquerdista"  # Altere para o termo que deseja buscar

# Autenticar e obter o token
token <- authenticate_bluesky(bluesky_username, bluesky_password)

# Buscar posts com o termo em toda a rede, até obter o máximo de resultados
posts_data <- search_term_in_network(search_term, token)

# Verificar se posts foram retornados e converter para data.frame
if (!is.null(posts_data) && length(posts_data) > 0) {
  posts_df <- convert_posts_to_df(posts_data)
  save_posts_to_csv(posts_df, search_term)
} else {
  cat("Nenhum post encontrado com o termo:", search_term, "\n")
}

# Suas raspagens estarão na pasta data com o nome do termo de busca 
# e a data de coleta (AAAA_MM_DD)