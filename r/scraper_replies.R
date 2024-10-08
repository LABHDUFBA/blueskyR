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

# Função para obter respostas de uma postagem específica, incluindo a postagem original
get_post_replies <- function(post_uri, token) {
  url <- "https://bsky.social/xrpc/app.bsky.feed.getPostThread"
  
  params <- list(
    uri = post_uri  # O URI da postagem que você deseja obter respostas
  )
  
  response <- GET(url, add_headers(Authorization = paste("Bearer", token)), query = params)
  
  if (response$status_code < 200 || response$status_code >= 300) {
    warning("Erro ao obter respostas da postagem: ", response$status_code)
    return(NULL)
  }
  
  thread <- content(response, as = "parsed")$thread
  
  # Verificar se há respostas e se a postagem original existe
  if (is.null(thread) || is.null(thread$replies)) {
    return(NULL)
  }
  
  # Inclua o post original junto com as respostas
  return(list(original_post = thread$post, replies = thread$replies))
}

# Função para converter as respostas em um data.frame, incluindo o post original
convert_replies_to_df <- function(replies_data, original_post) {
  if (length(replies_data) == 0) {
    return(NULL)  # Se não houver respostas, retorne NULL
  }
  
  replies_df <- do.call(rbind, lapply(replies_data, function(reply) {
    # Verifica se as informações de reply e da postagem original existem antes de adicionar ao data frame
    data.frame(
      reply_uri = ifelse(!is.null(reply$post$uri), reply$post$uri, NA),
      reply_cid = ifelse(!is.null(reply$post$cid), reply$post$cid, NA),
      original_post_uri = ifelse(!is.null(original_post$uri), original_post$uri, NA),
      original_post_text = ifelse(!is.null(original_post$record$text), original_post$record$text, NA),
      original_post_author_handle = ifelse(!is.null(original_post$author$handle), original_post$author$handle, NA),  # Aqui incluímos o autor da postagem original
      reply_text = ifelse(!is.null(reply$post$record$text), reply$post$record$text, NA),
      reply_created_at = ifelse(!is.null(reply$post$record$createdAt), reply$post$record$createdAt, NA),
      reply_reply_count = ifelse(!is.null(reply$post$replyCount), reply$post$replyCount, NA),
      reply_repost_count = ifelse(!is.null(reply$post$repostCount), reply$post$repostCount, NA),
      reply_like_count = ifelse(!is.null(reply$post$likeCount), reply$post$likeCount, NA),
      reply_indexed_at = ifelse(!is.null(reply$post$indexedAt), reply$post$indexedAt, NA),
      reply_author_handle = ifelse(!is.null(reply$post$author$handle), reply$post$author$handle, NA),
      reply_author_display_name = ifelse(!is.null(reply$post$author$displayName), reply$post$author$displayName, NA),
      reply_author_did = ifelse(!is.null(reply$post$author$did), reply$post$author$did, NA), stringsAsFactors = FALSE
    )
  }))
  
  return(replies_df)
}



# Função para obter todas as respostas para as postagens
get_all_replies_for_posts <- function(posts_df, token) {
  all_replies <- list()
  
  for (i in 1:nrow(posts_df)) {
    post_uri <- posts_df$uri[i]
    replies_data <- get_post_replies(post_uri, token)
    
    if (!is.null(replies_data)) {
      original_post <- replies_data$original_post
      replies <- replies_data$replies
      if (!is.null(replies)) {
        replies_df <- convert_replies_to_df(replies, original_post)
        all_replies <- append(all_replies, list(replies_df))
      }
    }
  }
  
  # Combine todas as respostas em um único data.frame
  all_replies_df <- do.call(rbind, all_replies)
  return(all_replies_df)
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

# Função para salvar os posts, perfil e respostas em CSV com o nome simples e a data de coleta
save_data_to_csv <- function(profile_df, posts_df, replies_df, username) {
  # Extrair a primeira parte do nome de usuário
  simple_username <- strsplit(username, "\\.")[[1]][1]
  
  # Obter a data atual no formato AAAA_mm_dd
  current_date <- format(Sys.Date(), "%Y_%m_%d")
  
  # Definir os nomes dos arquivos
  profile_file <- paste0("./data/", simple_username, "_profile_", current_date, ".csv")
  posts_file <- paste0("./data/", simple_username, "_posts_", current_date, ".csv")
  replies_file <- paste0("./data/", simple_username, "_replies_", current_date, ".csv")
  
  # Verificar se os objetos são data.frames antes de salvar
  if (is.data.frame(profile_df)) {
    write_csv(profile_df, profile_file)
    cat("Perfil salvo em", profile_file, "\n")
  } else {
    cat("Erro: perfil_df não é um data.frame para", username, "\n")
  }
  
  if (is.data.frame(posts_df)) {
    write_csv(posts_df, posts_file)
    cat("Postagens salvas em", posts_file, "\n")
  } else {
    cat("Erro: posts_df não é um data.frame para", username, "\n")
  }
  
  if (is.data.frame(replies_df)) {
    write_csv(replies_df, replies_file)
    cat("Respostas salvas em", replies_file, "\n")
  } else {
    cat("Aviso: replies_df não é um data.frame para", username, "ou não houve respostas.\n")
  }
}

# Loop para coletar dados de cada perfil
for (username in usernames) {
  cat("Coletando dados para:", username, "\n")
  
  # Obter dados do perfil e posts
  profile_data <- get_profile_data(username, token)
  posts_data <- get_all_posts(username, token)
  
  # Verifique se os dados do perfil e posts foram obtidos corretamente
  if (!is.null(profile_data) && length(posts_data) > 0) {
    cat("Dados obtidos com sucesso para:", username, "\n")
    
    # Converter perfil e posts para data.frames
    profile_df <- convert_profile_to_df(profile_data)
    posts_df <- convert_posts_to_df(posts_data)
    
    # Obter respostas para os posts
    replies_data <- get_all_replies_for_posts(posts_df, token)
    
    # Verificar se houve erro no replies_data e continuar com o salvamento
    if (!is.null(replies_data)) {
      replies_df <- replies_data
    } else {
      cat("Aviso: Não foi possível obter respostas para", username, "\n")
      replies_df <- NULL
    }
    
    # Salvar os dados em arquivos CSV
    save_data_to_csv(profile_df, posts_df, replies_df, username)
  } else {
    cat("Erro ao obter perfil ou posts para o perfil:", username, "\n")
  }
}


#############################################################################
############################# FINAL DAS FUNÇÕES #############################
############################################################################

################################################################################
# Exemplo de uso: buscar UM PERFIL APENAS, posts e respostas e salvar como CSV
###############################################################################

# Substitua pelo nome de usuário no Bluesky sem @
username <- "alguem.bsky.social"  # Exemplo de nome de usuário

# Obter dados do perfil e posts
profile_data <- get_profile_data(username, token)
posts_data <- get_all_posts(username, token)

# Verifique se os dados do perfil e posts foram obtidos corretamente
if (!is.null(profile_data) && length(posts_data) > 0) {
  cat("Dados obtidos com sucesso para:", username, "\n")
  
  # Converter perfil e posts para data.frames
  profile_df <- convert_profile_to_df(profile_data)
  posts_df <- convert_posts_to_df(posts_data)
  
  # Obter respostas para os posts
  replies_data <- get_all_replies_for_posts(posts_df, token)
  
  # Salvar os dados em arquivos CSV
  save_data_to_csv(profile_df, posts_df, replies_data, username)
} else {
  cat("Erro ao obter perfil ou posts para o perfil:", username, "\n")
}

########################################################################
####################### SALVAR A LISTA DE PERFIS #######################
########################################################################

# Lista de URLs de perfis
profile_urls <- c(
  "https://bsky.app/profile/adrianaaccorsi.bsky.social",
  "https://bsky.app/profile/camilajarams.bsky.social",
  "https://bsky.app/profile/camilavaladaopsol.bsky.social",
  "https://bsky.app/profile/celiatavares13.bsky.social",
  "https://bsky.app/profile/todandara.bsky.social",
  "https://bsky.app/profile/daniportelapsol.bsky.social",
  "https://bsky.app/profile/dudasalabert.bsky.social"#,
  # "https://bsky.app/profile/lenildaluna.bsky.social",
  # "https://bsky.app/profile/mariadorosario.bsky.social",
  # "https://bsky.app/profile/marinahelenabr.com.br",
  # "https://bsky.app/profile/nataliabonavides.bsky.social",
  # "https://bsky.app/profile/tabatasp.bsky.social",
  # "https://bsky.app/profile/taliriapetrone.bsky.social",
  # "https://bsky.app/profile/annemouraam.bsky.social",
  # "https://bsky.app/profile/carlaayres.bsky.social",
  # "https://bsky.app/profile/eucarolguedes.bsky.social",
  # "https://bsky.app/profile/keitlimasp.bsky.social",
  # "https://bsky.app/profile/lunazarattini.bsky.social",
  # "https://bsky.app/profile/mariadoceurecife.bsky.social",
  # "https://bsky.app/profile/martamerepresenta.bsky.social",
  # "https://bsky.app/profile/profsoniameire.bsky.social",
  # "https://bsky.app/profile/vanessafacundes.bsky.social",
  # "https://bsky.app/profile/vivireispsol.bsky.social",
  # "https://bsky.app/profile/iza-lourenca.bsky.social",
  # "https://bsky.app/profile/bellagoncalves.com.br",
  # "https://bsky.app/profile/fabyareis.bsky.social",
  # "https://bsky.app/profile/martasuplicy.bsky.social",
  # "https://bsky.app/profile/amandapaschoal.bsky.social",
  # "https://bsky.app/profile/acarolinaiara.bsky.social",
  # "https://bsky.app/profile/deboralimamtst.bsky.social",
  # "https://bsky.app/profile/quilomboperiferico.bsky.social",
  # "https://bsky.app/profile/luanapsol.bsky.social",
  # #"https://bsky.app/profile/did:plc:5tu3m7c3etn6tv4zf7jhky5n",
  # "https://bsky.app/profile/bfeministapsol.bsky.social",
  # "https://bsky.app/profile/julianacarvalho13.bsky.social",
  # "https://bsky.app/profile/monicacunhario.bsky.social",
  # "https://bsky.app/profile/tainadepaularj.bsky.social",
  # "https://bsky.app/profile/tatiroque.bsky.social",
  # "https://bsky.app/profile/monicabenicio.bsky.social",
  # "https://bsky.app/profile/cidafalabella.bsky.social",
  # "https://bsky.app/profile/creuzamar.bsky.social",
  # "https://bsky.app/profile/betabastos.bsky.social"
)



# Função para extrair o nome de usuário da URL
extract_username <- function(url) {
  parts <- strsplit(url, "/")[[1]]
  username <- parts[length(parts)]  # Pega a última parte da URL como o nome de usuário
  return(username)
}

# Iterar sobre a lista de URLs e extrair os nomes de usuário
usernames <- sapply(profile_urls, extract_username)

# Loop para coletar dados de cada perfil
for (username in usernames) {
  cat("Coletando dados para:", username, "\n")
  
  # Obter dados do perfil e posts
  profile_data <- get_profile_data(username, token)
  posts_data <- get_all_posts(username, token)
  
  # Verifique se os dados do perfil e posts foram obtidos corretamente
  if (!is.null(profile_data) && length(posts_data) > 0) {
    cat("Dados obtidos com sucesso para:", username, "\n")
    
    # Converter perfil e posts para data.frames
    profile_df <- convert_profile_to_df(profile_data)
    posts_df <- convert_posts_to_df(posts_data)
    
    # Obter respostas para os posts
    replies_data <- get_all_replies_for_posts(posts_df, token)
    
    # Salvar os dados em arquivos CSV
    save_data_to_csv(profile_df, posts_df, replies_data, username)
  } else {
    cat("Erro ao obter perfil ou posts para o perfil:", username, "\n")
  }
}
