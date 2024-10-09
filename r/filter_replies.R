rm(list = ls())
gc()


# Carregar pacotes necessários
library(dplyr)
library(readr)
library(stringr)

# Lista de palavras-chave para filtrar
termos <- c(
  "aberração", "aborteira", "abortista", "aborto", "acabada", "aleijada", "alejada", 
  "alienígena", "amante", "ameba", "analfabeta", "animal", "anta", "baguio", "bagulho", 
  "bandida", "baranga", "Batata", "besta", "bicha", "bicho", "boca", "bruxa", "burra", 
  "Cabeçuda", "Cabeluda", "Cafajeste", "calada", "Camuflada", "canalha", "canhota", 
  "capacho", "Charlatã", "chata", "chilique", "chimpanzé", "Chinelona", "chora", "Chora", 
  "chorona", "chupa", "Cobra", "coitada", "Comadre", "comunista", "Corja", "corrupta", 
  "Cova", "covarde", "cozinha", "Creche", "cretina", "criatura", "criminosa", "crioula", 
  "Cu", "dar o rabo", "Demagógica", "demente", "demônia", "descabelada", "descontrolada", 
  "Desgraçada", "despreparada", "Desqualificada", "dissimulada", "ditadura", "doida", 
  "dois ovos", "drogada", "embrião", "Escrota", "esquerdista", "esquerdopata", "Essazinha", 
  "estrume", "estupro", "et de varginha", "Excursão", "Fale", "Fanática", "Fantoche", 
  "Farinha", "fazendeira", "fdp", "fedida", "fedorenta", "feia", "feminazi", "feminista", 
  "Ferro", "filha da puta", "Fodida", "fraca", "frouxo", "Gado", "Gagá", "garota", 
  "gayzista", "gópi", "gorda", "gostosa", "grossa", "Guela", "High School Musical", 
  "hipócrita", "histérica", "horrorosa", "humanE", "ideologia de gênero", "idiota", 
  "imatura", "imbecil", "imoral", "imunda", "incompetente", "índia", "Infeliz", "Insulto", 
  "Insuportável", "intragável", "Jantou", "japa", "jumenta", "kinder ovo", "lacradores", 
  "Lacrar", "Ladeira", "ladra", "Lapada", "LGBT", "LGVT", "lixo", "louca", "lunática", 
  "lunático", "macaca", "macetável", "macho", "macho de saia", "machona", "maconheira", 
  "maldita", "maluca", "Mamadeira", "mamar", "Mandioca", "mangina", "Manicômio", 
  "Manipuladora", "marica", "Marionete", "marmita", "menina", "mentirosa", "merda", 
  "meter uma bala", "militante", "mimimi", "minion", "moleque", "moral", "Moralista", 
  "muié", "mula", "mulher", "mulher de verdade", "mulherzinha", "múmia", "namorada", 
  "namoradinha", "Narcisista", "Negacionista", "negra", "neguinha", "nojenta", 
  "nojo (transfobia)", "Nordestina", "Otária", "Palhaça", "Pão", "parasita", "Patata", 
  "Patética", "Patrícia", "peluda", "pepa, peppa", "Perdida", "personagem", "péssima", 
  "péssima mãe", "piada", "pilantra", "piranha", "porca", "Presta", "preta", "promíscua", 
  "prostituta", "puxa saco", "quadrilha", "queima rosca", "queimar", "raça", "retardada", 
  "ridícula", "rola", "rostinho bonitinho", "saias", "Saidinha", "santinha", "sapatão", 
  "sapatona", "selvagem", "sem graça", "Sofrida", "suja", "tapada", "tchutchuca", 
  "transtorno mental", "traveco", "trem desse", "Trouxa", "vaca", "vadia", "vagabunda", 
  "vara", "veia", "velha", "vergonha", "verme", "viadinho", "vitimismo", "fascista"
)

# Definir o diretório onde os CSVs estão localizados
caminho <- "./data"

# Obter a lista de arquivos CSV com "replies" no nome
arquivos <- list.files(path = caminho, pattern = "replies.*\\.csv", full.names = TRUE)


# Adicionar \b (limite de palavra) para garantir que sejam encontradas palavras exatas
termos_regex <- paste0("\\b", termos, "\\b")

# Função para processar as palavras e encontrar as palavras e suas contagens corretamente
processar_palavras <- function(arquivo) {
  # Ler o arquivo CSV
  df <- read_csv(arquivo)
  
  # Verificar se a coluna 'reply_text' está presente no arquivo
  if ("reply_text" %in% colnames(df)) {
    
    # Criar uma nova coluna com as palavras encontradas e contagem usando regex para correspondência exata
    df <- df %>%
      rowwise() %>%
      mutate(
        palavras_encontradas = paste(termos[str_detect(reply_text, termos_regex)], collapse = ", "),
        contagem_palavras = sum(str_count(reply_text, termos_regex))
      ) %>%
      filter(contagem_palavras > 0)  # Mantém apenas as linhas com palavras encontradas
    
    return(df)
  } else {
    # Se a coluna 'reply_text' não estiver presente, retornar um aviso
    warning(paste("Coluna 'reply_text' não encontrada no arquivo:", arquivo))
    return(data.frame())
  }
}

# Inicializar o DataFrame final para armazenar todos os resultados
resultados_final <- data.frame()

# Processar todos os arquivos
for (arquivo in arquivos) {
  # Processar o arquivo atual
  resultado <- processar_palavras(arquivo)
  
  # Combinar o resultado com o DataFrame final
  resultados_final <- bind_rows(resultados_final, resultado)
}

# Salvar o resultado final em um CSV
write_csv(resultados_final, "./data/filtro/replies_filtrados_M_final.csv")

########################## HOMENS ##########################

rm(list = ls())
gc()


# Carregar pacotes necessários
library(dplyr)
library(readr)
library(stringr)

# Lista de palavras-chave para filtrar
termos <- c(
  "Acabado", "Acéfalo", "Acéfalo", "Adolescente", "Aleijado", "Aliciado", "Alienígena", 
  "Amante", "Ameba", "Analfabeto", "Anta", "Apadrinhado", "Asno", "Babaca", "Bagulho", 
  "Bahiano", "Baixaria", "Bandido", "Bando", "Barraqueiro", "Bater", "Bêbado", "Besta", 
  "Bicha", "Bicho", "Bola", "Bosta", "Boules", "Broxa", "Bunda", "Burro", "Cabaré", 
  "Cabeçudo", "Cachaceiro", "Cafajeste", "Cafetão", "Cagalhão", "Cagar", "Calado", 
  "Cambada", "Camuflado", "Canalha", "Canhota", "Cão", "Capacho", "Capeta", "Capiroto", 
  "Censurado", "Charlatão", "Chato", "Cheirador", "Chilique", "Chimpanzé", "Chinelão", 
  "Chora", "Chorão", "Chupa", "Chupeta", "Cobra", "Coitada", "Compulsivo", "Comunista", 
  "Conversa", "Corja", "Corrupto", "Cova", "Covarde", "Cretino", "Criatura", "Criminoso", 
  "Crioulo", "Cu", "Cuzão", "Dar o rabo", "Demagógico", "Demente", "Demônio", 
  "Descontrolado", "Desgraçado", "Despreparado", "Desqualificado", "Dissimulado", 
  "Ditadura", "Doente", "Doido", "Drogado", "Embrião", "Enrustido", "Escroto", 
  "Esquerdista", "Essezinho", "Estimação", "Estrume", "Fanático", "Fantoche", "Farinha", 
  "Fascista", "fdp", "Fedido", "Feio", "Filho da puta", "Fiofó", "Fodido", "Fraco", 
  "Frouxo", "Gado", "Gagá", "Garoto", "Gayzista", "Genocida", "Gordo", "Grosso", "Guela", 
  "Hipócrita", "Homem", "Horroroso", "HumanE", "Ideologia de gênero", "Idiota", 
  "Ignóbil", "Imaturo", "Imbecil", "Imbroxável", "Imoral", "Imundo", "Incompetente", 
  "Índio", "Infeliz", "Insulto", "Insuportável", "Intragável", "Invadir", "Isento", 
  "Jantou", "Japa", "Jumento", "Lacradores", "Ladeira", "Ladrão", "Lapada", "Lixo", 
  "Louco", "Lugar", "Macaco", "Macho", "Maconheiro", "Maldito", "Maluco", "Mamadeira", 
  "Mamar", "Mandioca", "Mangina", "Manicômio", "Manipulador", "Marica", "Marionete", 
  "Marmita", "Menino", "Mensaleiro", "Mentiroso", "Merda", "Meter uma bala", "Miliciano", 
  "Militante", "Mimimi", "Minion", "Moleque", "Moral", "Moralista", "Muié", "Múmia", 
  "Narcisista", "Narrativa", "Negacionista", "Negro", "Neguinho", "Neutro", "Nine", 
  "Nojo", "Nordestino", "Otário", "Palhaço", "Pão", "Papo", "Parasita", "Patético", 
  "Pau", "Perdido", "Péssimo", "Piada", "Pilantra", "Pinto", "Piroca", "Porra", 
  "Presidiário", "Presta", "Preto", "Puto", "Puxa saco", "Quadrilha", "Queima rosca", 
  "Retardado", "Ridículo", "Rola", "Saco", "Safado", "Saidinha", "Santinho", 
  "Satanista", "Selvagem", "Sem graça", "Sujo", "Tchutchuca", "Traíra", "Trans", 
  "Transtorno mental", "Traveco", "Trosoba", "Trouxa", "Vagabundo", "Vara", "Velho", 
  "Verdade", "Vergonha", "Verme", "Viadinho", "Viciado", "Vitimismo"
)


# Definir o diretório onde os CSVs estão localizados
caminho <- "./data"

# Obter a lista de arquivos CSV com "replies" no nome
arquivos <- list.files(path = caminho, pattern = "replies.*\\.csv", full.names = TRUE)


# Adicionar \b (limite de palavra) para garantir que sejam encontradas palavras exatas
termos_regex <- paste0("\\b", termos, "\\b")

# Função para processar as palavras e encontrar as palavras e suas contagens corretamente
processar_palavras <- function(arquivo) {
  # Ler o arquivo CSV
  df <- read_csv(arquivo)
  
  # Verificar se a coluna 'reply_text' está presente no arquivo
  if ("reply_text" %in% colnames(df)) {
    
    # Criar uma nova coluna com as palavras encontradas e contagem usando regex para correspondência exata
    df <- df %>%
      rowwise() %>%
      mutate(
        palavras_encontradas = paste(termos[str_detect(reply_text, termos_regex)], collapse = ", "),
        contagem_palavras = sum(str_count(reply_text, termos_regex))
      ) %>%
      filter(contagem_palavras > 0)  # Mantém apenas as linhas com palavras encontradas
    
    return(df)
  } else {
    # Se a coluna 'reply_text' não estiver presente, retornar um aviso
    warning(paste("Coluna 'reply_text' não encontrada no arquivo:", arquivo))
    return(data.frame())
  }
}

# Inicializar o DataFrame final para armazenar todos os resultados
resultados_final <- data.frame()

# Processar todos os arquivos
for (arquivo in arquivos) {
  # Processar o arquivo atual
  resultado <- processar_palavras(arquivo)
  
  # Combinar o resultado com o DataFrame final
  resultados_final <- bind_rows(resultados_final, resultado)
}

# Salvar o resultado final em um CSV
write_csv(resultados_final, "./data/filtro/replies_filtrados_H_final.csv")
