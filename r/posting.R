# install.packages('devtools')
#remotes::install_github('christopherkenny/bskyr')

# Limpar a memória
rm(list=ls())
gc()
# Bibliotecas
library(bskyr)

# Definir as credenciais da API
bluesky_username <- Sys.getenv("BLUESKY_APP_USER")  # Defina seu nome de usuário do Bluesky nas variáveis de ambiente
bluesky_password <- Sys.getenv("BLUESKY_APP_PASS")  # Defina sua senha do Bluesky nas variáveis de ambiente

# posting text

bs_post('text = Your text goes here.')

# posting image

bs_post(
  text = 'Your text goes here.', 
  images = c('path/to/image1.jpg', 'path/to/image2.png')
)

bs_post(
  text = 'Your text goes here.', 
  images = c('path/to/image1.jpg', 'path/to/image2.png'), 
  images_alt = c('Alt text for image 1', 'Alt text for image 2')
)