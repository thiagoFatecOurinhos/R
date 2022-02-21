library("EBImage")
setwd("G:\\Meu Drive\\Fatec\\Fatec - Aulas\\2022-1\\CD\\backups HD ext")
options(digits = 0)
img <- readImage("faces\\arfaceDataset_Cropped+Selected_by--thiago-at-fatecourinhos.edu.br\\face_1-1.jpg")
img <- channel(img,"gray")
img_data <- as.data.frame(img*256)

# # Função para calcular o histograma
# calculaHistograma <- function(dataset){
#   
#   histo <- matrix(0, 1, 256)
#   
#   qtddLinhas <- dim(dataset)[1]
#   qtddColunas<- dim(dataset)[2]
#   
#   for(linha in 1:qtddLinhas){
#     for(coluna in 1:qtddColunas){
#       histo[1,dataset[linha,coluna]] <- histo[1,dataset[linha,coluna]] + 1
#     }
#   }
#   
#   return(histo)
# }
# histo_original <- calculaHistograma(img_data)
# barplot(histo_original, xlim = c(1, 256))


# Função para normalização dos dados
normalizaDataset <- function(dataset){
  
  img_normalizada <- img
  
  qtddLinhas <- dim(dataset)[1]
  qtddColunas<- dim(dataset)[2]
  
  minValor <- min(img)
  maxValor <- max(img)
  
  for(linha in 1:qtddLinhas){
    for(coluna in 1:qtddColunas){
      img_normalizada[linha,coluna] <- ((img[linha,coluna] - minValor) / (maxValor - minValor))
    }
  }
  
  return(img_normalizada)
  
}


nova_imagem <- normalizaDataset(img_data)*255
nova_imagem <- nova_imagem + 1
#histo_normalizado <- calculaHistograma(nova_imagem)
#barplot(histo_normalizado, xlim = c(1, 256))



# Função para calcular o LBP
calculaLBP <- function(dataset){
  
  vetorLBP <- matrix(0, 1, 256)
  matriz_pesos <- matrix(c(1,2,4, 128, 0, 8, 64, 32, 16), nrow = 3, ncol = 3, byrow = TRUE)
  
  qtddLinhas <- dim(dataset)[1]
  qtddColunas<- dim(dataset)[2]
  
  for(linhaLBP in 2:(qtddLinhas-1)){
    for(colunaLBP in 2:(qtddColunas-1)){
      #obtem os vizinhos numa escala de 8 (3x3)
      #
      #    [x-1,y+1]   [x,y+1]  [x+1,y+1]    
      #    [ x-1,y ]   [  x  ]  [ x+1,y ]
      #    [x-1,y-1]   [x,y-1]  [x+1,y-1]
      #

      linhaInicio  <- linhaLBP - 1
      linhaFinal   <- linhaLBP + 1
      colunaInicio <- colunaLBP - 1
      colunaFinal  <- colunaLBP +1
      
      #sprintf("%i %i %i %i", linhaInicio, linhaFinal, colunaInicio, colunaFinal)
      #readline
      vizinhos_3x3 <- dataset[linhaInicio:linhaFinal,colunaInicio:colunaFinal] 
      vizinhos_3x3_processada <- matrix(0, 3, 3)
      
      

      for(iii in 1:3){
        for(jjj in 1:3){
          if(vizinhos_3x3[iii,jjj] >= vizinhos_3x3[2,2]){
            vizinhos_3x3_processada[iii,jjj] <- 1
          }
        }
      }
      
      valorLBP <- sum(vizinhos_3x3_processada * matriz_pesos)
      vetorLBP[1,valorLBP] <- vetorLBP[1,valorLBP] + 1
      
      }
  }

return(vetorLBP)      
} 
  
vetorLBP_final <- calculaLBP(nova_imagem)

