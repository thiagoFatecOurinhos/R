library("EBImage")
setwd("/home/thiago/Área de Trabalho/")
img <- readImage("pout.tif")*256
options(digits = 2)


# Função para calcular o histograma
calculaHistograma <- function(dataset){
  
  histo <- matrix(0, 1, 256)
  
  qtddLinhas <- dim(dataset)[1]
  qtddColunas<- dim(dataset)[2]
  
  for(linha in 1:qtddLinhas){
    for(coluna in 1:qtddColunas){
      histo[1,dataset[linha,coluna]] <- histo[1,dataset[linha,coluna]] + 1
    }
  }

  return(histo)
}
histo_original <- calculaHistograma(img)
#barplot(histo_original, xlim = c(1, 256))


# Função para normalização dos dados
normalizaDataset <- function(dataset){

  img_normalizada <- img

  qtddLinhas <- dim(dataset)[1]
  qtddColunas<- dim(dataset)[2]

  for(linha in 1:qtddLinhas){
  for(coluna in 1:qtddColunas){
    img_normalizada[linha,coluna] <- (img[linha,coluna] - min(img)) / (max(img) - min(img))
  }
}
  
return(img_normalizada)
  
}

nova_imagem <- normalizaDataset(img)*256
histo_normalizado <- calculaHistograma(nova_imagem)
#barplot(histo_normalizado, xlim = c(0, 256))
