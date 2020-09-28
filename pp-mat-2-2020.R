# Pre-processamento - PISRC_2020-2 - Matutino

# Seleciona o dir. de trabalho
setwd("G:\\Meu Drive\\Fatec\\FATEC - Aulas\\2020-2\\PISRC\\Datasets\\Matutino")



# Importa o dataset
dataset <- read.csv("Thursday-WorkingHours-Morning-WebAttacks.pcap_ISCX.csv", 
                    header = TRUE, stringsAsFactors = TRUE)



# Discretiza o dataset (todos os dados serão transformados
# em números inteiros positivos)
dataset_b <- data.matrix(dataset)



# Remove valores NaN (Not a Number)
dataset_c <- na.omit(dataset_b)



# Remove as colunas 15 e 16 por
# conterem valores infinitos
dataset_d = subset(dataset_c, select = -c(15,16))



# Seleciono somente as colunas de 1 a 76
# pois não se deve normalizar as labels (classes)
dataset_sem_label <- dataset_d[,1:76]



# Aplica a função de normalização
#
# novo_valor =  valor_atual - minimo_global
#              -----------------------------
#              maximo_global - minimo_global
dataset_e <- t(apply(dataset_sem_label, 1,
                     function(x)(x-min(x))/(max(x)-min(x))))

# Para visualizar os dados sem notação científica: 
# options(scipen = 999)
options(scipen = 999)

# Gera o dataset_final contendo:
# - à esquerda (dataset_e que é o dataset discrtizado e normalizado)
# - à direita (dataset_d[,77] que é a coluna dos rótulos (ou labels))
dataset_final <- merge(dataset_e, dataset_d[,77],
                       by="row.names", all = TRUE)

# Removendo a coluna Row.names que foi criada
# pelo merge()
dataset_final <- dataset_final[,2:78]

# Renomeia a coluna de rótulos de "y" para "Label"
names(dataset_final)[names(dataset_final) == "y"] <- "Label"

# Transformar as Labels para binário
# 1 -> benigno
# 2 -> ataque
dataset_final$Label[dataset_final$Label != 1] <- 0    

# Obtendo uma amostra de 1% dos dados
amostra_1perc <- dataset_final[
                               sample(nrow(dataset_final),
                               (nrow(dataset_final)/100)*1),]

# Obtendo uma amostra de 5% dos dados
amostra_5perc <- dataset_final[
                               sample(nrow(dataset_final),
                               (nrow(dataset_final)/100)*5),]


# Obtendo uma amostra de 10% dos dados
amostra_10perc <- dataset_final[
                               sample(nrow(dataset_final),
                               (nrow(dataset_final)/100)*10),]

# Obtendo uma amostra de 25% dos dados
amostra_25perc <- dataset_final[
                               sample(nrow(dataset_final),
                               (nrow(dataset_final)/100)*25),]

# Obtendo uma amostra de 50% dos dados
amostra_50perc <- dataset_final[
                               sample(nrow(dataset_final),
                               (nrow(dataset_final)/100)*50),]

# Grava no HD
write.csv(amostra_1perc, "amostra_1perc.csv", row.names = FALSE)  
write.csv(amostra_5perc, "amostra_5perc.csv", row.names = FALSE)  
write.csv(amostra_10perc, "amostra_10perc.csv", row.names = FALSE)  
write.csv(amostra_25perc, "amostra_25perc.csv", row.names = FALSE)  
write.csv(amostra_50perc, "amostra_50perc.csv", row.names = FALSE)  
write.csv(dataset_final, "amostra_full.csv", row.names = FALSE)  
