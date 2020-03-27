remove(list = ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(rmarkdown)
library(dplyr)
#### Tabella covid ####
covid <- read_excel("./File_Marchetti/ESAMI_MURRI2703.xlsx")
covid <- as.data.frame(covid)
NosograficoCovid <- unique(covid$NOSOGRAFICO)
#### Tabella tampone ####
tampone <- read_excel("./File_Marchetti/TAMPONI2703.xlsx")
tampone <- as.data.frame(tampone)
NosograficoTampone <- unique(tampone$NOSOGRAFICO)
# tampone$RISULTATO_ESAME <- as.factor(tampone$RISULTATO_ESAME)
# tampone$DESC_MATERIALE <- as.factor(tampone$DESC_MATERIALE)

# fare un join dei nosografici tabella covid e tabella tampone
# NosograficoJoint <- NosograficoTampone
NosograficoJoint <- intersect(NosograficoCovid, NosograficoTampone)
# estrarre solo il nosografico presente nelle tabella covid e tampone
tamponeProva <- tampone[ tampone$NOSOGRAFICO %in% NosograficoJoint, ]

#### Raggruppo il tampone faringeo e nasale in un'unico tampone ####
for (i in NosograficoJoint){
  
  NosogrSubset <- c()
  newData <- c()
  NosogrSubset <- tamponeProva[which(tamponeProva$NOSOGRAFICO==i),]
  # prendo la riga con tampone faringeo
  indice <- which(NosogrSubset$DESC_MATERIALE=='Tampone faringeo')
  rowNameFar <- row.names(NosogrSubset)[which(NosogrSubset$DESC_MATERIALE=='Tampone faringeo')]
  newData <- NosogrSubset[-which(row.names(NosogrSubset) 
                                 %in% rowNameFar), ]
  
  
  for (materiale in indice){
    prova <- rownames(newData)[which(newData$DATA_RICHIESTA>=NosogrSubset$DATA_RICHIESTA[materiale] & 
                                       newData$DATA_RICHIESTA<NosogrSubset$DATA_RICHIESTA[materiale]+60*60)]
    
    if(length(prova)!=0 & length(which(row.names(tamponeProva) %in% prova))!=0){
      
      tamponeProva <- tamponeProva[-which(row.names(tamponeProva) 
                                          %in% prova), ]
      tamponeProva$DESC_MATERIALE[which(row.names(tamponeProva) %in% row.names(NosogrSubset)[materiale])] <- 'Tampone faringeo, Secr. nasale'
      
    }
    
  }
}

rm(i, indice, materiale, prova, rowNameFar, newData)

#### Estrarre colonna outcome e salvarlo nel dataframe tamponeFinale####
tamponeFinale <- matrix(nrow = length(NosograficoJoint), 
                        ncol = length(c('DATA_ORA_AGGIORNAMENTO', 'SANITARIO', 'NOSOGRAFICO',
                                        'DATA_RICHIESTA', 'RISULTATO_ESAME',
                                        'NUMERO_TAMP')))
tamponeFinale <- data.frame(tamponeFinale)
colnames(tamponeFinale) <- c('DATA_ORA_AGGIORNAMENTO', 'SANITARIO', 'NOSOGRAFICO',
                             'DATA_RICHIESTA', 'RISULTATO_ESAME',
                             'NUMERO_TAMP')
tamponeFinale$DATA_ORA_AGGIORNAMENTO <- as.POSIXct(tamponeFinale$DATA_ORA_AGGIORNAMENTO, tz="Europe/London")
tamponeFinale$SANITARIO <- as.character(tamponeFinale$SANITARIO)
tamponeFinale$NOSOGRAFICO <- as.character(tamponeFinale$NOSOGRAFICO)
tamponeFinale$DATA_RICHIESTA <- as.POSIXct(tamponeFinale$DATA_RICHIESTA, tz="Europe/London") 
tamponeFinale$RISULTATO_ESAME <- as.character(tamponeFinale$RISULTATO_ESAME)
tamponeFinale$NOSOGRAFICO <- NosograficoJoint

for (i in NosograficoJoint){
  NosogrSubset <- c()
  NosogrSubset <- tamponeProva[which(tamponeProva$NOSOGRAFICO==i),]
  if(length(which(NosogrSubset$RISULTATO_ESAME=='+'))!=0){
    
    NosogrSubsetPos <- NosogrSubset[which(NosogrSubset$RISULTATO_ESAME=='+'),]
    indice <- which(NosogrSubsetPos$DATA_RICHIESTA==min(NosogrSubsetPos$DATA_RICHIESTA))
    tamponeFinale[which(tamponeFinale$NOSOGRAFICO==i),c(1,2,5)] <- NosogrSubsetPos[indice,c(1,2,9)]
    tamponeFinale$DATA_RICHIESTA[which(tamponeFinale$NOSOGRAFICO==i)] <- min(NosogrSubsetPos$DATA_RICHIESTA)
    tamponeFinale$NUMERO_TAMP[which(tamponeFinale$NOSOGRAFICO==i)] <- nrow(NosogrSubset)
  }
  else{
    
    tamponeFinale[which(tamponeFinale$NOSOGRAFICO==i),c(1,2,5)] <- NosogrSubset[1,c(1,2,9)]
    tamponeFinale$DATA_RICHIESTA[which(tamponeFinale$NOSOGRAFICO==i)] <- min(NosogrSubset$DATA_RICHIESTA)
    tamponeFinale$NUMERO_TAMP[which(tamponeFinale$NOSOGRAFICO==i)] <- nrow(NosogrSubset)
  }
}
rm(i, NosogrSubset, indice, NosogrSubsetPos)


#### calcolo delta T esame all'esame origine ####
covid$STRNOMEANALISIREFERTO <- as.factor(covid$STRNOMEANALISIREFERTO)
esami <- levels(covid$STRNOMEANALISIREFERTO)
covid$deltaT <- NA

for (Noso in NosograficoCovid){
  print(Noso)
  NosogrSubset <- c()
  NosogrSubset <- covid[which(covid$NOSOGRAFICO==Noso),]
  for (esame in esami){
    filtroEs <- c()
    filtroEs <- NosogrSubset[which(NosogrSubset$STRNOMEANALISIREFERTO==esame),]
    indiceMinore <- row.names(filtroEs)[which(filtroEs$DTMDATAORAPRELIEVO==
                                                min(filtroEs$DTMDATAORAPRELIEVO))]
    dataMin <- min(filtroEs$DTMDATAORAPRELIEVO, na.rm = T)[1]
    covid[which(row.names(covid) 
                %in% indiceMinore), "deltaT"] <- 0
    if(nrow(filtroEs)>1){
      filtroEs <- filtroEs[-which(row.names(filtroEs) 
                                  %in% indiceMinore),]
      newRow <- row.names(filtroEs)
      dateDifference <- round(as.numeric(difftime(filtroEs$DTMDATAORAPRELIEVO,
                                                  dataMin, units = 'hours'))/12)
      covid[which(row.names(covid) 
                  %in% newRow), "deltaT"] <- dateDifference
      
    }
  }
}

rm(esami, Noso, NosogrSubset, dateDifference, newRow, filtroEs, 
   dataMin, indiceMinore, esame)

covidFinale <- (merge(covid, tamponeFinale, by=c("NOSOGRAFICO","NOSOGRAFICO")))

#### Modifiche livelli ####

covidFinale$Bilirubina <- NA
covidFinale$Bilirubina <- covidFinale$STRRISULTATOESAMECORTO

covidFinale$STRRISULTATOESAMECORTO <- as.factor(covidFinale$STRRISULTATOESAMECORTO)

covidFinale$STRRISULTATOESAMECORTO <- mapvalues(covidFinale$STRRISULTATOESAMECORTO, from = c("CONNI", "NCALC", "CNI", "CNP", "assente", "tracce", "CEM", "CCO", "CINS", "ND", "RP", "X-NORESULT", "CC", "CNE", "NDISP", "CLIP","<6.00",'> 2.0','<190.00','>35200.00','<13.00','>75.00','< 0.05'), 
                                                to = c(NA, NA, NA, NA, "0", "0", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,'5.99','2.1','189.00','35201','12.99','76.00','0.04'))
