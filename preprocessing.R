remove(list = ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(rmarkdown)
library(plyr)
covid <- as.data.frame(covid)
NosograficoCovid <- unique(covid$NOSOGRAFICO)

#### Modifiche livelli ####
covid$STRRISULTATOESAMECORTO <- as.factor(covid$STRRISULTATOESAMECORTO)

covid$STRRISULTATOESAMECORTO <- mapvalues(covid$STRRISULTATOESAMECORTO, from = c("CONNI", "NCALC", "CNI", "CNP", "assente", "tracce", "CEM", "CCO", "CINS", "ND", "RP", "X-NORESULT", "CC", "CNE", "NDISP", "CLIP","<6.00",'> 2.0','<190.00','>35200.00','<13.00','>75.00','< 0.05','< 0.2','<15.00','>35000.00','>700'), 
                                          to = c(NA, NA, NA, NA, "0", "0", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,'5.99','2.1','189.00','35201','12.99','76.00','0.04','0.1','14','35001','701'))
covid$STRRISULTATOESAMECORTO <- as.numeric(as.character(covid$STRRISULTATOESAMECORTO))

#### calcolo delta T esame all'esame origine ####
covid$STRNOMEANALISIREFERTO <- as.factor(covid$STRNOMEANALISIREFERTO)
esami <- levels(covid$STRNOMEANALISIREFERTO)
covid$deltaT <- NA
covid$deltaT_baseline <- NA
covid$diffEsame <- NA

for (Noso in NosograficoCovid){
  print(Noso)
  NosogrSubset <- c()
  NosogrSubset <- covid[which(covid$NOSOGRAFICO==Noso),]
  for (esame in esami){
    filtroEs <- c()
    diffEsame <- c()
    diffData <- c()
    filtroEs <- NosogrSubset[which(NosogrSubset$STRNOMEANALISIREFERTO==esame),]
    indiceMinore <- row.names(filtroEs)[which(filtroEs$DTMDATAORAPRELIEVO==
                                                min(filtroEs$DTMDATAORAPRELIEVO))]
    dataMin <- min(filtroEs$DTMDATAORAPRELIEVO, na.rm = T)[1]
    covid[which(row.names(covid)
                %in% indiceMinore), "deltaT"] <- 0
    covid[which(row.names(covid)
                %in% indiceMinore), "deltaT_baseline"] <- 0
    diffEsame <- diff(filtroEs$STRRISULTATOESAMECORTO)
    diffData <- round(diff(filtroEs$DTMDATAORAPRELIEVO))
    if(units(diffData)!='days') {
      units(diffData) <- "days"
      diffData <- round(diffData)
    }
    
    if(nrow(filtroEs)>1){
      filtroEs <- filtroEs[-which(row.names(filtroEs)
                                  %in% indiceMinore[1]),]
      newRow <- row.names(filtroEs)
      dateDifference <- round(as.numeric(difftime(filtroEs$DTMDATAORAPRELIEVO,
                                                  dataMin, units = 'days')))
      covid[which(row.names(covid)
                  %in% newRow), "deltaT_baseline"] <- dateDifference
      covid[which(row.names(covid)
                  %in% newRow), "diffEsame"] <- diffEsame
      
      covid[which(row.names(covid)
                  %in% newRow), "deltaT"] <- seq(1,length(diffData))
      
    }
  }
}
covidFinale <- covid
rm(esami, Noso, NosogrSubset, dateDifference, newRow, filtroEs,
   dataMin, indiceMinore, esame, diffEsame, diffData, NosograficoCovid,
   covid)