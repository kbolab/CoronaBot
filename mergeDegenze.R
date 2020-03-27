# file di test
DEGENZE_COVID2703 <- read_excel("File_Marchetti/DEGENZE_COVID2703.xlsx")
toselect <- c("SANITARIO","NOSOGRAFICO","DATA_RICHIESTA","DATA_VALIDAZIONE","RISULTATO_ESAME","ETA",
              "DESC_PROVINCIA_RESIDENZA","DESC_REGIONE_RESIDENZA","NAZIONALITA",
              "SESSO","INGRESSO_PS","INDIVIDUAZIONE_PAZIENTE","RICOVERATI","DATA_RICOVERO",
              "DIMESSI","DATA_DIMISSIONE","N_TRASFERIMENTI",
              "DIAGNOSI_PRICIPALE","REP_RICOVERO","REP_DIMISSIONE","INTENSIVA","CLASSE_PAZIENTE",
              "DECEDUTI","DECEDUTI_TI","TI_DATA_INGRESSO",
              "TI_DATA_USCITA","GIORNI_DO","GIORNI_TI","DO_PRIMA_TI","GIORNATE_DEGENZA")
degenze <- DEGENZE_COVID2703[,which(names(DEGENZE_COVID2703) %in% toselect)]
degenze <- degenze[, toselect]

#seleziono gli esami laboratorio baseline e la data
baseline_labs <- covidFinale[which(covidFinale$deltaT ==0),c("NOSOGRAFICO",
                                                             "STRNOMEANALISIREFERTO","STRRISULTATOESAMECORTO")]

baseline_labs$STRRISULTATOESAMECORTO <- as.numeric(as.character(baseline_labs$STRRISULTATOESAMECORTO))
baseline_labs$STRNOMEANALISIREFERTO <- as.character(baseline_labs$STRNOMEANALISIREFERTO) 
baseline_labs$STRNOMEANALISIREFERTO <- gsub(" ", "", baseline_labs$STRNOMEANALISIREFERTO)

baseline_labs_wide <- dcast(baseline_labs, 
                            NOSOGRAFICO ~ STRNOMEANALISIREFERTO, value.var="STRRISULTATOESAMECORTO",
                            fun.aggregate =function(x) paste(x[1]))
names(baseline_labs_wide[,2:dim(baseline_labs_wide)[2]]) <- paste(names(baseline_labs_wide[,2:dim(baseline_labs_wide)[2]]),"_baseline")

degenze_labbaseline <- merge(degenze,baseline_labs_wide,by = "NOSOGRAFICO")

library(openxlsx)
write.xlsx(degenze_labbaseline,file = "degenze_labbaseline.xlsx")