#####################################################################
##### FASE 3: Association tests                                 #####
##### Association test on the lcuster results
#
#####################################################################

#LIBRERIAS
library(reshape2)
library(dplyr)
library(data.table)

#DATASET
#Dataset 2018
nfich4 <- c("QualyQuant2F.RData")
load(nfich4)

###########################################################################
###           Chi-Square Test                                           ###
### H0: Both variables are independet
### Ha: Both variables are associated one to each other
###########################################################################


# STEp 1: Processing of the Crypto.Qualy table
Crypto.Qualy.red <- Crypto.Qualy
#Focoused on the relevant variables for the association tests
Crypto.Qualy.red <- Crypto.Qualy.red[,c(-1,-5,-7,-8,-9,-10,-11,-12)] #Quitamos algunas variables

#Renaming some variables
colnames(Crypto.Qualy.red) <- c("Algorithm","ProofType","MkCap","Age","Sharp","M2","M2Sort",
                     "Sort","Beta","Kmeans","HistDAWass","TADPole","Combi")

#Re-ordering of the variables
setcolorder(Crypto.Qualy.red, c("Algorithm","ProofType","MkCap","Beta","M2","M2Sort","Sort",
                                "Sharp","Age","Kmeans","HistDAWass","TADPole","Combi"))

#We apply a data.frame format required for the association functions
Crypto.Qualy.red <- as.data.frame(Crypto.Qualy.red)

#The table DepVarCat.Fisher is filled with the p-values of all association tests
DepVarCat.Fisher <- matrix(nrow = ncol(Crypto.Qualy.red), ncol = ncol(Crypto.Qualy.red), byrow = FALSE)
colnames(DepVarCat.Fisher) <- colnames(Crypto.Qualy.red)
rownames(DepVarCat.Fisher) <- colnames(Crypto.Qualy.red)

### We process the intersection of clusters (rename of subcluster focouse on the 6's higher cardinality)
Combi10 <- c(6,2,16,12,18,14) #Identificadores de las principaels intersecciones de clusters 
#We filter the higher cardinality
Crypto.Qualy.red2 <- subset(Crypto.Qualy.red, Combi %in% Combi10)
Crypto.Qualy.red2$Combi <- as.factor(Crypto.Qualy.red2$Combi)
#We rename the intersections
Crypto.Qualy.red2$Combi <- factor(Crypto.Qualy.red2$Combi,
                                  labels = c("6","2","16","12","18","14"))
Crypto.Qualy.red2$Combi <- as.integer(Crypto.Qualy.red2$Combi) #Pasamos Combi de Factor a String


#STEP 2: ASSOCIATION TEST computation
set.seed(123)
for (i in 1:ncol(Crypto.Qualy.red)){        #For columns
        for (j in i:ncol(Crypto.Qualy.red)){#For rows
                if (i==j) {
                        DepVarCat.Fisher[i,j]=0
                        #DepVarCat.Cramer[i,j]=1
                        next}
                tempContingTabl <- table(Crypto.Qualy.red[,i],Crypto.Qualy.red[,j])
                
                #Fisher's exact test
                temp1 <- chisq.test(tempContingTabl, 
                                                 simulate.p.value = TRUE, B=8000)$p.value
                DepVarCat.Fisher[i,j] <- DepVarCat.Fisher[j,i] <- temp1
        }
}

#p-values for all association tests
sink('AssocFisherTest.txt')
DepVarCat.Fisher
sink()

#STEP 3: Processing of association results
# We highlight more relevant associations setting a threshold for the p-values (>0.01)
temp <- DepVarCat.Fisher
temp1 <- temp > 0.01
temp[temp1] <- 1 
set.seed(123)
heatmap(1-temp, Colv = NA, Rowv = NA, 
        col = cm.colors(256), symm = TRUE,revC = TRUE,
        cexRow = 0.9, cexCol = 0.9
)

#Funtion for detailed analysis of the association for the different values of categoriacal variables
residPearson.mtx <- function(chiQres.mtx, chiQobs.mtx){ #The parameter are the residuals of the contigency table
        maxcol <- max.col(abs(chiQres.mtx)) #We filter by the maxium residual values
        nr <- dim(chiQres.mtx)[1]
        i <- 1:nr
        #techno <- data.frame(Alg=character(), Cons=character(), res=numeric(), stringsAsFactors = FALSE)
        temp.res <- vapply(1:nr,function(x) {chiQres.mtx[x,maxcol[x]]}, numeric(1))
        temp.obs<- vapply(1:nr,function(x) {chiQobs.mtx[x,maxcol[x]]}, numeric(1))
        techno <-cbind(rownames(chiQres.mtx)[i], colnames(chiQres.mtx)[maxcol[i]], temp.res, temp.obs)        
        colnames(techno) <- c('Var1','Var2','res','obs')
        row.names(techno)<-c()
        
        techno <- as.data.frame(techno, stringsAsFactors = FALSE)
        techno$res <- as.numeric(techno$res)
        techno$obs <- as.integer(techno$obs)
        techno <- techno[order(techno$res, decreasing = TRUE),]
        techno <- techno[order(-abs(techno$res)),] #Dataframe ordenado por residuo
        return(techno)
} 


#STEP 4: PEARSON'S RESIDUALS
library(vcd) #Library for Person's residula representation

########### (Figure 11.a) MarketCap - Kmeans ########################################
Feat <- Crypto.Qualy.red[,3]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,10]
ClustFeat <- as.factor(ClustFeat)

Marketcap <- Feat
ClustKmeans <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustKmeans,Marketcap)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Marketcap VS Cluster Kmeans')

sink('ContingTablMkCap_Kmeans.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed

sink('PearsTablMkCap_Kmeans.txt')
temp.Chi$residuals
sink()

######### Figure 11.b) MarketCap - HistDAWass ########################################
Feat <- Crypto.Qualy.red[,3]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,11]
ClustFeat <- as.factor(ClustFeat)

Marketcap <- Feat
ClustDAWass <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustDAWass,Marketcap)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Marketcap VS Cluster Hist DAWass')

sink('ContingTablMkCap_DAWass.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed

sink('PearsTablMkCap_DAWass.txt')
temp.Chi$residuals
sink()


######### Figure 12) Pearson's residual for K-means and Beta
Feat <- Crypto.Qualy.red[,4]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NegBeta','CashLike','LowVol','NASDAQlike',
                              'HighVol','Extreme'))

ClustFeat <- Crypto.Qualy.red[,10]
ClustFeat <- as.factor(ClustFeat)

Beta <- Feat
ClustKmeans <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustKmeans,Beta)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Beta VS K-means cluster')

sink('TabFreqBetaKMeans.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocBetaKMeans.txt')
temp.Chi$residuals
sink()

### Figure 13) Pearson's resisual for TADPole clustering (Sharpe ratio)
# Ratio de Sharp - TADPole ##############################################
Feat <- Crypto.Qualy.red[,8]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','SRF','ERP','Acc','GOOD'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,12]
ClustFeat <- as.factor(ClustFeat)

Sharp <- Feat
ClusTADPole <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClusTADPole,Sharp)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Ratio de Sharp vs TADPole')

sink('TabFreqSharpeTADPole.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed

sink('TabAsocSharpeTADPole.txt')
temp.Chi$residuals
sink()


##Figure 14.a) Modigliani-Modigliani TADPole
# Ratio de Modigliani-Modigliani (M2) - TADPole ##############################################
Feat <- Crypto.Qualy.red[,5]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,12]
ClustFeat <- as.factor(ClustFeat)

M2 <- Feat
ClusTADPole <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClusTADPole,M2)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Modigliani-Modigliani ratio VS TADPole')

sink('TabFreqM2TADPole.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocM2TADPole.txt')
temp.Chi$residuals
sink()

##Figure 14.b) Sortino - TADPole
# Ratio de Sorti - TADPole ##############################################
Feat <- Crypto.Qualy.red[,7]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,12]
ClustFeat <- as.factor(ClustFeat)

Sort <-Feat
ClusTADPole<-ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClusTADPole,Sort)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Ratio de Sorti VS TADPole')

sink('TabFreqSortTADPole.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocSortTADPole.txt')
temp.Chi$residuals
sink()

##Figure 14.c) M2 Sortino - TADPole
# Ratio de Modigliani-Modigliani (M2) Sortino - TADPole ##############################################
Feat <- Crypto.Qualy.red[,6]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,12]
ClustFeat <- as.factor(ClustFeat)

ClusTADPole <- ClustFeat
M2Sort <- Feat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClusTADPole,M2Sort)
#mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
#           main = 'Ratio de M2 Sorti VS TADPole')

sink('TabFreqM2SortTADPole.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocM2SortTADPole.txt')
temp.Chi$residuals
sink()

Crypto.Qualy[PercMKCap=='P99'&TADPole==2,.(SYM, ClassMKCap, PercMKCap, DecM2Sort, TADPole)]
Crypto.Qualy[PercMKCap=='P100'&TADPole==3,.(SYM, ClassMKCap, PercMKCap, DecM2Sort, TADPole)]

### Figure 15.a) Beta and Cluster Interesections
# Beta - Combi de clusters ##############################################
#Primero nos quedamos con los 10 clusters combinados o bunches de mayor cardinalidad
Feat <- Crypto.Qualy.red2[,4]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','NegBeta','CashLike','LowVol','NASDAQlike',
                              'HighVol','Extreme'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red2[,13]
ClustFeat <- as.factor(ClustFeat)

Beta <- Feat
Combi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(Combi,Beta)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Beta VS Cluster combinations')

sink('TabFreqBetaCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocBetaCombi.txt')
temp.Chi$residuals
sink()

# Figure 15.b) Sharpe ratio - Cluster Intersection
# Ratio de Sharpe - Combi ##############################################
Feat <- Crypto.Qualy.red2[,8]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','SRF','ERP','Acc','GOOD'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red2[,13]
ClustFeat <- as.factor(ClustFeat)

Sharpe <- Feat
Combi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(Combi,Sharpe)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Ratio de Sharp vs TADPole')

sink('TabFreqSharpeCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocSharpeCombi.txt')
temp.Chi$residuals
sink()


## Figure 16.a) Modigliani - Modigliani (M2) - Intersection
# Ratio de Modigliani-Modigliani (M2) - Combi ##############################################
Feat <- Crypto.Qualy.red2[,5]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red2[,13]
ClustFeat <- as.factor(ClustFeat)

M2 <- Feat
Combi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(Combi,M2)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Modigliani-Modigliani ratio VS TADPole')

sink('TabFreqM2Combi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$stdres, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocM2Combi.txt')
temp.Chi$residuals
sink()

## Figure 16.b) Sortino - Intersection
# Ratio de Sortino - Combi de clusters ##############################################
Feat <- Crypto.Qualy.red2[,7]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red2[,13]
ClustFeat <- as.factor(ClustFeat)

Sort <- Feat
Combi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(Combi,Sort)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Sortino ratio VS Cluster combinations')

sink('TabFreqSortCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocSortCombi.txt')
temp.Chi$residuals
sink()

# Figure 16.c) M2 Sortino - Intersection of clusters
# Ratio de M2 Sorti - Combi ##############################################
Feat <- Crypto.Qualy.red2[,6]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red2[,13]
ClustFeat <- as.factor(ClustFeat)

Combi <- ClustFeat
M2Sort <- Feat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(Combi,M2Sort)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Ratio de M2 Sorti VS TADPole')

sink('TabFreqM2SortCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocM2SortCombi.txt')
temp.Chi$residuals
sink()


# Figure 17) MarketCap -  Cluster Intersection
# MarketCap - Combi ########################################
Feat <- Crypto.Qualy.red2[,3]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red2[,13]
ClustFeat <- as.factor(ClustFeat)

Marketcap <- Feat
Combi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(Combi,Marketcap)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Marketcap VS Cluster combinations')

sink('TabFreqMkCapCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocMkCapCombi.txt')
temp.Chi$residuals
sink()

## Figure 18.a) Age - Kmeans
# Edad - Kmeans ##############################################
Feat <- Crypto.Qualy.red[,9]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,10]
ClustFeat <- as.factor(ClustFeat)

#tempContingTabl <- table(Feat,ClustFeat)
ClustKmeans <- ClustFeat
Maturity <- Feat
tempContingTabl <- table(ClustKmeans,Maturity)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Maturity vs K-means')

sink('TabFreqAgeKmeans.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$stdres, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocAgeKmeans.txt')
temp.Chi$residuals
sink()

## Figure 18.b) Age - WH-Kmeans
# Edad - WH-Kmeans ##############################################
Feat <- Crypto.Qualy.red[,9]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,11]
ClustFeat <- as.factor(ClustFeat)

#tempContingTabl <- table(Feat,ClustFeat)
Maturity <- Feat
ClustDAWass <- ClustFeat
tempContingTabl <- table(ClustDAWass,Maturity)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Maturity vs Histo-DAWass')

sink('TabFreqAgeDAWass.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$stdres, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocAgeDAWass.txt')
temp.Chi$residuals
sink()

# Figure 18.c) Age - Cluster Intersection
# Edad - Combi ##############################################
Feat <- Crypto.Qualy.red2[,9]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red2[,13]
ClustFeat <- as.factor(ClustFeat)

Maturity <- Feat
Combi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(Combi,Maturity)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Age (maturity) VS Cluster combinations')

sink('TabFreqAgeCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$residuals
temp.Chi$observed
sink('TabAsocAgeCombi.txt')
temp.Chi$residuals
sink()




################### SCREENING ######################################
### Clusters Kmeans ##############
#Cluster 1
Crypto.Qualy[Kmeans==1 & ClassBeta=='NegBeta' & PercMKCap=='P99',.(SYM, PercMKCap, ClassBeta, Kmeans)]
Crypto.Qualy[Kmeans==1 & ClassBeta=='HighVol' & PercMKCap=='P99',.(SYM, PercMKCap, ClassBeta, Kmeans)]

#Cluster 2
Crypto.Qualy[Kmeans==2,.(SYM, PercMKCap, ClassBeta, Kmeans)]

#Cluster 3
Crypto.Qualy[Kmeans==3,.(SYM, PercMKCap, Kmeans)]
Crypto.Qualy[Kmeans==3 & ClassBeta=='CashLike' & PercMKCap=='P70',.(SYM, PercMKCap, ClassBeta, Kmeans)]
Crypto.Qualy[Kmeans==3 & ClassBeta=='LowVol' & PercMKCap=='P100',.(SYM, PercMKCap, ClassBeta, Kmeans)]

Crypto.Famd[(Kmeans==3 & rmedio > 0),.(SYM, rmedio, Kmeans)]
aaa <- Crypto.Famd[(Kmeans==3 & rmedio > 0),.(SYM)]
bbb <- Crypto.Qualy[SYM %in% aaa$SYM]
bbb <- bbb[(DecRet=='D10'& DecRisk=='D04'),.(SYM, DecRet, DecRisk, PercMKCap)]

### Clusters Hist DAWass #############
#We incorporate the Coefficient of Variation and Marketcap
Crypto.Famd.ext <- Crypto.Famd
Crypto.Famd.ext$CV <- Crypto.Famd$volatility / Crypto.Famd$rmedio
Crypto.Famd.ext <- merge(Crypto.Famd.ext,MKTCAPShare[,c(-3,-4)])

table(Crypto.Famd.ext$HistDAWass)
table(Crypto.Qualy$HistDAWass)
zerovolatiN.total = nrow(Crypto.Famd.ext[volatility == 0]) #Total number of zero volatility Cryptocurrencies in 2018

#Cluster 1
temp <- Crypto.Qualy[HistDAWass==1,.(SYM, PercMKCap, DecRisk, DecRet, HistDAWass)]
temp$PercMKCap <- as.factor(temp$PercMKCap)
temp$DecRisk <- as.factor(temp$DecRisk)
temp$DecRet <- as.factor(temp$DecRet)
table(temp$DecRisk)
table(temp$DecRet)

temp2 <- Crypto.Famd.ext[HistDAWass==1,.(SYM, rmedio, volatility, MKCapRate, CV, HistDAWass)]
temp2[volatility==0] #Cryptocurrencies with zero volatility in Cluster 1
zerovolatiR.Cluster = nrow(temp2[volatility==0])/nrow(temp2) #Rate of zero volatility

temp2 <- temp2[order(temp2$MKCapRate, na.last = NA, decreasing = TRUE)]
temp2
temp2 <- temp2[order(temp2$CV, na.last = NA, decreasing = TRUE)]
temp2
hist(temp2$CV, breaks = 200)
mean(temp2$CV, na.rm = TRUE)

#Cluster 2
temp <- Crypto.Qualy[HistDAWass==2,.(SYM, PercMKCap, DecRisk, DecRet, HistDAWass)]
temp
temp$PercMKCap <- as.factor(temp$PercMKCap)
temp$DecRisk <- as.factor(temp$DecRisk)
temp$DecRet <- as.factor(temp$DecRet)
table(temp$DecRisk)
table(temp$DecRet)

temp2 <- Crypto.Famd.ext[HistDAWass==2,.(SYM, rmedio, volatility, MKCapRate, CV, HistDAWass)]
temp2[volatility==0] #Cryptocurrencies with zero volatility in Cluster 1
zerovolatiR.Cluster = nrow(temp2[volatility==0])/nrow(temp2) #Rate of zero volatility

temp2 <- temp2[order(temp2$MKCapRate, na.last = NA, decreasing = TRUE)]
temp2
temp2 <- temp2[order(temp2$CV, na.last = NA, decreasing = TRUE)]
temp2
hist(temp2$CV, breaks = 200)
mean(temp2$CV, na.rm = TRUE)

Crypto.Qualy[HistDAWass==2 &
                     PercMKCap=='P99',.(SYM, ClassMKCap, PercMKCap, DecRisk, DecRet, HistDAWass)]

#Cluster 3
temp <- Crypto.Qualy[HistDAWass==3,.(SYM, PercMKCap, DecRisk, DecRet, HistDAWass)]
temp$PercMKCap <- as.factor(temp$PercMKCap)
temp$DecRisk <- as.factor(temp$DecRisk)
temp$DecRet <- as.factor(temp$DecRet)
table(temp$DecRisk)
table(temp$DecRet)

temp2 <- Crypto.Famd.ext[HistDAWass==3,.(SYM, rmedio, volatility, MKCapRate, CV, HistDAWass)]
temp2[volatility==0] #Cryptocurrencies with zero volatility in Cluster 1
zerovolatiR.Cluster = nrow(temp2[volatility==0])/nrow(temp2) #Rate of zero volatility

temp2 <- temp2[order(temp2$MKCapRate, na.last = NA, decreasing = TRUE)]
temp2
temp2 <- temp2[order(temp2$CV, na.last = NA, decreasing = TRUE)]
temp2
hist(temp2$CV, breaks = 200)
mean(temp2$CV, na.rm = TRUE)

#Cluster 4
temp <- Crypto.Qualy[HistDAWass==4,.(SYM, PercMKCap, DecRisk, DecRet, HistDAWass)]
temp$PercMKCap <- as.factor(temp$PercMKCap)
temp$DecRisk <- as.factor(temp$DecRisk)
temp$DecRet <- as.factor(temp$DecRet)
table(temp$DecRisk)
table(temp$DecRet)

temp2 <- Crypto.Famd.ext[HistDAWass==4,.(SYM, rmedio, volatility, MKCapRate, CV, HistDAWass)]
temp2[volatility==0] #Cryptocurrencies with zero volatility in Cluster 1
zerovolatiR.Cluster = nrow(temp2[volatility==0])/nrow(temp2) #Rate of zero volatility

temp2 <- temp2[order(temp2$MKCapRate, na.last = NA, decreasing = TRUE)]
temp2
temp2 <- temp2[order(temp2$CV, na.last = NA, decreasing = TRUE)]
temp2
hist(temp2$CV, breaks = 200)
mean(temp2$CV, na.rm = TRUE)

#Cluster 5
temp <- Crypto.Qualy[HistDAWass==5,.(SYM, PercMKCap, DecRisk, DecRet, HistDAWass)]
temp$PercMKCap <- as.factor(temp$PercMKCap)
temp$DecRisk <- as.factor(temp$DecRisk)
temp$DecRet <- as.factor(temp$DecRet)
table(temp$DecRisk)
table(temp$DecRet)

temp2 <- Crypto.Famd.ext[HistDAWass==5,.(SYM, rmedio, volatility, MKCapRate, CV, HistDAWass)]
temp2[volatility==0] #Cryptocurrencies with zero volatility in Cluster 1
zerovolatiR.Cluster = nrow(temp2[volatility==0])/nrow(temp2) #Rate of zero volatility

temp2 <- temp2[order(temp2$MKCapRate, na.last = NA, decreasing = TRUE)]
temp2
temp2 <- temp2[order(temp2$CV, na.last = NA, decreasing = TRUE)]
temp2
hist(temp2$CV, breaks = 200)
mean(temp2$CV, na.rm = TRUE)

### Clusters TADPOLE #############
Crypto.Qualy[PercMKCap=='P90'&TADPole==1,.(SYM, ClassMKCap, PercMKCap, TADPole)]
#M2 ratio
Crypto.Qualy[PercMKCap=='P100'&TADPole==3 & DecM2=='D4',.(SYM, ClassMKCap, PercMKCap, DecM2, TADPole)]
Crypto.Qualy[PercMKCap=='P100'&TADPole==2 & DecM2=='D10',.(SYM, ClassMKCap, PercMKCap, DecM2, TADPole)]

#### Criptomonedas por Clusters
xtable(Crypto.Qualy[(PercMKCap=='P90' | PercMKCap=='P70')& (ClassBeta=='CashLike') & Kmeans==3,.(SYM, ClassMKCap, PercMKCap, ClassBeta, Kmeans)])
xtable(Crypto.Qualy[(DecM2=='D10'| DecM2=='D9') & PercMKCap=='P80'& TADPole==1,.(SYM, ClassMKCap, PercMKCap, DecM2, TADPole)])
xtable(Crypto.Qualy[(DecM2Sort=='D6'| DecM2Sort=='D7'|DecM2Sort=='D8') & PercMKCap=='P100'& TADPole==3,.(SYM, ClassMKCap, PercMKCap, DecM2Sort, TADPole)])
xtable(Crypto.Qualy[(DecSortRat=='D04') & PercMKCap=='P100'& TADPole==3,.(SYM, ClassMKCap, PercMKCap, DecSortRat, TADPole)])
xtable(Crypto.Qualy[(ClassSharpR=='ERP') & PercMKCap=='P99'& TADPole==2,.(SYM, ClassMKCap, PercMKCap, ClassSharpR, TADPole)])



#### Intersections: Sub-groups  ####################################
#Descriptive stats
#Combi 1:
Crypto.Qualy[Kmeans == 3 &HistDAWass==3 & TADPole == 3,
             .(SYM, ClassMKCap, ClassRet, PercMKCap, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy[Kmeans == 3 &HistDAWass==3 & TADPole == 3 & 
                     PercMKCap=='P100',
             .(SYM, ClassMKCap, ClassRet, PercMKCap, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy[Kmeans == 3 &HistDAWass==3 & TADPole == 3 & 
                     PercMKCap=='P100' & (ClassBeta =='LowVol'),
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, ClassBeta)]

Crypto.Qualy[Kmeans == 3 &HistDAWass==3 & TADPole == 3 & 
                     PercMKCap=='P100' & ClassSharpR =='ERP',
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, ClassSharpR)]

Crypto.Qualy[Kmeans == 3 &HistDAWass==3 & TADPole == 3 & 
                     PercMKCap=='P70' & DecM2=='D10' & DecM2Sort=='D10' & DecSortRat=='D10',
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, 
               DecM2, DecM2Sort, DecSortRat)]


#Combi 2:
Crypto.Qualy[Kmeans == 3 & HistDAWass==3 & TADPole == 2 &
                PercMKCap == 'P99' & ClassRet == 'High',
             .(SYM, ClassMKCap, ClassRet, PercMKCap, HistDAWass)]
Crypto.Qualy[Kmeans == 3 &HistDAWass==3 & TADPole == 2 & 
                     ClassBeta =='CashLike',
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, ClassBeta)]


Crypto.Qualy[Kmeans == 3 &HistDAWass==3 & TADPole == 2 & 
                     PercMKCap=='P100' & (ClassBeta =='LowVol'),
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, ClassBeta)]

#Combi 3:
Crypto.Qualy[Kmeans == 1 & HistDAWass==3 & TADPole == 3 &
                     PercMKCap == 'P99',
             .(SYM, ClassMKCap, ClassRet, PercMKCap, HistDAWass)]
Crypto.Qualy[Kmeans == 1 & HistDAWass==3 & TADPole == 3 &
                     PercMKCap == 'P90' & ClassBeta =='NASDAQlike',
             .(SYM, ClassMKCap, ClassRet, PercMKCap, ClassBeta, Kmeans,HistDAWass,TADPole)]

Crypto.Qualy[Kmeans == 1 &HistDAWass==3 & TADPole == 3 & 
                     PercMKCap=='P99' & ClassSharpR =='ERP',
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, ClassSharpR)]

Crypto.Qualy[Kmeans == 1 &HistDAWass==3 & TADPole == 3 & 
                     PercMKCap=='P80' & DecM2=='D10' & DecM2Sort=='D10' & DecSortRat=='D10',
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, 
               DecM2, DecM2Sort, DecSortRat)]

#Combi 4:
Crypto.Qualy[Kmeans == 1 & HistDAWass==3 & TADPole == 2,
             .(SYM, ClassMKCap, ClassRet, PercMKCap, HistDAWass)]
#Highest MkCap
Crypto.Qualy[Kmeans == 1 & HistDAWass==3 & TADPole == 2 &
                     (PercMKCap=='P100' ),
             .(SYM, ClassMKCap, ClassRet, PercMKCap, HistDAWass)]


Crypto.Qualy[Kmeans == 1 & HistDAWass==3 & TADPole == 2 &
                     ClassRet == 'Average' & PercMKCap =='P100',
             .(SYM, ClassMKCap, ClassRet, PercMKCap, HistDAWass)]

# Higher positive volatility crypto
Crypto.Qualy[Kmeans == 1 &HistDAWass==3 & TADPole == 2 & 
                     (PercMKCap=='P99' | PercMKCap=='P90')  & ClassBeta =='HighVol',
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, ClassBeta)]

# Higher negative volatility
Crypto.Qualy[Kmeans == 1 &HistDAWass==3 & TADPole == 2 & 
                     (PercMKCap=='P99' | PercMKCap=='P90')  & ClassBeta =='NegBeta',
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, ClassBeta)]


#Combi 5:
Crypto.Qualy[Kmeans == 1 & HistDAWass==2 & TADPole == 3 &
                     PercMKCap =='P99',
             .(SYM, ClassMKCap, ClassRet, PercMKCap, HistDAWass)]

Crypto.Qualy[Kmeans == 1 & HistDAWass==2 & TADPole == 3 &
                     ClassRet =='Average',
             .(SYM, ClassMKCap, ClassRet, PercMKCap, HistDAWass)]

Crypto.Qualy[Kmeans == 1 & HistDAWass==2 & TADPole == 3 &
                     ClassRet =='',
             .(SYM, ClassMKCap, ClassRet, ClassRisk, PercMKCap, HistDAWass)]

#Combi 6:
Crypto.Qualy[Kmeans == 1 & HistDAWass==2 & TADPole == 2 &
                     PercMKCap =='P100',
             .(SYM, ClassMKCap, ClassRet, PercMKCap, HistDAWass)]

Crypto.Qualy[Kmeans == 1 & HistDAWass==2 & TADPole == 2 &
                     PercMKCap =='P90',
             .(SYM, ClassMKCap, ClassRet, PercMKCap, HistDAWass)]

# Higher positive volatility crypto
Crypto.Qualy[Kmeans == 1 &HistDAWass==2 & TADPole == 2 & 
                     (PercMKCap=='P99' | PercMKCap=='P90')  & ClassBeta =='HighVol',
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, ClassBeta)]

# Higher negative volatility
Crypto.Qualy[Kmeans == 1 &HistDAWass==2 & TADPole == 2 & 
                     (PercMKCap=='P99' | PercMKCap=='P90')  & ClassBeta =='NegBeta',
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, ClassBeta)]
# Sortino ratio
Crypto.Qualy[Kmeans == 1 &HistDAWass==2 & TADPole == 2 & 
                     (PercMKCap=='P80' | PercMKCap=='P90')  & (DecSortRat =='D4' | DecM2 == 'D4'),
             .(SYM, Kmeans, HistDAWass, TADPole, PercMKCap, DecSortRat, DecM2)]

####    + Associations
####   Financial ratios
Crypto.Qualy[Kmeans == 3 &HistDAWass==3 & TADPole == 3 & 
                     PercMKCap=='P70' & ClassSharpR =='ERP' &
                     DecM2Sort == 'D10' & DecM2 == 'D10'& DecSortRat =='D10',
                    .(SYM, ClassMKCap, PercMKCap, Kmeans, HistDAWass, TADPole)]
#Beta

Crypto.Qualy[Kmeans == 1 &HistDAWass==3 & TADPole == 3 & 
                     PercMKCap=='P100' & (ClassBeta =='CashLike'|ClassBeta =='LowVol'|ClassBeta=='NASDAQlike'),
             .(SYM, Kmeans, HistDAWass, TADPole, ClassMKCap, PercMKCap, ClassBeta)]

### ASSOCIATIONS of AGE
Crypto.Qualy[Kmeans == 3 &
                     PercMKCap=='P70' & DecAge =='D4' ,
             .(SYM, Kmeans, HistDAWass, ClassMKCap, PercMKCap, DecAge)]

Crypto.Qualy[HistDAWass == 1 
                      ,
             .(SYM, Kmeans, HistDAWass, ClassMKCap, PercMKCap, DecAge)]


Crypto.Qualy[Kmeans == 3 & HistDAWass==1 & 
                     PercMKCap=='P70' & DecAge =='D4' ,
             .(SYM, Kmeans, HistDAWass, ClassMKCap, PercMKCap, DecAge)]
