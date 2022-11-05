#####################################################################
#                    Data Wrangling
#        Get, Clean and Transform of the Data
#####################################################################
# Source CRPTg3_DSv3.R


library("reshape2")
library("data.table")

#Setting of the folder with the datasets
setwd('D:/DOCTORADO/Scripts/R/Entrega/Datasets/')
CRYP.DS <- read.table("DS_OHLCV19Final31012019.csv",header=TRUE, sep=",")

#Format and datatype conversions:
CRYP.DS$time<-as.Date(CRYP.DS$time) 
CRYP.DS$close<-as.double(CRYP.DS$close)
CRYP.DS$volumefrom<-as.double(CRYP.DS$volumefrom)
CRYP.DS$volumeto<-as.double(CRYP.DS$volumeto)
CRYP.DT <- as.data.table(CRYP.DS)

##################################### VARIABLE DESCRIPTIONS ######################################
# Variables: all prices in USD
# close: daily close price 
# high: daily highest price
# open: daily open price
# low: daily lowest price
#XRP-USD pair VOLUMETO means the volume in the currency that is being traded (i.e. USD)
#XRP-USD pair VOLUMEFROM means the volume in the base currency that things are traded into (i.e. XRP).
#       lets say you are looking at the Doge/BTC market.
#       the volumeto is the Doge volume for example 1,000,000
#the VOLUMEFROM is the BTC volume which if we assume Doge price was 100 fixed it will be 10,000
#SYM: cryptocurrency ticket

# ##################  OUTLIERS TREATMENT and TIMEFRAME FILTERING ################################

# CLEANING 1: duplicated registers
CRYP.DT.Filtered<- unique(CRYP.DT)
#Gross return: Rg(t)=S(t)/S(t-1)
#Continous compound return: rend = r(t) = log(S(t)/S(t-1)) = log(S(t)) - log(S(t-1));
#Simple Net return: Rt(t)=S(t)/S(t-1) - 1 = exp(Rg(t)) - 1

CRYP.RT.short <- CRYP.DT.Filtered[,.(time, close, volumefrom, volumeto, rend = log(close)-log(close[-1]),
                                  Rt = (close - close[-1])/close[-1]), by=SYM]

### Market cap and Volume variables declaration on the table ################################ 
## By definition, VOLUMETO variable equal to the market map in USD of the cryptocurency,
# and VOLUMETO is referred to the traded volume based on the cryptocurrency
setnames(CRYP.RT.short,c('volumeto','volumefrom'),c('marketcap','volume'))

# CLEANING 2: Remove of NaN registers
CRYP.RT.short <- na.omit(CRYP.RT.short)

# CLEANING 3: Remove of Rend with Inf values 
CRYP.RT.short <- CRYP.RT.short[is.finite(rend),]

# FILTER 1: Only registers of 2018
CRYP.RT.short <- CRYP.RT.short[time >='2017-12-31' & time <='2018-12-31'] 

# FILTER 2: Only cryptoasset that were traded most of the days along all the year 2018 taken BTC as reference 
Crypt19 <- CRYP.RT.short[,.N,by=SYM]
Crypt19$SYM <- as.character(Crypt19$SYM)
Nmin <- Crypt19[SYM=='BTC']$N
Crypt19 <- Crypt19[N==Nmin] #Nos quedamos con las que han cotizado 366 dias (los mismo que el BTC)
CRYP.RT.short <- merge(CRYP.RT.short,Crypt19, by="SYM")
CRYP.RT.short <- CRYP.RT.short[,.(SYM,time,close, volume, marketcap, rend, Rt)]

###################################################################################################
####             Table transformations: from long-format to wide-format                        ####
####             Variables: CLOSE, VOLUME and REND                                             ####
####Comments: some of the following tables will be used by the different clustering R functions####
###################################################################################################


CRYP.wDT.close <- dcast.data.table(CRYP.RT.short, time ~ SYM, value.var = "close")
CRYP.wDT.volume <- dcast.data.table(CRYP.RT.short, time ~ SYM, value.var ="volume")
CRYP.wDT.rend <- dcast.data.table(CRYP.RT.short, time ~ SYM, value.var ="rend")

CRYP.wDF.close <- as.data.frame(CRYP.wDT.close) 
CRYP.wDF.volume <- as.data.frame(CRYP.wDT.volume) 
CRYP.wDF.rend <- as.data.frame(CRYP.wDT.rend) 

CRYP.timeWise.rend <- t(CRYP.wDT.rend)
colnames(CRYP.timeWise.rend) <- CRYP.timeWise.rend[1,]
CRYP.timeWise.rend <- CRYP.timeWise.rend[-1,]
CRYP.timeWise.price <- t(CRYP.wDT.close)
colnames(CRYP.timeWise.price) <- CRYP.timeWise.price[1,]
CRYP.timeWise.price <- CRYP.timeWise.price[-1,]

temp1 <- unlist(CRYP.timeWise.rend)
temp2 <- as.numeric(temp1)
temp3 <- matrix(temp2, byrow = TRUE, nrow = nrow(temp1))
colnames(temp3) <- colnames(CRYP.timeWise.rend)
rownames(temp3) <- rownames(CRYP.timeWise.rend) 
CRYP.timeWise.rend <- temp3

temp1 <- unlist(CRYP.timeWise.price)
temp2 <- as.numeric(temp1)
temp3 <- matrix(temp2, byrow = TRUE, nrow = nrow(temp1))
colnames(temp3) <- colnames(CRYP.timeWise.price)
rownames(temp3) <- rownames(CRYP.timeWise.price) 
CRYP.timeWise.price <- temp3

#Creation of a list of cryptocurrencies with  daily quotation, returns and marketcap
# <funcCrypto> return a list with following objects:
#1) Dataframe1: CRYPT ticket, first and last day of quotation, traded days
#2) Dataframe2: time-series of quotation (close, timestamp)
#3) Dataframe3: time-series of marketcap (marketcap, timestamp)
#4) Dataframe4: time-sries of daily returns
funCrypto <- function(n1){
        nz <- CRYP.wDF.close[,colnames(CRYP.wDF.close)[n1]]
        nz2 <- CRYP.wDF.volume[,colnames(CRYP.wDF.volume)[n1]]
        nz3 <- CRYP.wDF.rend[,colnames(CRYP.wDF.rend)[n1]]
        
        rg.fech <- CRYP.wDF.close$time[!is.na(nz)]
        first_fech <- rg.fech[1]
        last_fech <- tail(rg.fech, n=1)
        
        basicInf.df <- data.frame(colnames(CRYP.wDF.close)[n1], first_fech, last_fech, length(rg.fech))
        colnames(basicInf.df) <- c('CRYP','First','Last','Samples')
        serieClose.df <- cbind.data.frame(rg.fech,na.omit(nz))
        colnames(serieClose.df) <- c('Fecha','Close')
        serieVolume.df <- cbind.data.frame(rg.fech,na.omit(nz2))
        colnames(serieVolume.df) <- c('Fecha','Volume')
        serieRend.df <- cbind.data.frame(rg.fech,na.omit(nz3))
        colnames(serieRend.df) <- c('Fecha','Rend')
        result <- list(Crypt=colnames(CRYP.wDF.close)[n1], BasicInfo=basicInf.df,
                       serieclose=serieClose.df, serieVolume=serieVolume.df, serieRend=serieRend.df)        
        
        return(result)
}

#We generate a list of each cryptocurrency
n1 <- nrow(Crypt19)
CRYP.LST <- lapply(1:n1,funCrypto)


##################################   Format adaptation  ########################################
library('xts')
CRYPrice_xts <- as.xts(CRYP.wDT.close) #Returns table in XTS format (requires xts library)


###############################################################################################
#                        Treatment of cryptocurrency description file                         #
###############################################################################################
#File 1: One sample of information of each cryptocurrency
COINTYPE19.DS <- read.table("Coins2019FINAL.txt")
COINTYPE19.DT <- as.data.table(COINTYPE19.DS)

TypeField <- grep('TYPE',COINTYPE19.DT$V1)
COINTYPE19.DT[TypeField]=NA
COINTYPE19.DT <- na.omit(COINTYPE19.DT)

SymbolField <- grep('FROMSYMBOL',COINTYPE19.DT$V1)

seqSimb <- c()
toString(seqSimb)

for (i in SymbolField){
        simbolo <- rep(toString(COINTYPE19.DT$V2[i]),36)
        seqSimb <- append(seqSimb, simbolo)
}

COINTYPE19.DT$V3 <- seqSimb
CRYPTO19.DT <- dcast.data.table(COINTYPE19.DT, V3 ~ V1, value.var='V2')
indx <- colnames(CRYPTO19.DT)
CRYPTO19.DT$MKTCAP <- as.numeric(as.character(CRYPTO19.DT$MKTCAP))

#Outlier
#CRYPTO19.DT <- CRYPTO19.DT[V3 != 'HIVE']

# File 2: Descriptive information of each cryptocurrency (source in JSON format)
# Id: Index indentifier
# Url, Name, Symbol, Full name, Algortihm, Prooftype,...
library(rjson)
CoinCharact19.lst <- fromJSON(file = 'CoinCharactFINAL2.json')

#Transform from List to Dataframe
library(plyr)
CoinCharact19.df <- ldply(CoinCharact19.lst, data.frame)
temp <- merge(CRYPTO19.DT, CoinCharact19.df, by.x = 'FROMSYMBOL', by.y = 'Symbol',
              all.x = TRUE)
CRYPTO19.DT <- as.data.table(temp[,c('FROMSYMBOL','Id','MKTCAP', 'SUPPLY','TotalCoinSupply','Url',
                                     'Algorithm','ProofType')])
CRYPTO19.DT$FROMSYMBOL <- as.character(CRYPTO19.DT$FROMSYMBOL)
CRYPTO19.DT$Id <- as.character(CRYPTO19.DT$Id)
CRYPTO19.DT$SUPPLY <- as.numeric(CRYPTO19.DT$SUPPLY)
CRYPTO19.DT$TotalCoinSupply <- as.numeric(CRYPTO19.DT$TotalCoinSupply)
CRYPTO19.DT$Url <- as.character(CRYPTO19.DT$Url)

# Market cap computed as median value of Marketcap (VOLUMETO) variable for each cryptocurrency
# Incluimos el Marketcap mediano por criptomoneda durante 2018 (en lugar de median para evitar outliers)
temp <- CRYP.RT.short[,.(median(marketcap)), by = SYM]
CRYPTO19.DT <- merge(CRYPTO19.DT,temp, all.y = TRUE, by.x = 'FROMSYMBOL', by.y = 'SYM' )
setnames(CRYPTO19.DT,'V1','mkcap18')


#############################################################################
#             END OF CRYPTOCURRENCIES TREATMENT                             #
#############################################################################



#################################################################################
#                         SP500 INDEX TREATMENT                                 #
#################################################################################
# Data Transformation of time-series to use Portfolio library
SP500Index <- read.table("SP500Index.csv",header=TRUE, sep=",")
SP500Index_zoo <- read.csv.zoo('SP500Index.csv', header=TRUE, sep=',', format='%Y-%m-%d')
SP500Index_xts <- as.xts(SP500Index_zoo)

library('PerformanceAnalytics')
SP500Index_xts$RtSP <- Return.calculate(SP500Index_xts$Adj.Close)

SP500Index$Date <- as.Date(SP500Index$Date)
SP500Index.DT <- as.data.table(SP500Index)
SP500Index.DT <- SP500Index.DT[order(-rank(Date))]


# Transform dayly returns to continous dayly returns transforming by Log (ln) function
SP500Index.DT <- SP500Index.DT[,.(Date, Open, High, Low, Close, Adj.Close, Volume,
                        rendSP = log(Adj.Close)-log(Adj.Close[-1]),
                        RtSP = (Adj.Close - Adj.Close[-1])/Adj.Close[-1])]
setnames(SP500Index.DT,'Date','time')

### End of SP500 treatment ################################################

########### Fixed rate investment instruments ############################
################ U.S. treasury 10 years #################################
Bono10.DS <- read.table("Bono10.csv",header=TRUE, sep=";")
Bono10.DT <- as.data.table(Bono10.DS)
Bono10.DT$Date <- as.Date(Bono10.DT$Date, tryFormats=c("%m/%d/%Y"))
Bono10.DT <- Bono10.DT[order(-rank(Date))]
names(Bono10.DT)<- c('Date','X10YRate')

Bono10.zoo<- read.csv.zoo('Bono10.csv', header=TRUE, sep=';', format='%m/%d/%Y')
Bono10.xts<- as.xts(Bono10.zoo)
names(Bono10.xts)<-"Y10Rate"
Bono10.xts$Rf<-Return.calculate(Bono10.xts$Y10Rate)

# Log returns
# Variables:
# rendB10: continous compound rate return
# RtB10: simple interest rate
Bono10.DT <- Bono10.DT[,.(Date, X10YRate,
                                  rendB10 = log(X10YRate)-log(X10YRate[-1]),
                          RtB10 = (X10YRate - X10YRate[-1])/X10YRate[-1])]

############## U.S. Treasury 90 days #####################################
Letra90.DS <- read.table("TreasuryBill90.csv",header=TRUE, sep=";")
Letra90.DT <- as.data.table(Letra90.DS)
names(Letra90.DT) <- c('Date', 'X90dayTreasuryBill')
Letra90.DT$Date <- as.Date(Letra90.DT$Date, tryFormats=c("%m/%d/%Y"))

################ Dataset del NASDAQ COMPOSITE ###################################
NasdaQ.DS <- read.table("NasdaqComposite2018.csv", header = TRUE, sep = ",")
NasdaQ.DT <- as.data.table(NasdaQ.DS)

NasdaQ.DT$Date <- as.Date(NasdaQ.DT$Date, tryFormats=c("%Y-%m-%d"))
NasdaQ.DT <- NasdaQ.DT[order(-rank(Date))]

NasdaQ.zoo <- read.csv.zoo("NasdaqComposite2018.csv", header = TRUE, sep = ",",
                         format = '%Y-%m-%d')
NasdaQ.xts <- as.xts(NasdaQ.zoo)
NasdaQ.xts$RtNSQC <- Return.calculate(NasdaQ.xts$Adj.Close)

# Rendimientos
NasdaQ.DT <- NasdaQ.DT[,.(Date, Adj.Close, 
                          RtNSQC = (Adj.Close - Adj.Close[-1])/Adj.Close[-1])]

names(NasdaQ.DT) <- c('Date','AdjClose','RtNSQ')

######## VARIABLES DE USO POR OTROS SCRIPTS #####################################
# Salvamos las principales variables de los diferentes datasets
CRYP.RT <- CRYP.RT.short

# Dataset 2018
nfich1 <- c("CRYPT_DS_18v2g3F.RData")
# save(CRYPrice_xts, CRYP.DT.Filtered, CRYP.RT, CRYP.LST, CRYP.timeWise.rend, 
#       CRYP.timeWise.price, file=nfich1)
#load(nfich1)

# Dataset de Criptomonedas ########################################################
nfich2 <- c("CoinsDescr19_18v2g3F.RData")
#save(CRYPTO19.DT, file=nfich2)

# Dataset de SP500 Index ########################################################
nfich3 <- c("SP500IndexV2F.RData")
#save(SP500Index.DT, SP500Index_xts, file=nfich3)

# Dataset Indices de references ########################################################
nfich4 <- c("IndexesRef.RData")
# save(SP500Index.DT, SP500Index_xts, Bono10.DT, Bono10.xts, NasdaQ.DT,
#     NasdaQ.xts, Letra90.DS, Letra90.DT, file=nfich4)
