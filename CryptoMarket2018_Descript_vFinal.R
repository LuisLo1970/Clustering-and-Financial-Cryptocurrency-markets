###################################################################
#                Cryptocurrency market evolution
###################################################################
#Source: CRYPTO_DS_v5

#Librerias
library("reshape2")
library("data.table")
library("ggplot2")

#Setting of the folder with the datasets
CRYP.DS <- read.table("DS_OHLCV19Final31012019.csv",header=TRUE, sep=",")
CRYP.DS$time=as.Date(CRYP.DS$time) #Conversion de formato de Factor a Date
CRYP.DT <- as.data.table(CRYP.DS)

################################################################################################
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
##################################################################################################

################## Dataset cleanning: #############################
#1) Register with zero value in CLOSE, HIGH and OPEN are removed
CRYP.DT.Filtered <- CRYP.DT[open !=0 & close!=0 & high!=0 & low!=0]
#2) Duplicated registers
CRYP.DT.Filtered <- unique(CRYP.DT.Filtered)

################# Creation of some support tables ##################
CRYP.DT.short <- CRYP.DT.Filtered #Table with clean Dataset
#Wide-format tables for the variables CLOSE and VOLUMETO
CRYP.wDT <- dcast.data.table(CRYP.DT.short, time ~ SYM, value.var = "close")
CRYP.wDT.vol <- dcast.data.table(CRYP.DT.short, time ~ SYM, value.var ="volumeto")


######### Descriptive plots of the cryptocurrency market
# Days quoted for each cryptocurrency ###############
nCryptoDay.vc <- colSums(!is.na(CRYP.wDT)) 
hist(sort(nCryptoDay.vc, decreasing = TRUE), main = 'Quotation frequency', 
     xlab = 'Quoted days', 
     ylab = 'Number of cryptocurrencies', breaks = 50)

plot(sort(nCryptoDay.vc), main = 'Quoted days by cryptocurrency',
     xlab = 'Cryptocurrencies',
     ylab = 'Quoted days')

# Number of quoted cryptocurrencies (cryptocurencies in the market dayly traded)
nCryptoDay2.vc <- rowSums(!is.na(CRYP.wDT))-1 #Numero de criptomonedas cotizadas por dia (restamos la columna fecha)

plot(CRYP.wDT$time, nCryptoDay2.vc, pch=23,cex=0.3,col='blue',
     main = 'Different cryptocurrencies daily traded',
     xlab = 'Date',
     ylab = 'Number of cryptocurrencies')


#Table with the number of cryptocurrencies quoted every day
NumCrypt <- c('')
NumCrypt<- as.character(CRYP.wDT$time)
NumCrypt <- cbind(NumCrypt,nCryptoDay2.vc)
colnames(NumCrypt) <- c('Date','N')
NumCrypt <- as.data.frame(NumCrypt, stringsAsFactors=FALSE)
NumCrypt$Date <- as.Date(NumCrypt$Date)
NumCrypt$N <- as.integer(NumCrypt$N)
NumCrypt <- as.data.table(NumCrypt)
NumCrypt <- NumCrypt[,.(Date, N, N30 = diff(N,30))]

#Figure 1. Number of new cryptocurrencies (red) and its respective cumulative
#number since 2010
ggplot()+
        geom_line(data=NumCrypt[,.(Date,N)],aes(x=Date, y=N), color="blue") +
        geom_line(data=NumCrypt[,.(Date,N30)],aes(x=Date, y=N30*2700/300), color="red")+
        scale_y_continuous(name = "Accumulated number of cryptocurrencies", 
                           limits = c(0, 2700),
                           sec.axis = sec_axis(~ . *300/2700, name = "30-days increment of cryptoc."))+
        theme(
        #        plot.margin = margin(2,2,2,2, "cm"),
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x = element_text(size=12),
                axis.title.y = element_text(colour = "blue", size = 15, 
                                            margin = margin(t = 0, r = 15, b = 0, l = 0)),
                axis.title.y.right = element_text(colour = "red", size = 15, 
                                                  margin = margin(t = 0, r = 0, b = 0, l = 15))
        ) +
        theme(plot.title = element_text(hjust = 1))

mean(NumCrypt[Date>'2017-12-31',N30])
mean(NumCrypt[,N30])
