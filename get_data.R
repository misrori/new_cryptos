# get_crypto_lsit
library(data.table)
library(jsonlite)
library(lubridate)
adat <- data.table(fromJSON("https://api.coinmarketcap.com/v1/ticker/?limit=0"))

regi<- read.csv("regi_coins.txt", stringsAsFactors = F, header = F)
names(regi)<- "ticker"
regiek <- unique(regi$ticker)
adat <-adat[symbol %in%regiek==F,]


ket_honap<-as.Date(as.POSIXct(Sys.Date())-lubridate::days(60) )

get_one_coin <- function(coin){
  print(coin)
  link<- paste('https://min-api.cryptocompare.com/data/histoday','?fsym=',coin,'&tsym=USD&limit=2000',sep ="")
  adat <- fromJSON(link)
  if(adat$Response=="Success"){
    adat<- data.table(adat$Data)
    adat<- adat[high!=0&close!=0&low!=0,]
    adat$time <- as.POSIXct(adat$time, origin="1970-01-01")
    adat$symbol <- coin
    adat <- adat[,c("symbol","time",'open')]
    if(as.Date(min(adat$time))>ket_honap){
      write.csv(adat, paste0('friss_coin/', coin, '.csv'), row.names = F)
      return("friss")
    }else{
      write(coin, file = 'regi_coins.txt', append = T)

      return("regi")
    }
    
  }else{
    return("nincs adat")
  }
}

sapply(adat$symbol, get_one_coin)
