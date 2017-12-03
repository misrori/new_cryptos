library(data.table)
library(jsonlite)
library(lubridate)
library(plotly)

my_list<- paste0("friss_coin/",list.files("friss_coin/"))

my_lista<- list()
for(i in my_list){
 my_lista[[i]]<- fread(i)   
}

adatom<- rbindlist(my_lista)
names(adatom)[3]<-"close"

list_of_markets <- unique(adatom$symbol)

setorder(adatom, symbol, time)
print(str(adatom))

for (i in list_of_markets) {
  baseline <- adatom[symbol == i, close][1]
  adatom[symbol == i, change := (close/baseline-1)*100]
}
adatom<- adatom[symbol%in%unique(adatom[change>1000,]$symbol)==F]



tozsde_plot <- function(adatom,min_date,max_date){

  adatom <- adatom[as.Date(adatom$time)>=as.Date(min_date) & as.Date(adatom$time)<=as.Date(max_date),]
  
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "Date time",
    titlefont = f
  )
  y <- list(
    title = "Change (%)"
  )
  
  m <- list(
    l = 100,
    r = 80,
    b = 10,
    t = 150,
    pad = 4
  )
  p<-plot_ly(adatom, x = ~time, y = ~change, color =~symbol, text= ~paste('$',close))%>%
    add_lines()%>%layout( xaxis = x, yaxis = y)
  
  return(p)
  
}