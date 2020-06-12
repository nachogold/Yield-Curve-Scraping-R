#SCRAPING - YIELD CURVE
#Juan Ignacio Goldberg and Felipe Moronta

#install.packages("selectr")
#install.packages("xml2")
#install.packages("rvest")
#install.packages("tidyverse")

library(selectr)
library(xml2)
library(rvest)
library(tidyverse)


TO_21="https://finance.yahoo.com/quote/TO21.BA?p=TO21.BA"
TO_23="https://finance.yahoo.com/quote/TO23.BA?p=TO23.BA"
TO_26="https://finance.yahoo.com/quote/TO26.BA?p=TO26.BA"
bonos=c(TO_21,TO_23,TO_26)


info_bonos=function(bonos){
  aux=list()
fecha_vto=c()
tir_bonos=c()
  for (i in 1:length(bonos)) {
webpage <- read_html(bonos[i])
#Header
header=webpage %>%
  html_nodes("h1") %>%
  html_text()
header=strsplit(header, " ")
header=header[[1]]

nombre_bono=header[1]

#tabla
url=bonos[i]
tabla <- url %>% 
  read_html() %>% 
  html_table(header = FALSE) %>% 
  map_df(bind_cols) %>% 
  as_tibble()

#Hallar fecha vto
hoy=as.Date(Sys.time())
if(header[1]=="TO23.BA"){
  vto=as.Date("2023-10-17")
}else{if(header[1]=="TO26.BA"){
  vto=as.Date("2026-10-17")
}else{
  vto=as.Date(header[6],format="%d/%m/%y")}}

periodos=ceiling(as.numeric((vto-hoy)/180))
f_cupon=rev(seq(vto, length=periodos, by="-6 month"))
d_cupon=as.numeric(f_cupon-hoy)
fecha_vto=c(fecha_vto,round(tail(d_cupon,1)/365.25,2))

#Vector de cashflows
price=as.numeric(tabla[1,2])
cupon=as.numeric(strsplit(header[str_detect(header,".%")],"%")[[1]])
cashflows=rep(0,(length(d_cupon)+1)) 
cashflows[1]=-price
cashflows[2:length(d_cupon)]=cupon/2 
cashflows[length(d_cupon)+1]=(100+(cupon/2))

cupones=data.frame(f_cupon,cashflows[-1])
colnames(cupones)=c("Fecha de cupon","Monto")

#Calculo NPV

van<-function(cashflows,d_cupon,i){
  d_cupon=c(0,d_cupon)/365.25
  van<-sum(cashflows*((1+i)^(-d_cupon)))
  van
}

#Calculo IRR
tir<-function(error=0.001,a=0,b=5){
  options(digits=15)
 f<-function(x){
  d_cupon=c(0,d_cupon)/365.25
  van<-sum(cashflows*((1+x)^(-d_cupon)))
  return(van)
 }
 error_e=error+0.1
 while(error_e>error){
  x<-(a+b)/2   
  if(f(a)*f(x)<0){
   b<-x
   error_e=abs(f(x))
  }else{
   a<-x
   error_e=abs(f(x))
  }
 }
 return(round(x,6))
}
tir=tir()*100
tir_bonos=c(tir_bonos,tir)
lista_i=list(Info=tabla,Cupones=cupones,TIR=tir)
  aux[[nombre_bono]]=lista_i
  }
fecha_vto=as.numeric(fecha_vto)
plot(fecha_vto,tir_bonos,xlab = "Years to Maturity",
     ylab = "Yield (%)",
     type = "o",
     main = paste("Argentina BOTES Yield Curve as of",Sys.Date(),"(Previous Close)"),ylim=c(min(tir_bonos)-5,max(tir_bonos)+5))
grid(col = 2)
return(aux)
}

serieTO=info_bonos(bonos)