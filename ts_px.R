dataPX <- read.csv("~/Downloads/historicalData_PX.csv", sep=";")
dataP <- dataPX[,c(1,3)]
colnames(dataP) <-c("datum", "cena")
dataP$datum <-as.Date(dataP$datum, "%d.%m.%Y")
dataP$cena <- sub(",",".",dataP$cena)
dataP$cena <- as.numeric(dataP$cena)
min(dataP[,2])
timeseries <- ggplot(data=dataP, aes(x=datum, y=cena)) +
  geom_line() +
  #geom_abline(intercept= mean(fin$cena[which(fin$kdo=="Adam")]),slope=0, lwd=1,linetype="dashed",color=wes_palette("Darjeeling")[1])+
  #geom_abline(intercept= mean(fin$cena[which(fin$kdo=="Anežka")]),slope=0, lwd=1,linetype="dashed",color=wes_palette("Darjeeling")[2])+
  geom_smooth(aes(group = 1),method = "lm", se = FALSE, colour ="black",linetype="dotted")+
  theme_hc() +
  scale_color_manual(values = wes_palette("Darjeeling"))+
  theme(legend.position="top")+
  labs(x = "Datum", y = "Cena") 
  #scale_x_date(labels = date_format("%Y-%m-%d"))
ggplotly(timeseries)

consumption$date <- paste(consumption$rok,"-",substr(consumption$q,2,2), sep="")
consumption$date <-as.yearqtr(consumption$date)

timeseries2 <- ggplot(data=consumption, aes(x=date, y=consumption)) +
  geom_line() +
  #geom_abline(intercept= mean(fin$cena[which(fin$kdo=="Adam")]),slope=0, lwd=1,linetype="dashed",color=wes_palette("Darjeeling")[1])+
  #geom_abline(intercept= mean(fin$cena[which(fin$kdo=="Anežka")]),slope=0, lwd=1,linetype="dashed",color=wes_palette("Darjeeling")[2])+
  #geom_smooth(aes(group = 1),method = "lm", se = FALSE, colour ="black",linetype="dotted")+
  theme_hc() +
  scale_color_manual(values = wes_palette("Darjeeling"))+
  theme(legend.position="top")+
  labs(x = "Datum", y = "Cena") 
#scale_x_date(labels = date_format("%Y-%m-%d"))
ggplotly(timeseries2)

spotreba_stale_ceny <- na.omit(spotreba_stale_ceny)

spotreba_bezne_ceny <- spotreba_bezne_ceny[1:82,]

spotreba_stale_ceny$deflator <- spotreba_stale_ceny$cons_adj/spotreba_bezne_ceny$cons_un