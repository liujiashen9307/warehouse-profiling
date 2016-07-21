rm(list = ls())
library(ggplot2)
library(sqldf)
library(gridExtra)
require(ggplot2)
require(sqldf)
setwd("C:/Users/apple/Desktop/R Practice/Warehouse Profiling")
spr_lines<-read.csv("Spr_lines.csv")
Spr_Skus<-read.csv("Spr_skus.csv")
#####Warehouse Profiling by SQL#####
####Where are they stored#####
Spr_Skus$Aisle<-as.numeric(Spr_Skus$Aisle)
Spr_Skus$Bay<-as.numeric(Spr_Skus$Bay)
Byzone<-sqldf("SELECT Zone,count(Sku_id) from Spr_Skus group by ZOne order by count(Sku_id)")
names(Byzone)[2]<-"Amount"
ByAisle<-sqldf("SELECT Aisle,count(Sku_id) from Spr_Skus group by Aisle order by Aisle")
names(ByAisle)[2]<-"Amount"
Byboth<-sqldf("SELECT Zone,Aisle,count(Sku_id) from Spr_Skus group by Zone,Aisle order by Aisle ")
names(Byboth)[3]<-"Amount"
#####PLOT#####
a1<-ggplot(data=Byzone,aes(x=Zone,y=Amount))+geom_bar(position = "dodge",stat = "identity")+ggtitle("SkusByZone")
a2<-ggplot(data=ByAisle,aes(x = Aisle,y=Amount))+geom_bar(position = "dodge",stat = "identity")+ggtitle("SkusByAisle")
a3<-ggplot(data=Byboth,aes(x = Zone,y=Aisle))+geom_tile(aes(fill=Amount),color="white")+scale_fill_gradient(low="yellow",high="red")+ggtitle("SkuByBoth")
#####Which are the most popular#####
Newform<-merge(spr_lines,Spr_Skus)
####By order frequency####
ByFreq<-sqldf("SELECT Sku_id,count(Order_id) from Newform group by Sku_id order by count(Order_id)")
names(ByFreq)[2]<-"OrderFreq"
####By order quantity######
ByQuant<-sqldf("SELECT Sku_id,sum(Order_qty) from Newform group by Sku_id order by sum(Order_qty)")
names(ByQuant)[2]<-"Ship_qty"
####Amount of skus never requested####
ReqSku<-sqldf("SELECT Sku_id,count(Sku_id) from Newform group by Sku_id")
Amount<-length(Spr_Skus$Sku_id)-length(ReqSku$Sku_id)
####Assortment change overtime########
Trend<-sqldf("SELECT DOFT,count(Sku_id) as Assortment from Spr_Skus group by DOFT")
for(n in (1:length(Trend$DOFT)))
{
  if (n >1){
    Trend[n,2]<-Trend[n,2]+Trend[n-1,2]
  }
}
Trend<-Trend[-1,]
#####PLOT######
#####Work description#####
####Where are they#####
WorkZone<-sqldf("SELECT Zone,sum(Ship_qty) from Newform group by Zone order by sum(Ship_qty)")
WorkAisle<-sqldf("SELECT Aisle,sum(Ship_qty) from Newform group by Aisle order by sum(Ship_qty)")
WorkSku<-sqldf("SELECT Sku_id,sum(Ship_qty) from Newform group by Sku_id order by sum(Ship_qty)")
WorkBay<-sqldf("SELECT Bay,sum(Ship_qty) from Newform group by Bay order by sum(Ship_qty)")
PPR<-sqldf("SELECT Zone,Aisle,Bay,sum(Ship_qty) from Newform group by Zone,Aisle,Bay order by Aisle,Bay")
names(PPR)[4]<-"Amount"
WInZoneA<-PPR[which(PPR$Zone=="A"),];PLWA<-ggplot(data = WInZoneA,aes(x = Aisle,y=Bay))+geom_point(aes(size=Amount),color="Black")+ggtitle("WorkinZoneA")
WInZoneB<-PPR[which(PPR$Zone=="B"),];PLWB<-ggplot(data = WInZoneB,aes(x = Aisle,y=Bay))+geom_point(aes(size=Amount),color="Black")+ggtitle("WorkinZoneB")
WInZoneC<-PPR[which(PPR$Zone=="C"),];PLWC<-ggplot(data = WInZoneC,aes(x = Aisle,y=Bay))+geom_point(aes(size=Amount),color="Black")+ggtitle("WorkinZoneC")
WInZoneD<-PPR[which(PPR$Zone=="D"),];PLWD<-ggplot(data = WInZoneD,aes(x = Aisle,y=Bay))+geom_point(aes(size=Amount),color="Black")+ggtitle("WorkinZoneD")
WInZoneE<-PPR[which(PPR$Zone=="E"),];PLWE<-ggplot(data = WInZoneE,aes(x = Aisle,y=Bay))+geom_point(aes(size=Amount),color="Black")+ggtitle("WorkinZoneE")
WInZoneG<-PPR[which(PPR$Zone=="G"),];PLWG<-ggplot(data = WInZoneG,aes(x = Aisle,y=Bay))+geom_point(aes(size=Amount),color="Black")+ggtitle("WorkinZoneG")
sss<-ggplot(PPR,aes(x=Aisle,y=Bay))+geom_point(aes(size=Amount))+facet_wrap(~Zone)
coordpolarprac<-ggplot(PPR,aes(x=Zone,y=Amount))+geom_bar(stat = "identity",width = 1)+coord_polar(theta = "x",direction = 1)
coordpolarprac
#Order more than 20 lines####
Total<-sqldf("SELECT Order_id,count(Order_id) from spr_lines group by Order_id")
Twentity<-Total[which(Total$`count(Order_id)`>=20),]
Percentage<-length(Twentity$Order_id)/length(Total$Order_id)
####Distribution of lines per zone######
TotalDit<-sqldf("SELECT Order_id,Zone,count(Order_id) from Newform group by Order_id,Zone order by count(Order_id)")
names(TotalDit)[3]<-"NumberofLines"
DitA<-TotalDit[which(TotalDit$Zone=="A"),];DitB<-TotalDit[which(TotalDit$Zone=="B"),];DitC<-TotalDit[which(TotalDit$Zone=="C"),]
DitD<-TotalDit[which(TotalDit$Zone=="D"),];DitE<-TotalDit[which(TotalDit$Zone=="E"),];DitG<-TotalDit[which(TotalDit$Zone=="G"),]
PercentageofSingleLine<-length(TotalDit[which(TotalDit$NumberofLines==1),]$NumberofLines)/length(TotalDit$Order_id)
#####Different Area####
TMezzanine<-unique(sqldf("SELECT Order_id,Zone from Newform where Zone='A'or'B' group by Order_id,Zone")$Order_id)
TFloor<-unique(sqldf("SELECT Order_id,Zone from Newform where Zone='C'or'D' group by Order_id,Zone")$Order_id)
TSecurity<-unique(sqldf("SELECT Order_id,Zone from Newform where Zone='E' group by Order_id,Zone")$Order_id)
TPR<-unique(sqldf("SELECT Order_id,Zone from Newform where Zone='G' group by Order_id,Zone")$Order_id)
TouchDifferentArea<-data.frame(Zone=c("Mezzanine","Floor","Security","PalletRack"),Amount=c(length(TMezzanine)/56743,length(TFloor)/56743,length(TSecurity)/56743,length(TPR)/56743))
d1<-ggplot(data = TouchDifferentArea,aes(x=Zone,y=Amount))+geom_bar(position = "dodge",stat = "identity")+ggtitle("TouchDifferentArea")
#####Only Touch Every Zone#####
TouchCDEG<-unique(sqldf("SELECT Order_id,Zone from Newform where Zone='C'or'D'or'E'or'G' group by Order_id,Zone")$Order_id)
TouchABEG<-unique(sqldf("SELECT Order_id,Zone from Newform where Zone='A'or'B'or'E'or'G' group by Order_id,Zone")$Order_id)
TouchABCDG<-unique(sqldf("SELECT Order_id,Zone from Newform where Zone='A'or'B'or'C'or'D'or'G' group by Order_id,Zone")$Order_id)
TouchABCDE<-unique(sqldf("SELECT Order_id,Zone from Newform where Zone='A'or'B'or'C'or'D'or'G' group by Order_id,Zone")$Order_id)

####Shipping Discrepancy######
####Where are they####
Dis<-sqldf("SELECT Zone,Aisle,Bay,sum(Order_qty-Ship_qty) from Newform group by Zone,Aisle,Bay order by Aisle,Bay")
names(Dis)[4]<-"Discrepancy"
DinA<-Dis[which(Dis$Zone=="A"),];PLDA<-ggplot(data = DinA,aes(x= Aisle,y= Bay))+geom_point(aes(size=Discrepancy),color="Black")+ggtitle("A")
DinB<-Dis[which(Dis$Zone=="B"),];PLDB<-ggplot(data = DinB,aes(x= Aisle,y= Bay))+geom_point(aes(size=Discrepancy),color="Black")+ggtitle("B")
DinC<-Dis[which(Dis$Zone=="C"),];PLDC<-ggplot(data = DinC,aes(x= Aisle,y= Bay))+geom_point(aes(size=Discrepancy),color="Black")+ggtitle("C")
DinD<-Dis[which(Dis$Zone=="D"),];PLDD<-ggplot(data = DinD,aes(x= Aisle,y= Bay))+geom_point(aes(size=Discrepancy),color="Black")+ggtitle("D")
DinE<-Dis[which(Dis$Zone=="E"),];PLDE<-ggplot(data = DinE,aes(x= Aisle,y= Bay))+geom_point(aes(size=Discrepancy),color="Black")+ggtitle("E")
DinG<-Dis[which(Dis$Zone=="G"),];PLDG<-ggplot(data = DinG,aes(x= Aisle,y= Bay))+geom_point(aes(size=Discrepancy),color="Black")+ggtitle("G")
DisP<-grid.arrange(PLDA,PLDB,PLDC,PLDD,PLDE,PLDG)
#####Amount Skus######
Dis2<-sqldf("SELECT Sku_id,sum(Order_qty-Ship_qty) from Newform group by Sku_id")
###Plot is not necessary########
