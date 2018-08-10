rm(list=ls())
library(raster)
library(sp)
library(rgeos)

tmin=getData('worldclim',var='tmin',res=2.5)
tmax=getData('worldclim',var='tmax',res=2.5)
#tmean=getData('worldclim',var='tmean',res=10)
#prec=getData('worldclim',var='prec',res=10)
alt=getData('worldclim',var='alt',res=2.5)
bioclim=getData('worldclim',var='bio',res=2.5)
annual_mean_temp=bioclim[[1]]
annual_precip=bioclim[[12]]
US=getData('GADM',country='USA',level=0)
states=getData('GADM',country='USA',level=1)
Canada=getData('GADM',country='CAN',level=1)
Mexico=getData('GADM',country='MEX',level=1)
counties=getData('GADM',country='USA',level=2)

#restrict US map to outside of Hawaii
states=subset(states,NAME_1!="Hawaii")

#merge US and CAN
#North_America=bind(states,Canada)
#rm(states)
#rm(Canada)
#gc()

myextent=extent(c(-170,-35,14,90))
#clip rasters to USA
July_tmin=crop(tmin[[7]],myextent)
July_tmax=crop(tmax[[7]],myextent)
January_tmin=crop(tmin[[1]],myextent)
January_tmax=crop(tmax[[1]],myextent)
annual_mean_temp=crop(annual_mean_temp,myextent)
annual_precip=crop(annual_precip,myextent)
alt=crop(alt,myextent)

#masking rasters for threshold
annual_mean_temp_binary=annual_mean_temp
annual_mean_temp_binary[which(!is.na(annual_mean_temp[]))]=0
annual_mean_temp_binary[which(annual_mean_temp[]>120)]=1

July_tmin_binary=July_tmin
July_tmin_binary[which(!is.na(July_tmin[]))]=0
July_tmin_binary[which(July_tmin[]>20)]=1

July_tmax_binary=July_tmax
July_tmax_binary[which(!is.na(July_tmax[]))]=0
July_tmax_binary[which(July_tmax[]>120)]=1

#January_tmin_binary=January_tmin
#January_tmin_binary[which(!is.na(January_tmin[]))]=0
#January_tmin_binary[which(January_tmin[]>20)]=1

#January_tmax_binary=January_tmax
#January_tmax_binary[which(!is.na(January_tmax[]))]=0
#January_tmax_binary[which(January_tmax[]>120)]=1

annual_precip_binary=annual_precip
annual_precip_binary[which(!is.na(annual_precip[]))]=0
annual_precip_binary[which(annual_precip[]>1000)]=1

alt_binary=alt
alt_binary[which(!is.na(alt[]))]=0
alt_binary[which(alt[]<300)]=1

#detections=NULL
#detections=rbind(detections,c(-74.913325,40.5690331)) #Hunterdon County, NJ
#detections=rbind(detections,c(-78.575874,38.029169)) #Albemarle County, VA
#detections=rbind(detections,c(-78.211913,38.907769)) #Warren County, VA
#detections=rbind(detections,c(-78.826744,39.018390)) #Hardy County, WV
#detections=rbind(detections,c(-94.241500,36.351246)) #Benton County, AR
#detections=rbind(detections,c(-74.305179,40.662940)) #Union County, NJ
#detections=rbind(detections,c(-82.179584, 35.282329)) #Polk County, NC
#detections=rbind(detections,c(-74.050308, 40.959123)) #Bergen County, NJ
#detections=rbind(detections, c(-73.811310,41.121811)) #Westchester County, NY
#detections=rbind(detections, c(-81.903876, 38.529845)) #Putnam County, WV
#detections=rbind(detections,c(-82.078684,38.192263)) #Lincoln County, WV
#detections=rbind(detections, c(-80.877643, 39.472793)) #Tyler County, WV
#detections=rbind(detections,c( -80.051839,39.333591)) #Taylor County, WV
#detections=rbind(detections, c(-81.062226,39.176876)) #Ritchie County, WV
#detections=rbind(detections, c(-74.375705,40.452399)) #Middlesex County, NJ
#detections=rbind(detections, c(-74.685972,40.275700)) #Mercer County, NJ

#detections based on counties
detections=which(counties$NAME_1=="New Jersey"&counties$NAME_2=="Hunterdon")
detections=c(detections,which(counties$NAME_1=="Virginia"&counties$NAME_2=="Albemarle"))
detections=c(detections,which(counties$NAME_1=="Virginia"&counties$NAME_2=="Warren"))
detections=c(detections,which(counties$NAME_1=="West Virginia"&counties$NAME_2=="Hardy"))
detections=c(detections,which(counties$NAME_1=="Arkansas"&counties$NAME_2=="Benton"))
detections=c(detections,which(counties$NAME_1=="New Jersey"&counties$NAME_2=="Union"))
detections=c(detections,which(counties$NAME_1=="North Carolina"&counties$NAME_2=="Polk"))
detections=c(detections,which(counties$NAME_1=="New Jersey"&counties$NAME_2=="Bergen"))
detections=c(detections,which(counties$NAME_1=="New York"&counties$NAME_2=="Westchester"))
detections=c(detections,which(counties$NAME_1=="West Virginia"&counties$NAME_2=="Putnam"))
detections=c(detections,which(counties$NAME_1=="West Virginia"&counties$NAME_2=="Lincoln"))
detections=c(detections,which(counties$NAME_1=="West Virginia"&counties$NAME_2=="Tyler"))
detections=c(detections,which(counties$NAME_1=="West Virginia"&counties$NAME_2=="Taylor"))
detections=c(detections,which(counties$NAME_1=="West Virginia"&counties$NAME_2=="Ritchie"))
detections=c(detections,which(counties$NAME_1=="New Jersey"&counties$NAME_2=="Middlesex"))
detections=c(detections,which(counties$NAME_1=="New Jersey"&counties$NAME_2=="Mercer"))
detections=c(detections,which(counties$NAME_1=="Pennsylvania"&counties$NAME_2=="Centre"))
detections=c(detections,which(counties$NAME_1=="New York"&counties$NAME_2=="Richmond"))
detections=c(detections,which(counties$NAME_1=="Maryland"&counties$NAME_2=="Washington"))

type_of_detection=c("off-host/human/sheep","cow","horse","cow","dog","off-host/horse/dog","opossum","off-host/human","NA","dog","dog","white-tailed deer","NA","dog","goats","off-host","white-tailed deer","NA","deer")


composite=annual_mean_temp_binary+July_tmin_binary+July_tmax_binary+annual_precip_binary+alt_binary
#composite2=annual_mean_temp_binary+January_tmin_binary+January_tmax_binary+annual_precip_binary+alt_binary
plot(composite,col=c("lemonchiffon","lightgoldenrod","yellow","greenyellow","green","forestgreen"))
#plot(composite2,col=c("lemonchiffon","lightgoldenrod","yellow","greenyellow","green","forestgreen"))
plot(states,add=TRUE)
plot(Canada,add=TRUE)
plot(Mexico,add=TRUE)
plot(counties[detections,],add=TRUE,border="red",lwd=3)
#points(detections,pch=20,col="red")

#proportion of area of the raster with different values - this includes Greenland
number_cells=length(which(!is.na(as.matrix(composite))))
prop_five=length(which(as.matrix(composite)==5))/number_cells
prop_four=length(which(as.matrix(composite)==4))/number_cells
prop_three=length(which(as.matrix(composite)==3))/number_cells
prop_two=length(which(as.matrix(composite)==2))/number_cells
prop_one=length(which(as.matrix(composite)==1))/number_cells
prop_zero=length(which(as.matrix(composite)==0))/number_cells

#values at detection counties
composite_values_at_detections=extract(composite,counties[detections,],fun=mean)
annual_mean_temp_at_detections=extract(annual_mean_temp_binary,counties[detections,],fun=mean)
July_tmin_temp_at_detections=extract(July_tmin_binary,counties[detections,],fun=mean)
July_tmax_temp_at_detections=extract(July_tmax_binary,counties[detections,],fun=mean)
#January_tmin_temp_at_detections=extract(January_tmin_binary,counties[detections,],fun=mean)
#January_tmax_temp_at_detections=extract(January_tmax_binary,counties[detections,],fun=mean)
annual_precip_binary_at_detections=extract(annual_precip_binary,counties[detections,],fun=mean)
alt_binary_at_detections=extract(alt_binary,counties[detections,],fun=mean)


values_for_detections=cbind(counties$NAME_1[detections],counties$NAME_2[detections],composite_values_at_detections,annual_mean_temp_at_detections,July_tmin_temp_at_detections,July_tmax_temp_at_detections,annual_precip_binary_at_detections,alt_binary_at_detections,type_of_detection)
values_for_detections=as.data.frame(values_for_detections)
names(values_for_detections)[1]="State"
names(values_for_detections)[2]="County"
names(values_for_detections)[3]="Composite score"
names(values_for_detections)[4]="Annual mean temp"
names(values_for_detections)[5]="July min temp"
names(values_for_detections)[6]="July max temp"
names(values_for_detections)[7]="Annual total precip"
names(values_for_detections)[8]="Altitude"
names(values_for_detections)[9]="Detection type"

o=order(values_for_detections$`Composite score`,decreasing=TRUE)
values_for_detections=values_for_detections[o,]

library(knitr)
library(arsenal)
library(magrittr)
tmpdir=getwd()
values_for_detections$`Composite score`=as.numeric(as.character(values_for_detections$`Composite score`))
values_for_detections$`Annual mean temp`=as.numeric(as.character(values_for_detections$`Annual mean temp`))
values_for_detections$`July min temp`=as.numeric(as.character(values_for_detections$`July min temp`))
values_for_detections$`July max temp`=as.numeric(as.character(values_for_detections$`July max temp`))
values_for_detections$`Annual total precip`=as.numeric(as.character(values_for_detections$`Annual total precip`))
values_for_detections$Altitude=as.numeric(as.character(values_for_detections$Altitude))
values_for_detections %>% knitr::kable(digits=3,row.names=FALSE) %>% write2word(paste0(tmpdir,"/detections_table.doc"),quiet=TRUE)


threshold=mean(composite_values_at_detections)

#creating table for states
states=states[-2,]

composite_values_per_state=extract(composite,states,fun=mean,na.rm=TRUE)
annual_mean_temp_per_state=extract(annual_mean_temp_binary,states,fun=mean,na.rm=TRUE)
July_tmin_temp_per_state=extract(July_tmin_binary,states,fun=mean,na.rm=TRUE)
July_tmax_temp_per_state=extract(July_tmax_binary,states,fun=mean,na.rm=TRUE)
#January_tmin_temp_per_state=extract(January_tmin_binary,states,fun=mean,na.rm=TRUE)
#January_tmax_temp_per_state=extract(January_tmax_binary,states,fun=mean,na.rm=TRUE)
annual_precip_binary_per_state=extract(annual_precip_binary,states,fun=mean,na.rm=TRUE)
alt_binary_per_state=extract(alt_binary,states,fun=mean,na.rm=TRUE)

values_for_states=cbind(states$NAME_1,composite_values_per_state,annual_mean_temp_per_state,July_tmin_temp_per_state,July_tmax_temp_per_state,annual_precip_binary_per_state,alt_binary_per_state)
values_for_states=as.data.frame(values_for_states)
names(values_for_states)[1]="State"
names(values_for_states)[2]="Composite score"
names(values_for_states)[3]="Annual mean temp"
names(values_for_states)[4]="July min temp"
names(values_for_states)[5]="July max temp"
names(values_for_states)[6]="Annual total precip"
names(values_for_states)[7]="Altitude"
o=order(values_for_states$`Composite score`,decreasing=TRUE)
values_for_states=values_for_states[o,]

library(knitr)
library(arsenal)
library(magrittr)
tmpdir=getwd()
values_for_states$`Composite score`=as.numeric(as.character(values_for_states$`Composite score`))
values_for_states$`Annual mean temp`=as.numeric(as.character(values_for_states$`Annual mean temp`))
values_for_states$`July min temp`=as.numeric(as.character(values_for_states$`July min temp`))
values_for_states$`July max temp`=as.numeric(as.character(values_for_states$`July max temp`))
values_for_states$`Annual total precip`=as.numeric(as.character(values_for_states$`Annual total precip`))
values_for_states$Altitude=as.numeric(as.character(values_for_states$Altitude))
values_for_states %>% knitr::kable(digits=3,row.names=FALSE) %>% write2word(paste0(tmpdir,"/state_suitability.doc"),quiet=TRUE)


prop_level_five_states=length(which(composite_values_per_state>4))
prop_level_four_states=length(which(composite_values_per_state>3&composite_values_per_state<=4))
prop_level_three_states=length(which(composite_values_per_state>2&composite_values_per_state<=3))
prop_level_two_states=length(which(composite_values_per_state>1&composite_values_per_state<=2))

composite_values_per_county=extract(composite,counties,fun=mean)
counties$HL_suitability=composite_values_per_county
counties$GEO=paste(counties$NAME_1,counties$NAME_2,sep="\\")

#reading in cattle data
cattle=read.csv("cattle_by_county.csv",header=TRUE)
cattle$GEO=as.character(cattle$GEO)
state_totals=which(unlist(lapply(1:dim(cattle)[1],function(x) length(unlist(strsplit(cattle$GEO,'[\\]')[x]))))==1)
cattle=cattle[-state_totals,]
cattle$State=unlist(strsplit(cattle$GEO,'[\\]'))[seq(1,dim(cattle)[1]*2,2)]
cattle$County=unlist(strsplit(cattle$GEO,'[\\]'))[seq(2,dim(cattle)[1]*2,2)]

#mismatch counties
mismatches=which(!cattle$GEO %in% counties$GEO)

#fixing mismatches
cattle$County[25]="De Kalb"
cattle$County[58]="Saint Clair"
cattle$County[68]="Aleutians East"
cattle$County[70]="Fairbanks North Star"
cattle$County[149]="Saint Francis"
cattle$County[308]="Desoto"
cattle$County[350]="Saint Johns"
cattle$County[351]="Saint Lucie"
cattle$County[591]="Dupage"
cattle$County[651]="Saint Clair"
cattle$County[688]="De Kalb"
cattle$County[742]="Saint Joseph"
cattle$County[1131]="Saint Bernard"
cattle$County[1132]="Saint Charles"
cattle$County[1133]="Saint Helena"
cattle$County[1134]="Saint James"
cattle$County[1135]="Saint John the Baptist"
cattle$County[1136]="Saint Landry"
cattle$County[1137]="Saint Martin"
cattle$County[1138]="Saint Mary"
cattle$County[1139]="Saint Tammany"
cattle$County[1185]="Saint Mary's"
cattle$County[1278]="Saint Clair"
cattle$County[1279]="Saint Joseph"
cattle$County[1356]="Saint Louis"
cattle$County[1391]="Desoto"
cattle$County[1488]="De Kalb"
cattle$County[1548]="Saint Charles"
cattle$County[1549]="Saint Clair"
cattle$County[1550]="Sainte Genevieve"
cattle$County[1551]="Saint Francois"
cattle$County[1552]="Saint Louis"
cattle$County[1774]="Debaca"
cattle$County[1845]="Saint Lawrence"
cattle$County[1985]="Lamoure"
cattle$County[2258]="Mc Kean"
cattle$County[2557]="Dewitt"
cattle$County[2888]="Chesapeake"
cattle$County[2890]="Virginia Beach"
cattle$County[3040]="Saint Croix"


cattle$GEO2=paste(cattle$State,cattle$County,sep="\\")
mismatches2=which(!cattle$GEO2 %in% counties$GEO)
cattle$GEO=cattle$GEO2


#merge county data with cattle data
new_cattle=merge(cattle,counties,by="GEO",all=TRUE,sort=FALSE)
new_cattle$DATA=as.character(new_cattle$DATA)
new_cattle$DATA[which(new_cattle$DATA=="-")]="0"
new_cattle$DATA[which(new_cattle$DATA=="(D)")]="NA"

new_cattle$DATA=as.numeric(new_cattle$DATA)
HL_suitable_counties=which(new_cattle$HL_suitability>threshold)
sum(new_cattle$DATA[HL_suitable_counties],na.rm=TRUE)
#hist(new_cattle$DATA[which(new_cattle$HL_suitability>threshold)])
HL_suitable_counties_w_lots_cattle=which(new_cattle$DATA>10000&new_cattle$HL_suitability>threshold)
sum(new_cattle$DATA[HL_suitable_counties_w_lots_cattle],na.rm=TRUE)
length(HL_suitable_counties_w_lots_cattle)

counties_index_for_HL_suitable_counties_w_lots_cattle=unlist(lapply(HL_suitable_counties_w_lots_cattle,function(x) which(counties$GEO==new_cattle$GEO[x])))
states_wo_Alaska=states[-which(states$NAME_1=="Alaska"),]
plot(states_wo_Alaska)
#plot(US,xlim=c(-170,-35),ylim=c(14,90))
plot(counties[counties_index_for_HL_suitable_counties_w_lots_cattle,],col="red",add=TRUE)
#plot(Canada,add=TRUE)
#plot(Mexico,add=TRUE)
length(which(new_cattle$State[HL_suitable_counties_w_lots_cattle]=="California"))
new_cattle$DATA[which(new_cattle$State=="Washington"&new_cattle$DATA>10000&new_cattle$HL_suitability>threshold)]
new_cattle$County[which(new_cattle$State=="Washington"&new_cattle$DATA>10000&new_cattle$HL_suitability>threshold)]
new_cattle$HL_suitability[which(new_cattle$State=="Washington"&new_cattle$DATA>10000&new_cattle$HL_suitability>threshold)]
new_cattle$DATA[which(new_cattle$State=="Oregon"&new_cattle$DATA>10000&new_cattle$HL_suitability>threshold)]
new_cattle$County[which(new_cattle$State=="Oregon"&new_cattle$DATA>10000&new_cattle$HL_suitability>threshold)]
new_cattle$HL_suitability[which(new_cattle$State=="Oregon"&new_cattle$DATA>10000&new_cattle$HL_suitability>threshold)]

#reading in sheep data
sheep=read.csv("sheep_lambs_by_county.csv",header=TRUE)
sheep$GEO=as.character(sheep$GEO)
state_totals=which(unlist(lapply(1:dim(sheep)[1],function(x) length(unlist(strsplit(sheep$GEO,'[\\]')[x]))))==1)
sheep=sheep[-state_totals,]
sheep$State=unlist(strsplit(sheep$GEO,'[\\]'))[seq(1,dim(sheep)[1]*2,2)]
sheep$County=unlist(strsplit(sheep$GEO,'[\\]'))[seq(2,dim(sheep)[1]*2,2)]
#mismatch counties
mismatches=which(!sheep$GEO %in% counties$GEO)

sheep$County[25]="De Kalb"
sheep$County[58]="Saint Clair"
sheep$County[68]="Aleutians East"
sheep$County[70]="Fairbanks North Star"
sheep$County[149]="Saint Francis"
sheep$County[308]="Desoto"
sheep$County[350]="Saint Johns"
sheep$County[351]="Saint Lucie"
sheep$County[591]="Dupage"
sheep$County[651]="Saint Clair"
sheep$County[688]="De Kalb"
sheep$County[742]="Saint Joseph"
sheep$County[1131]="Saint Bernard"
sheep$County[1132]="Saint Charles"
sheep$County[1133]="Saint Helena"
sheep$County[1134]="Saint James"
sheep$County[1135]="Saint John the Baptist"
sheep$County[1136]="Saint Landry"
sheep$County[1137]="Saint Martin"
sheep$County[1138]="Saint Mary"
sheep$County[1139]="Saint Tammany"
sheep$County[1185]="Saint Mary's"
sheep$County[1278]="Saint Clair"
sheep$County[1279]="Saint Joseph"
sheep$County[1356]="Saint Louis"
sheep$County[1391]="Desoto"
sheep$County[1488]="De Kalb"
sheep$County[1548]="Saint Charles"
sheep$County[1549]="Saint Clair"
sheep$County[1550]="Sainte Genevieve"
sheep$County[1551]="Saint Francois"
sheep$County[1552]="Saint Louis"
sheep$County[1774]="Debaca"
sheep$County[1845]="Saint Lawrence"
sheep$County[1985]="Lamoure"
sheep$County[2258]="Mc Kean"
sheep$County[2557]="Dewitt"
sheep$County[2888]="Chesapeake"
sheep$County[2890]="Virginia Beach"
sheep$County[3040]="Saint Croix"


sheep$GEO2=paste(sheep$State,sheep$County,sep="\\")
mismatches2=which(!sheep$GEO2 %in% counties$GEO)
sheep$GEO=sheep$GEO2


#merge county data with sheep data
new_sheep=merge(sheep,counties,by="GEO",all=TRUE,sort=FALSE)
new_sheep$DATA=as.character(new_sheep$DATA)
new_sheep$DATA[which(new_sheep$DATA=="-")]="0"
new_sheep$DATA[which(new_sheep$DATA=="(D)")]="NA"

new_sheep$DATA=as.numeric(new_sheep$DATA)
HL_suitable_counties=which(new_sheep$HL_suitability>threshold)
sum(new_sheep$DATA[HL_suitable_counties],na.rm=TRUE)
#hist(new_sheep$DATA[which(new_sheep$HL_suitability>threshold)])
HL_suitable_counties_w_lots_sheep=which(new_sheep$DATA>1000&new_sheep$HL_suitability>threshold)
sum(new_sheep$DATA[HL_suitable_counties_w_lots_sheep],na.rm=TRUE)
length(HL_suitable_counties_w_lots_sheep)
length(unique(new_sheep$State[HL_suitable_counties_w_lots_sheep]))
counties_index_for_HL_suitable_counties_w_lots_sheep=unlist(lapply(HL_suitable_counties_w_lots_sheep,function(x) which(counties$GEO==new_sheep$GEO[x])))
plot(states_wo_Alaska)
#plot(US,xlim=c(-170,-35),ylim=c(14,90))
plot(counties[counties_index_for_HL_suitable_counties_w_lots_sheep,],col="red",add=TRUE)
#plot(Canada,add=TRUE)
#plot(Mexico,add=TRUE)
length(which(new_sheep$State[HL_suitable_counties_w_lots_sheep]=="California"))
new_sheep$DATA[which(new_sheep$State=="Washington"&new_sheep$DATA>1000&new_sheep$HL_suitability>threshold)]
new_sheep$County[which(new_sheep$State=="Washington"&new_sheep$DATA>1000&new_sheep$HL_suitability>threshold)]
new_sheep$HL_suitability[which(new_sheep$State=="Washington"&new_sheep$DATA>1000&new_sheep$HL_suitability>threshold)]
new_sheep$DATA[which(new_sheep$State=="Oregon"&new_sheep$DATA>1000&new_sheep$HL_suitability>threshold)]
new_sheep$County[which(new_sheep$State=="Oregon"&new_sheep$DATA>1000&new_sheep$HL_suitability>threshold)]
new_sheep$HL_suitability[which(new_sheep$State=="Oregon"&new_sheep$DATA>1000&new_sheep$HL_suitability>threshold)]
