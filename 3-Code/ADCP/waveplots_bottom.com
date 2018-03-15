echo " Script has been written to read CM data from the database and the reformated"     
echo " LOG9 file generated by WAVESMON. To simplyfy the data matching process you"      
echo " should rename the wave file to include the cruno and station corresponding"
echo " to the .rose file generated by adcp_request_datafile_rosediagram.com "
echo " in the ORACLE adcp selection process on quoddy3."
echo "

cruno  station  area                                                                

vr047   1   Welch Cove                                                       
vr048   1   Welch Cove                                                       
vr049   1   Welch Cove                                                       
vr052   1   Welch Cove                                                       
vr054   1   Welch Cove                                                       
vr055   1   Welch Cove                                                       
vr053   1   Blue Island"                                                      
echo " "
echo 'From table above enter cruno? \c'
read cruno 
echo 'From table above enter station? \c'
read station
case $cruno in
 vr047) studyarea="Welch Cove";;                                                                                           
 vr048) studyarea="Welch Cove";;                                                                                           
 vr049) studyarea="Welch Cove";;                                                                                           
 vr052) studyarea="Welch Cove";;                                                                                           
 vr054) studyarea="Welch Cove";;                                                                                           
 vr055) studyarea="Welch Cove";;                                                                                           
 vr053) studyarea="Blue Island";;                                                                                           
esac
more adcp$cruno*.rose
echo 'From listing above enter depth selection for plot (3rd Column = depth from bottom)? \c'
read depthsel
echo "
rosefile <- paste(\"adcp\",\"$cruno\",\"_\",\"$station\",\".rose\",sep=\"\")
cm <- matrix(scan(rosefile),ncol=18,byrow=T)
cmminutes <- cm[,9][cm[,9]>=50 & cm[,3] == $depthsel]
cmsegment <- cm[,2][cm[,9]>=50 & cm[,3] == $depthsel]
cmdepthbot <- cm[,3][cm[,9]>=50 & cm[,3] ==  $depthsel]
cmdepth <- cm[,4][cm[,9]>=50 & cm[,3] ==  $depthsel]
cmve <- cm[,14][cm[,9]>=50 & cm[,3] == $depthsel]
cmvn <- cm[,15][cm[,9]>=50 & cm[,3] == $depthsel]
cmday <- cm[,5][cm[,9]>=50 & cm[,3] == $depthsel]
cmmon <- cm[,6][cm[,9]>=50 & cm[,3] == $depthsel]
cmyr <- cm[,7][cm[,9]>=50 & cm[,3] == $depthsel]
cmhr <- cm[,8][cm[,9]>=50 & cm[,3] == $depthsel]
cmsec <- cm[,10][cm[,9]>=50 & cm[,3] == $depthsel]
cmspeed <- cm[,11][cm[,9]>=50 & cm[,3] == $depthsel]
cmdir <- cm[,12][cm[,9]>=50 & cm[,3] == $depthsel]
timeint <- c(rep(0.0,length(cmmon)))
timeint2 <- c(rep(0.0,length(cmmon)))
#initialize time interval period between sampling
vndel <- c(rep(0,length(cmmon)))
vedel <- c(rep(0,length(cmmon)))
dmy <- paste(cmday,cmmon,cmyr,sep=\"/\")
# concatenate my DMY together with a / separator to set into Splus format
HMS <- paste(cmhr,cmminutes,cmsec,sep=\":\")
# concatenate my HMS together with a : separator to set into Splus format
dmyHMS <- paste(dmy,HMS,sep=\" \")
mydate <- strptime(dmyHMS,\"%d/%m/%Y %H:%M:%S\")
tittlen <- paste(\"$studyarea\",dmy[1],\"to\",dmy[length(dmy)])
tittlen2 <- paste($depthsel,\"meters from bottom\")
# produce the vector mydate using the datetime function chron
Totaltime <- mydate[length(cmmon)] - mydate[1]
# 
ve <- sin(cmdir*2.0*pi/360.)*cmspeed
vn <- cos(cmdir*2.0*pi/360.)*cmspeed
# calculating vectors ve and vn from input matrix
timecor <- 0
timecor[1] <- 0
for(x in 2:length(cmday)){
timecor[x] <- difftime(mydate[x],mydate[x-1],units=\"mins\")}
timecor <- timecor*60
#convert to decimal seconds
firstx <- 2
lastx <- length(cmsec)
vndel[1] <- vn[1]*timecor[2]/100000
vedel[1] <- ve[1]*timecor[2]/100000
for(i in firstx:lastx){
 vndel[i] <- ((vn[i]*timecor[i]/100000)+vndel[i-1])
 vedel[i] <- ((ve[i]*timecor[i]/100000)+vedel[i-1])
 timeint[i] <- difftime(mydate[i],mydate[i-1],units=\"mins\")+timeint[i-1]
  }
timeint <- timeint/60/24
jpegfile <- paste(\"adcpwaved\",\"$cruno\",\"_\",\"$station\",\".jpg\",sep=\"\")
jpeg(jpegfile, height = 700, width = 700, quality = 100,pointsize=17)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE),c(4,4),c(3.5,3.5), TRUE)
par(mar=c(3,4,2,2))
  #screen(1)
  maxdis <- max(vedel,vndel)
  mindis <- min(vedel,vndel)
  absdel <- max(abs(mindis),abs(maxdis))
  #computing x and y axis limits
  plot(vedel,vndel,type=\"l\",
  xlab=\"\",ylab=\"\",
  xlim=c(-absdel,absdel),ylim=c(-absdel,absdel))
  mtext(\"Vn Kilometers\",side=2,line=2,cex=.9)
  mtext(\"Ve Kilometers\",side=1,line=2,cex=.9)
  segments(-absdel,0,absdel,0)
  segments(0,-absdel,0,absdel)

  #screen(2)

plot(cmdir,cmspeed,type=\"p\",
xlab=\" \",ylab=\"\")
mtext(\"Direction\",side=1,line=2,cex=.9)
mtext(\"Speed cm/sec\",side=2,line=2,cex=.9)
wavefile <- paste(\"adcp\",\"$cruno\",\"_\",\"$station\",\".wave\",sep=\"\")
x <- scan(wavefile,
what=list(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),fill=TRUE,flush=TRUE)
burst <- x[[1]]
yr <- x[[2]]
mon <- x[[3]]
day <- x[[4]]
hr <- x[[5]]
minutes <- x[[6]]
hs <- x[[9]]
tp <- x[[10]]
dp <- x[[11]]
pres <- x[[12]]
hssea <- x[[13]]
tpsea <- x[[14]]
dpsea <- x[[15]]
hsswell <- x[[16]]
tpswell <- x[[17]]
dpswell <- x[[18]]
hsmax <- x[[19]]
tpmax <- x[[20]]
hs10 <- x[[21]]
tp10 <- x[[22]]
hs33 <- x[[23]]
tp33 <- x[[24]]
hsmean <- x[[25]]
tpmean <- x[[26]]
dpmean <- x[[27]]
spd1 <- x[[29]]
dir1 <- x[[30]]
spd2 <- x[[31]]
dir2 <- x[[32]]
spd3 <- x[[33]]
dir3 <- x[[34]]
spd4 <- x[[35]]
dir4 <- x[[36]]
spd5 <- x[[37]]
dir5 <- x[[38]]
spd6 <- x[[39]]
dir6 <- x[[40]]
spd7 <- x[[41]]
dir7 <- x[[42]]
spd8 <- x[[43]]
dir8 <- x[[44]]
spd9 <- x[[45]]
dir9 <- x[[46]]
spd10 <- x[[47]]
dir10 <- x[[48]]
maxy=max(hs,hssea,hsswell,hs10,hs33)
miny=0
plot(burst,hsmean,type=\"l\",col=\"cyan\",ylim=c(miny,maxy),xlim=c(min(burst),max(burst)),cex=.2,xlab=\"\",ylab=\"\")
par(new=T)
plot(burst[hsmax > 0],hsmax[hsmax > 0],type=\"p\",col=\"red\",ylim=c(miny,maxy),xlim=c(min(burst),max(burst)),cex=.9,xlab=\"\",ylab=\"\")
mtext(\"Wave Height (m)\",side=2,line=2,cex=.9)
mtext(\"Hourly Burst\",side=1,line=2,cex=.9)
par(new=T)
plot(burst,hs,type=\"l\",ylim=c(miny,maxy),xlim=c(min(burst),max(burst)),cex=.2,
xlab=\" \",ylab=\"\")
par(new=T)
#plot(burst[hssea > 0],hssea[hssea > 0],type=\"l\",col=\"cyan\",ylim=c(miny,maxy),xlim=c(min(burst),max(burst)),cex=.2,xlab=\"\",ylab=\"\")
#par(new=T)
plot(burst,hsswell,type=\"l\",col=\"lightgreen\",ylim=c(miny,maxy),xlim=c(min(burst),max(burst)),cex=.2,xlab=\"\",ylab=\"\")
par(new=T)
legend(\"topleft\",legend=c(\"hs\",\"hsswell\",\"hsmax\",\"hsmean\"),col=c(\"black\",\"lightgreen\",\"red\",\"cyan\"),pt.cex=1,
cex=.8,bty='n',lty=c(1,1,0,1),pch=c(-1,-1,1,-1))
maxyy=max(tp,tpsea,tpswell,tp10,tp33)
minyy=0
maxyy=20
plot(burst,tpmean,type=\"l\",col=\"cyan\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,
xlab=\"\",ylab=\"\")
par(new=T)
plot(burst,tpswell,type=\"l\",col=\"lightgreen\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,xlab=\"\",ylab=\"\")
par(new=T)
plot(burst[tpmax > 0],tpmax[tpmax > 0],type=\"p\",col=\"red\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,xlab=\"\",ylab=\"\")
legend(\"topleft\",legend=c(\"tp\",\"tpswell\",\"tpmax\",\"tpmean\"),col=c(\"black\",\"lightgreen\",\"red\",\"cyan\"),pt.cex=1,
cex=.8,,bty=\"n\",lty=c(1,1,0,1),pch=c(-1,-1,1,-1))
mtext(\"Wave period (sec)\",side=2,line=2,cex=.9)
mtext(\"Hourly Burst\",side=1,line=2,cex=.9)
par(new=T)
plot(burst,tp,type=\"l\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,
xlab=\"\",ylab=\"\")
par(new=T)
#plot(burst,tpsea,type=\"l\",col=\"red\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,xlab=\"\",ylab=\"\")
#par(new=T)
#plot(burst,tp10,type=\"p\",col=\"cyan\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,xlab=\"\",ylab=\"\")
#par(new=T)

#Hs Significant Wave Height (mean of highest 1/3 of all waves)
#Tp mean of the periods of Hs
#Dp Peak Wave Direction
#H10 mean od 10% of the largest waves
#Tmean mean Period
#Dmean mean direction
title(main=tittlen,outer=T,line=-2.5,cex.main=1.3)
title(main=tittlen2,outer=T,line=-3.5,cex.main=1.3)
q()
" > waveplot.deck
R CMD BATCH waveplot.deck
gimp adcpwaved$cruno*.jpg    
