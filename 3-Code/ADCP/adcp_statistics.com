echo " "
echo "Script requires a y reply for pressure output"
echo " "
echo "Press enter to continue."
read return
./adcp_request_datafile_rosediagram.com
echo "
statistic <- scan(\"statistics.lis\",skip=3,what=list(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
deploy <- statistic[[1]]
segment <- statistic[[2]]
depthb <- statistic[[3]]
depths <- statistic[[4]]
day    <- statistic[[5]]
month  <- statistic[[6]]
year   <- statistic[[7]]
hour   <- statistic[[8]]
minute <- statistic[[9]]
second <- statistic[[10]]
speed  <- statistic[[11]]
direction <- statistic[[12]]
intensity <- statistic[[13]]
ve     <- statistic[[14]]
vn     <- statistic[[15]]
lat <- statistic[[16]]
long <- statistic[[17]]
tidehgt <- statistic[[18]]
pressure <- statistic[[19]]
quan25 <- aggregate(speed,by=list(depthb),function(x) round(quantile(x,.25),2))
quan50 <- aggregate(speed,by=list(depthb),function(x) round(quantile(x,.50),2))
quan75 <- aggregate(speed,by=list(depthb),function(x) round(quantile(x,.75),2))
meansp <- aggregate(speed,by=list(depthb),function(x) round(mean(x),2))
minsp <- aggregate(speed,by=list(depthb),function(x) round(min(x),2))
maxsp <- aggregate(speed,by=list(depthb),function(x) round(max(x),2))
medsp <- aggregate(speed,by=list(depthb),function(x) round(median(x),2))
lensp <- aggregate(speed,by=list(depthb),function(x) round(length(x),0))
dump <- cbind(depth=lensp[,1],cases=lensp[,2],min=minsp[,2],max=maxsp[,2],mean=meansp[,2],
median=medsp[,2],q25=quan25[,2],q50=quan50[,2],q75=quan75[,2])
write.table(dump,file=\"statistics.txt\",quote = FALSE,sep='\t',row.names=F)
#tapply(speed,by=list(depthb),function(x) c(quantile(x,.25),quantile(x,.50),quantile(x,.75)))
dev.off()
q()
" > statistics.deck
R CMD BATCH statistics.deck
echo "Enter cruno for naming purpose?"
read cruno
echo "Enter station for naming purpose?"
read station
mv statistics.txt stats$cruno\_$station.txt
