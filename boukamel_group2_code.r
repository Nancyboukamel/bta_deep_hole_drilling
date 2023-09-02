#########  Project 2 #############
setwd("/Users/nancyboukamel/Desktop/casestudies/folders/")
options(scipen = "99999")
library("tuneR")
library('seewave')
library('fftw')
library('dplyr')

get_header <- function(file, relevant = c('DATASET', 'VERSION ', 'SERIES ', 'DATE', 'TIME', 'RATE', 
                                          'VERT_UNITS', 'HORZ_UNITS', 'COMMENT','NUM_SERIES', 'STORAGE_MODE', 'FILE_TYPE', 
                                          'SLOPE', 'X_OFFSET', 'Y_OFFSET', 'NUM_SAMPS')){
  header = readLines(file)
  elements = lapply(relevant, function(x){ which(grepl(x, header))[1]})
  elements = unlist(elements)
  header = header[elements]
  positions = regexpr(' ',header )
  result = as.list(substr(header,positions+1, nchar(header) ))
  names(result) = substr(header, 1,positions-1 )
  for(vars in  c('Y_OFFSET', 'X_OFFSET', 'SLOPE', 'NUM_SERIES', 'RATE', 'VERSION', 'NUM_SAMPS')){
    result[[vars]] <- eval(parse(text = paste0('c(', result[[vars]], ')')) )
  }
  
  for(vars in c('VERT_UNITS', 'SERIES')){
    result[[vars]]  <- gsub(' ', '',  result[[vars]]  )
    result[[vars]]  <- strsplit( result[[vars]] , ",")[[1]]
  }
  
  return(result)
}


get_data <- function(file, header){
  sens_dat = readBin(file, integer(), n = (header$NUM_SERIES * header$NUM_SAMPS), size = 2, endian = 'little')
  mat = t(matrix(sens_dat, nrow = header$NUM_SERIES))
  for(i in 1:header$NUM_SERIES){
    mat[,i] <- mat[,i] * header$SLOPE[i] + header$Y_OFFSET[i]
  }
  print(header$SERIES)
  colnames(mat) <- header$SERIES
  return(mat)
}


header <- get_header('V2_00001.HDR')
dataV2 <- get_data('V2_00001.DAT',header)
dataV2 <- as.data.frame(dataV2)
for(i in names(header))
  attr(dataV2,i) <- header[[i]]

dataV2 <- rename(dataV2,CH2_Force=CH2_Kraft)
dataV2 <- rename(dataV2,CH4_Acoustic=CH4_akustik)

summary(dataV2)


## version 10
header <- get_header('V10_0001.HDR')
dataV10 <- get_data('V10_0001.DAT',header)
dataV10 <- as.data.frame(dataV10)
summary(dataV10)
for(i in names(header))
  attr(dataV10,i) <- header[[i]]

dataV10 <- rename(dataV10,CH2_Force=CH2_Kraft)
dataV10 <- rename(dataV10,CH4_Acoustic=CH4_akustik)

summary(dataV10)


## version 6
header <- get_header('V6_00001.HDR')
dataV6 <- get_data('V6_00001.DAT',header)
dataV6 <- as.data.frame(dataV6)
summary(dataV6)
for(i in names(header))
  attr(dataV6,i) <- header[[i]]

dataV6 <- rename(dataV6,CH2_Force=CH2_Kraft)
dataV6 <- rename(dataV6,CH4_Acoustic=CH4_akustik)


## version 17
header <- get_header('V17_0001.HDR')
dataV17 <- get_data('V17_0001.DAT',header)
dataV17 <- as.data.frame(dataV17)
summary(dataV17)
for(i in names(header))
  attr(dataV17,i) <- header[[i]]

dataV17 <- rename(dataV17,CH2_Force=CH2_Kraft)
dataV17 <- rename(dataV17,CH4_Acoustic=CH4_akustik)


## version 20
header <- get_header('V20_0001.HDR')
dataV20 <- get_data('V20_0001.DAT',header)
dataV20 <- as.data.frame(dataV17)
summary(dataV20)
for(i in names(header))
  attr(dataV20,i) <- header[[i]]

dataV20 <- rename(dataV20,CH2_Force=CH2_Kraft)
dataV20 <- rename(dataV20,CH4_Acoustic=CH4_akustik)

## version 24
header <- get_header('V24_0001.HDR')
dataV24 <- get_data('V24_0001.DAT',header)
dataV24 <- as.data.frame(dataV24)
summary(dataV24)
for(i in names(header))
  attr(dataV24,i) <- header[[i]]

dataV24 <- rename(dataV24,CH2_Force=CH2_Kraft)
dataV24 <- rename(dataV24,CH4_Acoustic=CH4_akustik)

## version 25
header <- get_header('V25a_001.HDR')
dataV25 <- get_data('V25a_001.DAT',header)
dataV25 <- as.data.frame(dataV25)
summary(dataV25)
for(i in names(header))
  attr(dataV25,i) <- header[[i]]

dataV25 <- rename(dataV25,CH2_Force=CH2_Kraft)
dataV25 <- rename(dataV25,CH4_Acoustic=CH4_akustik)

#### get headers of D files
get_dataD <- function(file,nb){
  sens_dat = readBin(file, integer(), n = (7*nb), size = 2, endian = 'little')
  mat = t(matrix(sens_dat, nrow = 7))
  slopes =  c(0.02836000, 1.07200000, 0.00768000, 0.00080000, 0.01000000, 0.00056610, 0.12000000)
  for(i in 1:7){
    mat[,i] <- mat[,i] * slopes[i] + 0
  }
  colnames(mat) <- c('CH1_Moment', 'CH2_Kraft', 'CH3_Biegemo', 'CH4_SyncSig', 'CH5_a3_BOZA', 'CH6_akustik', 'CH7_a4_Bohrst')
  return(mat)
}

## D04
dataD4 <- get_dataD('D0400001.HDR',4688582)
dataD4 <- as.data.frame(dataD4)
for(i in names(header))
  attr(dataD4,i) <- header[[i]]

dataD4 <- rename(dataD4,CH2_Force=CH2_Kraft)
dataD4 <- rename(dataD4,CH6_Acoustic=CH6_akustik)
dataD4 <- rename(dataD4,CH3_Bending=CH3_Biegemo)
dataD4 <- rename(dataD4,CH7_a4_Drilling=CH7_a4_Bohrst)

## D08
dataD8 <- get_dataD('D0800001.HDR',5349176)
dataD8 <- as.data.frame(dataD8)
for(i in names(header))
  attr(dataD8,i) <- header[[i]]

dataD8 <- rename(dataD8,CH2_Force=CH2_Kraft)
dataD8 <- rename(dataD8,CH6_Acoustic=CH6_akustik)
dataD8 <- rename(dataD8,CH3_Bending=CH3_Biegemo)
dataD8 <- rename(dataD8,CH7_a4_Drilling=CH7_a4_Bohrst)

## D06
dataD6 <- get_dataD('D0600001.HDR',5349176)
dataD6 <- as.data.frame(dataD6)
for(i in names(header))
  attr(dataD6,i) <- header[[i]]

dataD6 <- rename(dataD6,CH2_Force=CH2_Kraft)
dataD6 <- rename(dataD6,CH6_Acoustic=CH6_akustik)
dataD6 <- rename(dataD6,CH3_Bending=CH3_Biegemo)
dataD6 <- rename(dataD6,CH7_a4_Drilling=CH7_a4_Bohrst)



################################
plot(Wave(as.numeric(dataV24$CH1_Moment),samp.rate=20000),xlab="")
title(main="",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=1.3)
abline(v=35,col="red")
###################

dataV24[, c("CH1_Moment")][0:240000]
plot(Wave(as.numeric(dataV24[, c("CH1_Moment")][0:240000]),samp.rate=20000),xlab="")

## part 1
par(mfrow=c(1,1))


dataV24Part2 <-  dataV24[, c("CH1_Moment")][300000:300600]# 15 to 15.03
xar <- 1*seq(15,15.03 , length.out = 601)
plot(xar, dataV24Part2,type='l',main="",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5,cex.axis=1.5,sub="(a)",cex.sub=1.5)


dataV24Part3 <- dataV24[, c("CH1_Moment")][800000:800600]  # 40sec to 40.03
xar <- 1*seq(40,40.03 , length.out = 601)
plot(xar, dataV24Part3,type='l',main="",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.axis=1.5,sub="(b)",,cex.sub=1.5)


dataV24Part4 <- dataV24[, c("CH1_Moment")][3000000:3000600]  # 150sec to 150.03
xar <- 1*seq(150,150.03 , length.out = 601)
plot(xar, dataV24Part4,type='l',main="",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.axis=1.5,sub="(c)",,cex.sub=1.5)




par(mfrow=c(2,2))
myacf1 <- acf(dataV24Part2 ,lag.max = 25,plot = FALSE)
myacf1 <- myacf1[1:25]
plot(myacf1,main="",cex.lab=1.5,cex.axis=1.5,sub="(a)",cex.sub=1.5,yaxt = "n")
axis(2, at = c(0,0.2, 0.4),cex.axis=1.5)
myacf2 <- acf(dataV24Part3 ,lag.max = 25,plot=FALSE)
myacf2 <- myacf2[1:25]
plot(myacf2,main="",cex.lab=1.5,cex.axis=1.5,sub="(b)",cex.sub=1.5,)
myacf3 <- acf(dataV24Part4 ,lag.max = 25,plot = FALSE)
myacf3 <- myacf3[1:25]
plot(myacf3,main="",cex.lab=1.5,cex.axis=1.5,sub="(c)",cex.sub=1.5,yaxt="n")
axis(2, at = c(0,0.5,1),cex.axis=1.5)

#####################################



par(mfrow=c(4,2))
###############
plot(Wave(as.numeric(dataV6$CH1_Moment),samp.rate=20000),xlab="")
title(main="Moment of V6_00001 vs. time(sec)",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=2)

plot(Wave(as.numeric(dataV10$CH1_Moment),samp.rate=20000),xlab="")
title(main="Moment of V10_0001 vs. time(sec)",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=2)

plot(Wave(as.numeric(dataV2$CH1_Moment),samp.rate=20000), xlab="")
title(main="Moment of V2_00001 vs. time(sec)",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=2)

plot(Wave(as.numeric(dataV17$CH1_Moment),samp.rate=20000), xlab="")
title(main="Moment of V17_0001 vs. time(sec)",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=2)

plot(Wave(as.numeric(dataV24$CH1_Moment),samp.rate=20000), xlab="")
title(main="Moment of V24_0001 vs. time(sec)",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=2)

plot(Wave(as.numeric(dataV25$CH1_Moment),samp.rate=20000), xlab="")
title(main="Moment of V25a_001 vs. time(sec)",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=2)

plot(Wave(as.numeric(dataV20$CH1_Moment),samp.rate=20000), xlab="")
title(main="Moment of V20_0001 vs. time(sec)",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=2)


######################################


N=20000

## V2
par(mfrow=c(2,2))
FV6 <- dfreq(Wave(as.numeric(dataV6$CH1_Moment),samp.rate=20000),f=20000,wl=N, wn='hanning', fftw = TRUE, ovlp=0, type='l',cex.lab=1.5, cex.main=1.5,cex.axis=1.3,xlab="Time (sec)",main="Spectrogram of moment sensor of V6_00001")
plot(Wave(as.numeric(dataV6$CH1_Moment),samp.rate=20000),xlab="")
title(main="Moment of V6_00001 versus time",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=1.3)
FV10 <- dfreq(Wave(as.numeric(dataV10$CH1_Moment),samp.rate=20000),f=20000,wl=N, wn='hanning', fftw = TRUE, ovlp=0, type='l',cex.lab=1.5, cex.main=1.5,cex.axis=1.3,xlab="Time (sec)",main="Spectrogram of moment sensor of V10_00001")
plot(Wave(as.numeric(dataV10$CH1_Moment),samp.rate=20000),xlab="")
title(main="Moment of V10_00001 versus time",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=1.3)


par(mfrow=c(2,2))
FV20 <- dfreq(Wave(as.numeric(dataV20$CH1_Moment),samp.rate=20000),f=20000,wl=N, wn='hanning', fftw = TRUE, ovlp=0, type='l',main="Spectrogram of Moment Sensor of V20_0001",cex.lab=1.5, cex.main=1.5,cex.axis=1.3,xlab="Time (sec)")
plot(Wave(as.numeric(dataV20$CH1_Moment),samp.rate=20000),xlab="")
title(main="Moment of V20_0001 versus time",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=1.3)
FV24 <- dfreq(Wave(as.numeric(dataV24$CH1_Moment),samp.rate=20000),f=20000,wl=N, wn='hanning', fftw = TRUE, ovlp=0, type='l',main="Spectrogram of Moment Sensor of V24_0001",cex.lab=1.5, cex.main=1.5,cex.axis=1.3,xlab="Time (sec)")
plot(Wave(as.numeric(dataV24$CH1_Moment),samp.rate=20000),xlab="")
title(main="Moment of V24_0001 versus time",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=1.3)

FV2 <- dfreq(Wave(as.numeric(dataV2$CH1_Moment),samp.rate=20000),f=20000,wl=N, wn='hanning', fftw = TRUE, ovlp=0, type='l',main="Spectrogram of Moment Sensor of V2_00001",cex.lab=1.5, cex.main=1.5,cex.axis=1.3,xlab="Time (sec)")
plot(Wave(as.numeric(dataV2$CH1_Moment),samp.rate=20000),xlab="")
title(main="Moment of V2_00001 versus time",ylab="Moment (Nm)",xlab="Time (sec)",cex.lab=1.5, cex.main=1.5,cex.axis=1.3)


FV2DF <- as.data.frame(FV2)
FV6DF <- as.data.frame(FV6)
FV10DF <- as.data.frame(FV10)
FV20DF <- as.data.frame(FV20)
FV24DF <- as.data.frame(FV24)







R.Version()
citation()
citation(package = "tuneR")
citation(package = "seewave")
packageVersion("tuneR")
packageVersion("seewave")
packageVersion("fftw")
citation(package = "fftw")
citation(package = "dplyr")
packageVersion("dplyr")
