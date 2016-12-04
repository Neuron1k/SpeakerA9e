library(plyr)
library(dplyr)
library(tuneR)

wd <- getwd()
sampleFiles <- list.files("app/samples/")
setwd("app/samples/")

v <- lapply(sampleFiles, function(x) (unlist(strsplit(x, "_"))[1:2] ))
agex <- as.data.frame(do.call(rbind, v))
age = data.frame(as.numeric(as.character(agex$V2)))

wav_files <- lapply(sampleFiles, readWave)
mono_channels <- lapply(wav_files, function(x) (if(x@stereo == TRUE) mono(x,"left") else x))
mel_table <- lapply(mono_channels, (function(x) 
  ( melfcc( x,x@samp.rate,wintime=0.016, lifterexp=0, minfreq=133.33,maxfreq=6855.6, sumpower=FALSE)) ) )

mel_table_clustered <- lapply(mel_table, (function(x)
    kmeans(x[1:dim(x)[1],],3, nstart = 10 )    ))

mel_avg <- mapply((function(x,y)
  aggregate(x,list(y$cluster),mean)),x=mel_table, y=mel_table_clustered  )

Fall <- as.data.frame(do.call(rbind, mel_avg))
F1 <- as.data.frame(split(Fall$V1, ceiling(seq_along(Fall$V1)/13)))
F2 <- as.data.frame(split(Fall$V2, ceiling(seq_along(Fall$V2)/13)))
F3 <- as.data.frame(split(Fall$V3, ceiling(seq_along(Fall$V3)/13)))

F1 <- data.frame(t(F1[-c(1),]))
F2 <- data.frame(t(F2[-c(1),]))
F3 <- data.frame(t(F3[-c(1),]))

ages <- scale(age)
F1s <- scale(F1)
F2s <- scale(F2)
F3s <- scale(F3)

