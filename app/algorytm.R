library(plyr)
library(dplyr)
library(tuneR)

wd <- getwd()
sampleFiles <- list.files("app/samples/")
setwd("app/samples/")

wav_files <- lapply(sampleFiles, readWave)
mono_channels <- lapply(wav_files, function(x) (if(x@stereo == TRUE) mono(x,"left") else x))
mel_table <- lapply(mono_channels, (function(x) 
  ( melfcc( x,x@samp.rate,wintime=0.016, lifterexp=0, minfreq=133.33,maxfreq=6855.6, sumpower=FALSE)) ) )

mel_table_clustered <- lapply(mel_table, (function(x)
    kmeans(x[1:dim(x)[1],],3, nstart = 10 )    ))

mel_avg <- mapply((function(x,y)
  aggregate(x,list(y$cluster),mean)),x=mel_table, y=mel_table_clustered  )

