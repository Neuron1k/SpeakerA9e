library(dplyr)
library(plyr)
library(tuneR)
sampleFiles <- list.files("app/samples/")
setwd("app/samples/")
wav_files <- lapply(sampleFiles, readWave)
mono_channels <- lapply(wav_files, function(x) (if(x@stereo == TRUE) mono(x,"left") else x))
mel_table <- lapply( mono_channels, melfcc )





