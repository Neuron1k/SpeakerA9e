library(plyr)
library(dplyr)
library(tuneR)
library(nnet)

parametry <- function(directory){
  wd <- getwd()
  sampleFiles <- list.files(directory)
  setwd(directory)

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

  F1s <- scale(F1)
  F2s <- scale(F2)
  F3s <- scale(F3)
  
  Z <- data.frame(F1,F2,F3)
  setwd(wd)
  return(Z)
}

cecha <- function(directory){
  wd <- getwd()
  sampleFiles <- list.files(directory)
  setwd(directory)
  
  v <- lapply(sampleFiles, function(x) (unlist(strsplit(x, "_"))[1:2] ))
  agex <- as.data.frame(do.call(rbind, v))
  age = data.frame(as.numeric(as.character(agex$V2)))
  ages <- scale(age)
  setwd(wd)
  return (list("scaled"=ages,"normal"=age))
}


wektor_uczenia = parametry("app/samples/")
wektor_test = parametry("app/samples_test/")

wiek_uczenia = cecha("app/samples/")
wiek_testowy = cecha("app/samples_test/")

siecNN <- nnet(wektor_uczenia,wiek_uczenia$scaled, size=8,linout=TRUE)


predykcja= predict(siecNN, wektor_test, type="raw")
wyniki = predykcja * attr(wiek_testowy$scaled, 'scaled:scale') + attr(wiek_testowy$scaled, 'scaled:center')
mean_root_square_error <- mean((wyniki - wiek_testowy$normal)^2)



dajWiek <- function(odebrany_wav) {
  wav_files <- list(odebrany_wav)
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
  
  F1s <- scale(F1)
  F2s <- scale(F2)
  F3s <- scale(F3)
  
  Z <- data.frame(F1,F2,F3)
  predykcja= predict(siecNN, Z, type="raw")
  wynik = predykcja * attr(wiek_testowy$scaled, 'scaled:scale') + attr(wiek_testowy$scaled, 'scaled:center')
  return(wynik)
}
#przyklad
W = readWave("app/samples_test/men_23_s1.wav")
WW = dajWiek(W)
#zwraca 37.57