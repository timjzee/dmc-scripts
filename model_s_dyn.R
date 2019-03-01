library(mgcv)
library(itsadug)
library(languageR)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/cgn/"
} else {
  f_path = "/vol/tensusers/timzee/cgn/"
}

s_dyn = read.csv(paste(f_path, "test_s_dynamic2.csv", sep = ""))
s_dyn$type_of_s = as.character(s_dyn$type_of_s)
s_dyn = s_dyn[s_dyn$type_of_s != "OTHER",]
s_dyn$type_of_s = as.factor(s_dyn$type_of_s)
s_dyn$type_of_s = relevel(s_dyn$type_of_s, ref="S")
s_dyn$freq_bin = as.numeric(s_dyn$freq_bin)

dyn = bam(dB_per_Hz ~ type_of_s + te(time, freq_bin, k = c(4,4), by = type_of_s) 
              + s(speaker, bs = 're'), data = s_dyn, method = 'ML')
summary(dyn)

par(mfrow=c(2,3))
plot(dyn)

par(mfrow=c(2,2))
fvisgam(dyn,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'S'),
        main = 'S', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=TRUE)
fvisgam(dyn,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PL'),
        main = 'PL', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=TRUE)
fvisgam(dyn,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'GEN'),
        main = 'GEN', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=TRUE)
fvisgam(dyn,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'DER'),
        main = 'DER', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=TRUE)

