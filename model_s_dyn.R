library(mgcv)
library(itsadug)
library(languageR)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/"
} else {
  f_path = "/vol/tensusers/timzee/"
}

s_dyn = read.csv(paste(f_path, "cgn/comp-c_s_dynamic.csv", sep = ""))
s_dyn$type_of_s = as.character(s_dyn$type_of_s)
s_dyn = s_dyn[!(s_dyn$type_of_s %in% c("OTHER", "GEN-TIME")),]
s_dyn$type_of_s = as.factor(s_dyn$type_of_s)
s_dyn$type_of_s = relevel(s_dyn$type_of_s, ref="S")
s_dyn$freq_bin = as.numeric(s_dyn$freq_bin)

dyn = bam(dB_per_Hz ~ type_of_s + te(time, freq_bin, k = c(4,4), by = type_of_s) 
              + s(speaker, bs = 're'), data = s_dyn, method = 'ML')
summary(dyn)

par(mfrow=c(2,3))
plot(dyn)

par(mfrow=c(1,4))
fvisgam(dyn,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'S'), 
        zlim = c(53, 81),
        main = 'S', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PL'), 
        zlim = c(53, 81),
        main = 'PL', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'GEN-POSS'), 
        zlim = c(53, 81),
        main = 'GEN-POSS', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PART'),
        zlim = c(53, 81),
        main = 'PART', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)

par(mfrow=c(1,1))
plot_diff2(dyn, view = c("time", "freq_bin"), comp = list(type_of_s=c("PART", "PL")), show.diff = T)

