library(lme4)
library(effects)
library(car)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/af_classification/"
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
  ifadv_path = "/Volumes/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/Volumes/tensusers/timzee/ECSD/"
} else {
  f_path = "/vol/tensusers/timzee/af_classification/"
  cgn_path = "/vol/tensusers/timzee/cgn/"
  ifadv_path = "/vol/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/vol/tensusers/timzee/ECSD/"
}

prop_kaldi_in_tim = function(a, b, c, d) {
  x3 = d - c
  x2 = d - b
  x1 = a - c
  if (x1 <= 0) {
    x1 = 0
  } else if (x1 > x3) {
    x1 = x3
  }
  if (x2 <= 0) {
    x2 = 0
  } else if (x2 > x3) {
    x2 = x3
  }
  prop = (x3 - (x1 + x2)) / x3
  return(prop)
}

prop_kaldi_after_tim = function(a, b, c, d) {
  x3 = d - c
  x2 = d - b
  x1 = a - c
  if (x1 <= 0) {
    x1 = 0
  } else if (x1 > x3) {
    x1 = x3
  }
  if (x2 <= 0) {
    x2 = 0
  } else if (x2 > x3) {
    x2 = x3
  }
  prop = x2 / x3
  return(prop)
}

prop_kaldi_before_tim = function(a, b, c, d) {
  x3 = d - c
  x2 = d - b
  x1 = a - c
  if (x1 <= 0) {
    x1 = 0
  } else if (x1 > x3) {
    x1 = x3
  }
  if (x2 <= 0) {
    x2 = 0
  } else if (x2 > x3) {
    x2 = x3
  }
  prop = x1 / x3
  return(prop)
}

bounds = read.csv(paste(cgn_path, "eval_boundaries_nn_corp_300.csv", sep = ""))
bounds$cgn_comp = as.factor(substr(bounds$wav, 1, 1))

# bounds$prop_kal_in_tim = mapply(prop_kaldi_in_tim, 
#                                 bounds$tim_start,
#                                 bounds$tim_end,
#                                 bounds$kal_start,
#                                 bounds$kal_end)
# bounds$prop_kal_post_tim = mapply(prop_kaldi_after_tim, 
#                                 bounds$tim_start,
#                                 bounds$tim_end,
#                                 bounds$kal_start,
#                                 bounds$kal_end)
# bounds$prop_kal_pre_tim = mapply(prop_kaldi_before_tim, 
#                                 bounds$tim_start,
#                                 bounds$tim_end,
#                                 bounds$kal_start,
#                                 bounds$kal_end)
# 
# plot(density(bounds$prop_kal_in_tim))
# mean(bounds$prop_kal_in_tim)
# plot(density(bounds$prop_kal_post_tim))
# mean(bounds$prop_kal_post_tim)
# plot(density(bounds$prop_kal_pre_tim))
# mean(bounds$prop_kal_pre_tim)

bounds$tim_dur = bounds$tim_end - bounds$tim_start
bounds$kal_dur = bounds$kal_end - bounds$kal_start
bounds$nn_dur = bounds$nn_end - bounds$nn_start

# thresh = 0.6
# f_dur = 0.025
# min_dur = 1 / ((thresh - 0.5) * 2) * (f_dur/2)
# # we only need to take into account the intervals that are long enough to be used
# nrow(bounds[(bounds$prop_kal_in_tim > thresh) & (bounds$tim_dur > min_dur),]) / nrow(bounds[bounds$tim_dur > min_dur,])


bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
mean(abs(bounds[bounds$cgn_comp == "a",]$tim_min_kal_dur))
mean(abs(bounds[bounds$cgn_comp == "o",]$tim_min_kal_dur))
bounds$kal_min_nn_dur = bounds$kal_dur - bounds$nn_dur
bounds$tim_min_nn_dur = bounds$tim_dur - bounds$nn_dur
mean(abs(bounds[bounds$cgn_comp == "a"
#                & abs(bounds$kal_min_nn_dur) < 0.05
                ,]$tim_min_nn_dur), na.rm = TRUE)
mean(abs(bounds[bounds$cgn_comp == "o"
#                & abs(bounds$kal_min_nn_dur) < 0.05
                ,]$tim_min_nn_dur), na.rm = TRUE)
plot(density(bounds[bounds$cgn_comp == "a"
#                     & abs(bounds$kal_min_nn_dur) < 0.05
                     ,]$tim_min_nn_dur, na.rm = TRUE), xlim = c(-0.2, 0.2), lty = "dashed", main = "Duration difference to reference in CGN-A")
lines(density(bounds[bounds$cgn_comp == "a",]$tim_min_kal_dur))
abline(v = 0)
legend("right", legend = c("KALDI", "NN"),
       col = c("black", "black"),
       lty = c("solid", "dashed"), cex=0.7)
plot(density(bounds[bounds$cgn_comp == "o"
#                     & abs(bounds$kal_min_nn_dur) < 0.05
                     ,]$tim_min_nn_dur, na.rm = TRUE), xlim = c(-0.2, 0.2), lty = "dashed", main = "Duration difference to reference in CGN-O")
lines(density(bounds[bounds$cgn_comp == "o",]$tim_min_kal_dur))
abline(v = 0)
legend("right", legend = c("KALDI", "NN"),
       col = c("black", "black"),
       lty = c("solid", "dashed"), cex=0.7)

bounds$tim_min_kal_start = bounds$tim_start - bounds$kal_start
#mean(abs(bounds[bounds$cgn_comp == "a",]$tim_min_kal_start))
#mean(abs(bounds[bounds$cgn_comp == "o",]$tim_min_kal_start))
bounds$kal_min_nn_start = bounds$kal_start - bounds$nn_start
bounds$tim_min_nn_start = bounds$tim_start - bounds$nn_start
mean(abs(bounds[bounds$cgn_comp == "a"
#                 & abs(bounds$kal_min_nn_start) < 0.1
                 ,]$tim_min_kal_start), na.rm = TRUE)
mean(abs(bounds[bounds$cgn_comp == "o"
#                & abs(bounds$kal_min_nn_start) < 0.1
                ,]$tim_min_kal_start), na.rm = TRUE)
mean(abs(bounds[bounds$cgn_comp == "a"
#                & abs(bounds$kal_min_nn_start) < 0.1
                ,]$tim_min_nn_start), na.rm = TRUE)
mean(abs(bounds[bounds$cgn_comp == "o"
#                & abs(bounds$kal_min_nn_start) < 0.1
                ,]$tim_min_nn_start), na.rm = TRUE)
plot(density(bounds[bounds$cgn_comp == "a"
#                    & abs(bounds$kal_min_nn_start) < 0.1
                    ,]$tim_min_kal_start, na.rm = TRUE))
lines(density(bounds[bounds$cgn_comp == "a"
#                     & abs(bounds$kal_min_nn_start) < 0.1
                     ,]$tim_min_nn_start, na.rm = TRUE), lty = "dashed")
abline(v = 0)
plot(density(bounds[bounds$cgn_comp == "o"
#                    & abs(bounds$kal_min_nn_start) < 0.1
                    ,]$tim_min_kal_start, na.rm = TRUE))
lines(density(bounds[bounds$cgn_comp == "o"
#                     & abs(bounds$kal_min_nn_start) < 0.1
                     ,]$tim_min_nn_start, na.rm = TRUE), lty = "dashed")
abline(v = 0)

bounds$tim_min_kal_end = bounds$tim_end - bounds$kal_end
#mean(abs(bounds[bounds$cgn_comp == "a",]$tim_min_kal_end))
#mean(abs(bounds[bounds$cgn_comp == "o",]$tim_min_kal_end))
bounds$kal_min_nn_end = bounds$kal_end - bounds$nn_end
bounds$tim_min_nn_end = bounds$tim_end - bounds$nn_end
mean(abs(bounds[bounds$cgn_comp == "a" 
#                 & abs(bounds$kal_min_nn_end) < 0.1
                 ,]$tim_min_kal_end), na.rm = TRUE)
mean(abs(bounds[bounds$cgn_comp == "o" 
#                & abs(bounds$kal_min_nn_end) < 0.1
                ,]$tim_min_kal_end), na.rm = TRUE)
mean(abs(bounds[bounds$cgn_comp == "a" 
#                & abs(bounds$kal_min_nn_end) < 0.1
                ,]$tim_min_nn_end), na.rm = TRUE)
mean(abs(bounds[bounds$cgn_comp == "o" 
#                & abs(bounds$kal_min_nn_end) < 0.1
                ,]$tim_min_nn_end), na.rm = TRUE)
plot(density(bounds[bounds$cgn_comp == "a"
#                    & abs(bounds$kal_min_nn_end) < 0.1
                    ,]$tim_min_kal_end, na.rm = TRUE), xlim = c(-0.2, 0.2), main = "End boundary in CGN-A")
lines(density(bounds[bounds$cgn_comp == "a" 
#                     & abs(bounds$kal_min_nn_end) < 0.1
                     ,]$tim_min_nn_end, na.rm = TRUE), lty = "dashed")
abline(v = 0)
legend("right", legend = c("KALDI", "NN"),
       col = c("black", "black"),
       lty = c("solid", "dashed"), cex=0.7)
plot(density(bounds[bounds$cgn_comp == "o" 
#                     & abs(bounds$kal_min_nn_end) < 0.1
                     ,]$tim_min_nn_end, na.rm = TRUE), xlim = c(-0.2, 0.2), lty = "dashed", main = "End boundary in CGN-O")
lines(density(bounds[bounds$cgn_comp == "o"
#                    & abs(bounds$kal_min_nn_end) < 0.1
                    ,]$tim_min_kal_end, na.rm = TRUE))
abline(v = 0)
legend("right", legend = c("KALDI", "NN"),
       col = c("black", "black"),
       lty = c("solid", "dashed"), cex=0.7)


# now let's take NN end boundaries and calculate new durations
bounds$new_dur = bounds$nn_end - bounds$kal_start + 0.01
bounds$tim_min_new_dur = bounds$tim_dur - bounds$new_dur
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
mean(abs(bounds[bounds$cgn_comp == "a",]$tim_min_kal_dur))
mean(abs(bounds[bounds$cgn_comp == "o",]$tim_min_kal_dur))
mean(abs(bounds[bounds$cgn_comp == "a",]$tim_min_new_dur), na.rm = TRUE)
mean(abs(bounds[bounds$cgn_comp == "o",]$tim_min_new_dur), na.rm = TRUE)

plot(density(bounds[bounds$cgn_comp == "a"
                     ,]$tim_min_new_dur, na.rm = TRUE), lty = "dashed")
lines(density(bounds[bounds$cgn_comp == "a",]$tim_min_kal_dur))
abline(v = 0)
plot(density(bounds[bounds$cgn_comp == "o"
                     ,]$tim_min_new_dur, na.rm = TRUE), lty = "dashed")
lines(density(bounds[bounds$cgn_comp == "o",]$tim_min_kal_dur))
abline(v = 0)


par(mfrow = c(2,4), mar=c(2,2,2,1), oma=c(0,0,2,0))

### check k
bounds = read.csv(paste(cgn_path, "eval_boundaries_k.csv", sep = ""))

bounds$tim_min_nn_end = bounds$tim_end - bounds$nn_end
bounds$tim_min_kal_end = bounds$tim_end - bounds$kal_end
mean(abs(bounds$tim_min_nn_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_nn_end) < 0.02,]) / nrow(bounds)
mean(abs(bounds$tim_min_kal_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_kal_end) < 0.02,]) / nrow(bounds)
plot(density(bounds$tim_min_nn_end, na.rm = TRUE), xlim = c(-0.1, 0.1), main = "CGN-K")
lines(density(bounds$tim_min_kal_end), lty = "dashed")
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)

### check ifadv
bounds = read.csv(paste(ifadv_path, "eval_boundaries_ifadv.csv", sep = ""))

bounds$tim_min_nn_end = bounds$tim_end - bounds$nn_end
bounds$tim_min_kal_end = bounds$tim_end - bounds$kal_end
mean(abs(bounds$tim_min_nn_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_nn_end) < 0.02,]) / nrow(bounds)
mean(abs(bounds$tim_min_kal_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_kal_end) < 0.02,]) / nrow(bounds)
plot(density(bounds$tim_min_nn_end, na.rm = TRUE), xlim = c(-0.1, 0.1), main = "IFADV")
lines(density(bounds$tim_min_kal_end), lty = "dashed")
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)

### check ecsd
bounds = read.csv(paste(ecsd_path, "eval_boundaries_ecsd.csv", sep = ""))

bounds$tim_min_nn_end = bounds$tim_end - bounds$nn_end
bounds$tim_min_kal_end = bounds$tim_end - bounds$kal_end
mean(abs(bounds$tim_min_nn_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_nn_end) < 0.02,]) / nrow(bounds)
mean(abs(bounds$tim_min_kal_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_kal_end) < 0.02,]) / nrow(bounds)
plot(density(bounds$tim_min_nn_end, na.rm = TRUE), xlim = c(-0.1, 0.1), main = "ECSD")
lines(density(bounds$tim_min_kal_end), lty = "dashed")
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)

### check o
bounds = read.csv(paste(cgn_path, "eval_boundaries_o.csv", sep = ""))

bounds$tim_min_nn_end = bounds$tim_end - bounds$nn_end
bounds$tim_min_kal_end = bounds$tim_end - bounds$kal_end
mean(abs(bounds$tim_min_nn_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_nn_end) < 0.02,]) / nrow(bounds)
mean(abs(bounds$tim_min_kal_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_kal_end) < 0.02,]) / nrow(bounds)
plot(density(bounds$tim_min_nn_end, na.rm = TRUE), xlim = c(-0.1, 0.1), main = "CGN-O")
lines(density(bounds$tim_min_kal_end), lty = "dashed")
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)


### check c
bounds = read.csv(paste(cgn_path, "eval_boundaries_c.csv", sep = ""))

bounds$tim_min_nn_end = bounds$tim_end - bounds$nn_end
bounds$tim_min_kal_end = bounds$tim_end - bounds$kal_end
mean(abs(bounds$tim_min_nn_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_nn_end) < 0.02,]) / nrow(bounds)
mean(abs(bounds$tim_min_kal_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_kal_end) < 0.02,]) / nrow(bounds)
plot(density(bounds$tim_min_nn_end, na.rm = TRUE), xlim = c(-0.1, 0.1), main = "CGN-C")
lines(density(bounds$tim_min_kal_end), lty = "dashed")
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)

### check d
bounds = read.csv(paste(cgn_path, "eval_boundaries_d.csv", sep = ""))

bounds$tim_min_nn_end = bounds$tim_end - bounds$nn_end
bounds$tim_min_kal_end = bounds$tim_end - bounds$kal_end
mean(abs(bounds$tim_min_nn_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_nn_end) < 0.02,]) / nrow(bounds)
mean(abs(bounds$tim_min_kal_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_kal_end) < 0.02,]) / nrow(bounds)
plot(density(bounds$tim_min_nn_end, na.rm = TRUE), xlim = c(-0.1, 0.1), main = "CGN-D")
lines(density(bounds$tim_min_kal_end), lty = "dashed")
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)


### check a
bounds = read.csv(paste(cgn_path, "eval_boundaries_a.csv", sep = ""))

bounds$tim_min_nn_end = bounds$tim_end - bounds$nn_end
bounds$tim_min_kal_end = bounds$tim_end - bounds$kal_end
mean(abs(bounds$tim_min_nn_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_nn_end) < 0.02,]) / nrow(bounds)
mean(abs(bounds$tim_min_kal_end), na.rm = TRUE)
nrow(bounds[abs(bounds$tim_min_kal_end) < 0.02,]) / nrow(bounds)
plot(density(bounds$tim_min_nn_end, na.rm = TRUE), xlim = c(-0.1, 0.1), main = "CGN-A")
lines(density(bounds$tim_min_kal_end), lty = "dashed")
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)


plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left", legend = c("FA", "NN"),
       col = c("black", "black"),
       lty = c("dashed", "solid"), cex=1, box.lwd = 0)

mtext("End boundary difference to reference", side = 3, outer = TRUE)




