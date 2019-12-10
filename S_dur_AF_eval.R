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

bounds$tim_dur = bounds$tim_end - bounds$tim_start
bounds$kal_dur = bounds$kal_end - bounds$kal_start
bounds$nn_dur = bounds$nn_end - bounds$nn_start
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
bounds$kal_min_nn_dur = bounds$kal_dur - bounds$nn_dur
bounds$tim_min_nn_dur = bounds$tim_dur - bounds$nn_dur
# mean(abs(bounds$tim_min_nn_dur), na.rm = TRUE)
# mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
# plot(density(bounds$tim_min_nn_dur, na.rm = TRUE), xlim = c(-0.2, 0.2), lty = "dashed", main = "Duration difference to reference in CGN-K")
# lines(density(bounds$tim_min_kal_dur))
# abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)
bounds$new_dur = bounds$nn_end - bounds$kal_start + 0.01
bounds$tim_min_new_dur = bounds$tim_dur - bounds$new_dur
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
mean(abs(bounds$tim_min_new_dur), na.rm = TRUE)
mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
plot(density(bounds$tim_min_new_dur, na.rm = TRUE), xlim = c(-0.1, 0.1), lty = "dashed", 
     main = "CGN-K")
lines(density(bounds$tim_min_kal_dur))
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)

### check ifadv
bounds = read.csv(paste(ifadv_path, "eval_boundaries_ifadv.csv", sep = ""))

bounds$tim_dur = bounds$tim_end - bounds$tim_start
bounds$kal_dur = bounds$kal_end - bounds$kal_start
bounds$nn_dur = bounds$nn_end - bounds$nn_start
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
bounds$kal_min_nn_dur = bounds$kal_dur - bounds$nn_dur
bounds$tim_min_nn_dur = bounds$tim_dur - bounds$nn_dur
# mean(abs(bounds$tim_min_nn_dur), na.rm = TRUE)
# mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
# plot(density(bounds$tim_min_nn_dur, na.rm = TRUE), xlim = c(-0.2, 0.2), lty = "dashed", main = "Duration difference to reference in IFADV")
# lines(density(bounds$tim_min_kal_dur))
# abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)
bounds$new_dur = bounds$nn_end - bounds$kal_start + 0.01
bounds$tim_min_new_dur = bounds$tim_dur - bounds$new_dur
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
mean(abs(bounds$tim_min_new_dur), na.rm = TRUE)
mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
plot(density(bounds$tim_min_new_dur, na.rm = TRUE), xlim = c(-0.1, 0.1), lty = "dashed", 
     main = "IFADV")
lines(density(bounds$tim_min_kal_dur))
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)

### check ecsd
bounds = read.csv(paste(ecsd_path, "eval_boundaries_ecsd.csv", sep = ""))

bounds$tim_dur = bounds$tim_end - bounds$tim_start
bounds$kal_dur = bounds$kal_end - bounds$kal_start
bounds$nn_dur = bounds$nn_end - bounds$nn_start
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
bounds$kal_min_nn_dur = bounds$kal_dur - bounds$nn_dur
bounds$tim_min_nn_dur = bounds$tim_dur - bounds$nn_dur
# mean(abs(bounds$tim_min_nn_dur), na.rm = TRUE)
# mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
# plot(density(bounds$tim_min_nn_dur, na.rm = TRUE), xlim = c(-0.2, 0.2), lty = "dashed", main = "Duration difference to reference in ECSD")
# lines(density(bounds$tim_min_kal_dur))
# abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)
bounds$new_dur = bounds$nn_end - bounds$kal_start + 0.01
bounds$tim_min_new_dur = bounds$tim_dur - bounds$new_dur
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
mean(abs(bounds$tim_min_new_dur), na.rm = TRUE)
mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
plot(density(bounds$tim_min_new_dur, na.rm = TRUE), xlim = c(-0.1, 0.1), lty = "dashed", 
     main = "ECSD")
lines(density(bounds$tim_min_kal_dur))
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)

### check o
bounds = read.csv(paste(cgn_path, "eval_boundaries_o.csv", sep = ""))

bounds$tim_dur = bounds$tim_end - bounds$tim_start
bounds$kal_dur = bounds$kal_end - bounds$kal_start
bounds$nn_dur = bounds$nn_end - bounds$nn_start
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
bounds$kal_min_nn_dur = bounds$kal_dur - bounds$nn_dur
bounds$tim_min_nn_dur = bounds$tim_dur - bounds$nn_dur
# mean(abs(bounds$tim_min_nn_dur), na.rm = TRUE)
# mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
# plot(density(bounds$tim_min_nn_dur, na.rm = TRUE), xlim = c(-0.2, 0.2), lty = "dashed", main = "Duration difference to reference in CGN-O")
# lines(density(bounds$tim_min_kal_dur))
# abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)
bounds$new_dur = bounds$nn_end - bounds$kal_start + 0.01
bounds$tim_min_new_dur = bounds$tim_dur - bounds$new_dur
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
mean(abs(bounds$tim_min_new_dur), na.rm = TRUE)
mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
plot(density(bounds$tim_min_new_dur, na.rm = TRUE), xlim = c(-0.1, 0.1), lty = "dashed", 
     main = "CGN-O")
lines(density(bounds$tim_min_kal_dur))
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)

### check c
bounds = read.csv(paste(cgn_path, "eval_boundaries_c.csv", sep = ""))

bounds$tim_dur = bounds$tim_end - bounds$tim_start
bounds$kal_dur = bounds$kal_end - bounds$kal_start
bounds$nn_dur = bounds$nn_end - bounds$nn_start
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
bounds$kal_min_nn_dur = bounds$kal_dur - bounds$nn_dur
bounds$tim_min_nn_dur = bounds$tim_dur - bounds$nn_dur
# mean(abs(bounds$tim_min_nn_dur), na.rm = TRUE)
# mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
# plot(density(bounds$tim_min_nn_dur, na.rm = TRUE), xlim = c(-0.2, 0.2), lty = "dashed", main = "Duration difference to reference in CGN-C")
# lines(density(bounds$tim_min_kal_dur))
# abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)
bounds$new_dur = bounds$nn_end - bounds$kal_start + 0.01
bounds$tim_min_new_dur = bounds$tim_dur - bounds$new_dur
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
mean(abs(bounds$tim_min_new_dur), na.rm = TRUE)
mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
plot(density(bounds$tim_min_kal_dur, na.rm = TRUE), xlim = c(-0.1, 0.1), main = "CGN-C")
lines(density(bounds$tim_min_new_dur), lty = "dashed")
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)

### check d
bounds = read.csv(paste(cgn_path, "eval_boundaries_d.csv", sep = ""))

bounds$tim_dur = bounds$tim_end - bounds$tim_start
bounds$kal_dur = bounds$kal_end - bounds$kal_start
bounds$nn_dur = bounds$nn_end - bounds$nn_start
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
bounds$kal_min_nn_dur = bounds$kal_dur - bounds$nn_dur
bounds$tim_min_nn_dur = bounds$tim_dur - bounds$nn_dur
# mean(abs(bounds$tim_min_nn_dur), na.rm = TRUE)
# mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
# plot(density(bounds$tim_min_nn_dur, na.rm = TRUE), xlim = c(-0.2, 0.2), lty = "dashed", main = "Duration difference to reference in CGN-D")
# lines(density(bounds$tim_min_kal_dur))
# abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)
bounds$new_dur = bounds$nn_end - bounds$kal_start + 0.01
bounds$tim_min_new_dur = bounds$tim_dur - bounds$new_dur
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
mean(abs(bounds$tim_min_new_dur), na.rm = TRUE)
mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
plot(density(bounds$tim_min_new_dur, na.rm = TRUE), xlim = c(-0.1, 0.1), lty = "dashed", 
     main = "CGN-D")
lines(density(bounds$tim_min_kal_dur))
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)

### check a
bounds = read.csv(paste(cgn_path, "eval_boundaries_a.csv", sep = ""))

bounds$tim_dur = bounds$tim_end - bounds$tim_start
bounds$kal_dur = bounds$kal_end - bounds$kal_start
bounds$nn_dur = bounds$nn_end - bounds$nn_start
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
bounds$kal_min_nn_dur = bounds$kal_dur - bounds$nn_dur
bounds$tim_min_nn_dur = bounds$tim_dur - bounds$nn_dur
# mean(abs(bounds$tim_min_nn_dur), na.rm = TRUE)
# mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
# plot(density(bounds$tim_min_nn_dur, na.rm = TRUE), xlim = c(-0.2, 0.2), lty = "dashed", main = "Duration difference to reference in CGN-A")
# lines(density(bounds$tim_min_kal_dur))
# abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)
bounds$new_dur = bounds$nn_end - bounds$kal_start + 0.01
bounds$tim_min_new_dur = bounds$tim_dur - bounds$new_dur
bounds$tim_min_kal_dur = bounds$tim_dur - bounds$kal_dur
mean(abs(bounds$tim_min_new_dur), na.rm = TRUE)
mean(abs(bounds$tim_min_kal_dur), na.rm = TRUE)
plot(density(bounds$tim_min_kal_dur, na.rm = TRUE), xlim = c(-0.1, 0.1), main = "CGN-A")
lines(density(bounds$tim_min_new_dur), lty = "dashed")
abline(v = 0)
# legend("right", legend = c("KALDI", "NN"),
#        col = c("black", "black"),
#        lty = c("solid", "dashed"), cex=0.7)


plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left", legend = c("KALDI", "NN (end bound.)"),
       col = c("black", "black"),
       lty = c("solid", "dashed"), cex=1, box.lwd = 0)

mtext("Duration difference to reference (end bound. only)", side = 3, outer = TRUE)





####### now let's get (test?) data for components 'c', 'd', 'k', 'IFADV', 'ECSD'

s_dur_c = read.csv(paste(cgn_path, "comp-c_s_static_final.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$register = as.factor("conversation")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
s_dur_d = read.csv(paste(cgn_path, "comp-d_s_static_final.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
s_dur_d$register = as.factor("conversation")
s_dur_ifadv = read.csv(paste(ifadv_path, "ifadv_s_static_final.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$register = as.factor("conversation")
s_dur_ifadv$mean_hnr = as.factor(s_dur_ifadv$mean_hnr)
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
s_dur_ecsd = read.csv(paste(ecsd_path, "ecsd_s_static_final.csv", sep = ""))
s_dur_ecsd$corpus = as.factor("ecsd")
s_dur_ecsd$register = as.factor("conversation")
s_dur_k = read.csv(paste(cgn_path, "comp-k_s_static_final.csv", sep = ""))
s_dur_k$corpus = as.factor("cgn-k")
s_dur_k$register = as.factor("news")
s_dur_k$mean_hnr = as.factor(s_dur_k$mean_hnr)
s_dur = rbind(s_dur_c, s_dur_d, s_dur_ifadv, s_dur_ecsd, s_dur_k)

s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)

s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)
s_dur$stress_dist = s_dur$num_syl - s_dur$word_stress
# fix mistake from add_info_script
#s_dur$stress_dist[s_dur$stress_dist < 0] = NA
#s_dur$stressed[s_dur$stress_dist < 0] = NA
#s_dur$num_syl[s_dur$stress_dist < 0] = NA
is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron
# combine ifadv and ecsd to get rid of rank deficiency
#levels(s_dur$corpus) = c("cgn-a", "cgn-c", "cgn-d", "ifadv-ecsd", "ifadv-ecsd")

# get rid of unnecessary columns
drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced", "proportion_voiced2", "s_cog_full", "s_cog_window", "per_mil_wf", 
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "chan")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

# Get rid of GEN-TIME
s_dur = s_dur[s_dur$type_of_s != "GEN-TIME",]
s_dur$type_of_s = as.factor(as.character(s_dur$type_of_s))
s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")

# don't turn 0 into NA
#is.na(s_dur$bigram_f) = !s_dur$bigram_f
s_dur$log_bigf = log10(s_dur$bigram_f + 1)


vowels = c("@", "A", "AU", "E", "E2", "EI", "EU", "I", "O", "U", "UI", "a", "e", "i", "o", "u", "y")
liquids = c("r", "l")
approximants = c("j", "w")
nasals = c("n", "m", "N")
fricatives = c("G", "S", "Z", "f", "h", "s", "v", "x", "z")
plosives = c("b", "d", "g", "k", "p", "t")

get_phon_class = function(x) {
  if (x %in% vowels){
    return("V")
  } else if (x %in% liquids){
    return("L")
  } else if (x %in% approximants){
    return("APP")
  } else if (x %in% nasals){
    return("N")
  } else if (x %in% fricatives){
    return("F")
  } else if (x %in% plosives){
    return("P")
  } else if (!is.na(x) & x == "SIL"){
    return("SIL")
  } else {
    return(NA)
  }
}

s_dur$next_phon_class = as.factor(sapply(s_dur$next_phon_pron, get_phon_class))
s_dur$prev_phon_class = as.factor(sapply(s_dur$prev_phon_pron, get_phon_class))

# see https://statisticalhorizons.com/multicollinearity
# V has most cases by far for the prev_phon_class
s_dur$prev_phon_class = relevel(s_dur$prev_phon_class, ref="V")
s_dur$next_phon_class = relevel(s_dur$next_phon_class, ref="V")

# remove lines for which measurements failed
s_dur = s_dur[!is.na(s_dur$s_dur), ]
# remove NA lines
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

# inspect histogram
hist(s_dur$s_dur)

# remove unrepresentative outliers (Baayen, 2008, p. 243)
s_dur = s_dur[s_dur$s_dur < 0.4,]

# make new predictors and get rid of unnecessary NAs
n_cow = 5052213
n_subtlex = 437504
s_dur$wf = 10^s_dur$log_wf
s_dur[is.na(s_dur$wf),]$wf = 1
s_dur$log_wf = log10(s_dur$wf)
s_dur$p_next_w = (s_dur$bigram_f / n_cow) / (s_dur$wf / n_subtlex)

s_dur[is.na(s_dur$num_syl_pron),]$num_syl_pron = 0

s_dur[is.na(s_dur$lex_neb),]$lex_neb = 0
s_dur[is.na(s_dur$lex_neb_freq),]$lex_neb_freq = 0
#n_lexicon = 251563
s_dur$prop_lex_neb_freq = s_dur$lex_neb_freq / s_dur$wf

s_dur$mean_syl_dur = s_dur$base_dur / s_dur$num_syl_pron

# transform dependent variable
s_dur$log_s_dur = log10(s_dur$s_dur)


# remove lines for which continuous predictors are NA
nrow(s_dur)
s_dur = s_dur[!(is.na(s_dur$speech_rate_pron) | is.na(s_dur$base_dur) 
                | is.na(s_dur$num_syl_pron) | is.na(s_dur$num_cons_pron)
                | is.na(s_dur$log_wf) | is.na(s_dur$lex_neb) | is.na(s_dur$log_bigf)
                | is.na(s_dur$stress_dist)), ]
nrow(s_dur)
table(s_dur$type_of_s, s_dur$corpus)

### try limitting type/token ratio to get rid of collinearity with type_of_s
word_t = table(s_dur$word_ort)
s_dur$ort_freq = as.integer(word_t[s_dur$word_ort])
hist(s_dur$ort_freq, breaks = 500, xlim = c(0,5000))

set.seed(40)

s_dur_freq = s_dur[s_dur$ort_freq > 25,]
s_dur_freq$word_ort = as.factor(as.character(s_dur_freq$word_ort))
s_dur_freq_samp = s_dur_freq[unlist(tapply(1:nrow(s_dur_freq), s_dur_freq$word_ort, sample, 25)),]
s_dur_lim = rbind(s_dur_freq_samp, s_dur[s_dur$ort_freq <= 25,])

table(s_dur_lim$type_of_s, s_dur_lim$corpus)

##
c_data = s_dur_lim[s_dur_lim$corpus == "cgn-c",]
c_data = c_data[sample(nrow(c_data), 100),]
d_data = s_dur_lim[s_dur_lim$corpus == "cgn-d",]
d_data = d_data[sample(nrow(d_data), 100),]
k_data = s_dur_lim[s_dur_lim$corpus == "cgn-k",]
k_data = k_data[sample(nrow(k_data), 100),]
ifadv_data = s_dur_lim[s_dur_lim$corpus == "ifadv",]
ifadv_data = ifadv_data[sample(nrow(ifadv_data), 100),]
ecsd_data = s_dur_lim[s_dur_lim$corpus == "ecsd",]
ecsd_data = ecsd_data[sample(nrow(ecsd_data), 100),]

test_data = rbind(c_data, d_data, k_data, ifadv_data, ecsd_data)
test_data = test_data

write.table(test_data, file = paste(f_path, "nn_eval.csv", sep = ""), 
            row.names=TRUE, col.names=TRUE, sep=",", quote = FALSE)

