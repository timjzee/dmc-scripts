if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/classifier_evaluation/s/"
} else {
  f_path = "/vol/tensusers/timzee/classifier_evaluation/s/"
}

bounds = read.csv(paste(f_path, "s_eval_results.csv", sep = ""))
bounds$corpus = as.factor(ifelse(substr(bounds$wav, 1, 1) %in% c("a", "c", "d", "k", "o"), paste("cgn-", substr(bounds$wav, 1, 1), sep = ""), ifelse(substr(bounds$wav, 1, 1) == "D", "ifadv", "ecsd")))

bounds$TR_TS_start_b = bounds$TR_start_b - bounds$TS_start_b
bounds$TR_nn_start_b = bounds$TR_start_b - bounds$nn_start_b
bounds$TR_kal_start_b = bounds$TR_start_b - bounds$kal_b
bounds$TS_nn_start_b = bounds$TS_start_b - bounds$nn_start_b
bounds$TS_TR_start_b = bounds$TS_start_b - bounds$TR_start_b

bounds$TR_TS_start_e = bounds$TR_start_e - bounds$TS_start_e
bounds$TR_nn_start_e = bounds$TR_start_e - bounds$nn_start_e
bounds$TS_nn_start_e = bounds$TS_start_e - bounds$nn_start_e
bounds$TS_TR_start_e = bounds$TS_start_e - bounds$TR_start_e

bounds$TR_TS_end_b = bounds$TR_end_b - bounds$TS_end_b
bounds$TR_nn_end_b = bounds$TR_end_b - bounds$nn_end_b
bounds$TS_nn_end_b = bounds$TS_end_b - bounds$nn_end_b
bounds$TS_TR_end_b = bounds$TS_end_b - bounds$TR_end_b

bounds$TR_TS_end_e = bounds$TR_end_e - bounds$TS_end_e
bounds$TR_nn_end_e = bounds$TR_end_e - bounds$nn_end_e
bounds$TR_wrd_end_e = bounds$TR_end_e - bounds$wrd_e
bounds$TR_kal_end_e = bounds$TR_end_e - bounds$kal_e
bounds$TS_nn_end_e = bounds$TS_end_e - bounds$nn_end_e
bounds$TS_TR_end_e = bounds$TS_end_e - bounds$TR_end_e

#bounds = na.omit(bounds)

par(mfrow = c(2,4), mar=c(2,2,2,1), oma=c(0,0,2,0))

bounds_a = bounds[bounds$corpus == "cgn-a",]
mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_b")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_b")]))
mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(na.omit(bounds_a[,c("TR_kal_start_b")]))
plot(density(bounds_a$TR_nn_start_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-A")
lines(density(bounds_a$TR_TS_start_b, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_kal_start_b, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-c",]
mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_b")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_b")]))
mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(na.omit(bounds_a[,c("TR_kal_start_b")]))
plot(density(bounds_a$TR_nn_start_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-C")
lines(density(bounds_a$TR_TS_start_b, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_kal_start_b, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-d",]
mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_b")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_b")]))
mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(na.omit(bounds_a[,c("TR_kal_start_b")]))
plot(density(bounds_a$TR_nn_start_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-D")
lines(density(bounds_a$TR_TS_start_b, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_kal_start_b, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-k",]
mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_b")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_b")]))
mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(na.omit(bounds_a[,c("TR_kal_start_b")]))
plot(density(bounds_a$TR_nn_start_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-K")
lines(density(bounds_a$TR_TS_start_b, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_kal_start_b, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-o",]
mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_b")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_b")]))
mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(na.omit(bounds_a[,c("TR_kal_start_b")]))
plot(density(bounds_a$TR_nn_start_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 140), main = "CGN-O")
lines(density(bounds_a$TR_TS_start_b, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_kal_start_b, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "ifadv",]
mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_b")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_b")]))
mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(na.omit(bounds_a[,c("TR_kal_start_b")]))
plot(density(bounds_a$TR_nn_start_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 140), main = "IFADV")
lines(density(bounds_a$TR_TS_start_b, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_kal_start_b, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "ecsd",]
mean(abs(bounds_a$TR_TS_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_b) < 0.02,c("TR_TS_start_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_b")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_b) < 0.02,c("TR_nn_start_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_b")]))
mean(abs(bounds_a$TR_kal_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_start_b) < 0.02,c("TR_kal_start_b")])) / NROW(na.omit(bounds_a[,c("TR_kal_start_b")]))
plot(density(bounds_a$TR_nn_start_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 140), main = "ECSD")
lines(density(bounds_a$TR_TS_start_b, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_kal_start_b, na.rm= TRUE), lty = "longdash")
abline(v = 0)

plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left", legend = c("TR-TS", "TR-NN", "TR-KAL"),
       col = c("black", "black", "black"),
       lty = c("dashed", "solid", "longdash"), cex=1, box.lwd = 0)

mtext("Start 1st boundary", side = 3, outer = TRUE)

######################

par(mfrow = c(2,4), mar=c(2,2,2,1), oma=c(0,0,2,0))

bounds_a = bounds[bounds$corpus == "cgn-a",]
mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_e")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_e")]))
plot(density(bounds_a$TR_nn_start_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-A")
lines(density(bounds_a$TR_TS_start_e, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-c",]
mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_e")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_e")]))
plot(density(bounds_a$TR_nn_start_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-C")
lines(density(bounds_a$TR_TS_start_e, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-d",]
mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_e")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_e")]))
plot(density(bounds_a$TR_nn_start_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-D")
lines(density(bounds_a$TR_TS_start_e, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-k",]
mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_e")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_e")]))
plot(density(bounds_a$TR_nn_start_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 120), main = "CGN-K")
lines(density(bounds_a$TR_TS_start_e, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-o",]
mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_e")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_e")]))
plot(density(bounds_a$TR_nn_start_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-O")
lines(density(bounds_a$TR_TS_start_e, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "ifadv",]
mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_e")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_e")]))
plot(density(bounds_a$TR_nn_start_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "IFADV")
lines(density(bounds_a$TR_TS_start_e, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "ecsd",]
mean(abs(bounds_a$TR_TS_start_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_start_e) < 0.02,c("TR_TS_start_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_start_e")]))
mean(abs(bounds_a$TR_nn_start_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_start_e) < 0.02,c("TR_nn_start_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_start_e")]))
plot(density(bounds_a$TR_nn_start_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "ECSD")
lines(density(bounds_a$TR_TS_start_e, na.rm= TRUE), lty = "dashed")
abline(v = 0)

plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left", legend = c("TR-TS", "TR-NN"),
       col = c("black", "black"),
       lty = c("dashed", "solid"), cex=1, box.lwd = 0)

mtext("Start 2nd boundary", side = 3, outer = TRUE)

########################

par(mfrow = c(2,4), mar=c(2,2,2,1), oma=c(0,0,2,0))

bounds_a = bounds[bounds$corpus == "cgn-a",]
mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_b")]))
mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_b")]))
plot(density(bounds_a$TR_nn_end_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-A")
lines(density(bounds_a$TR_TS_end_b, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-c",]
mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_b")]))
mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_b")]))
plot(density(bounds_a$TR_nn_end_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-C")
lines(density(bounds_a$TR_TS_end_b, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-d",]
mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_b")]))
mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_b")]))
plot(density(bounds_a$TR_nn_end_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-D")
lines(density(bounds_a$TR_TS_end_b, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-k",]
mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_b")]))
mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_b")]))
plot(density(bounds_a$TR_nn_end_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-K")
lines(density(bounds_a$TR_TS_end_b, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-o",]
mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_b")]))
mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_b")]))
plot(density(bounds_a$TR_nn_end_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-O")
lines(density(bounds_a$TR_TS_end_b, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "ifadv",]
mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_b")]))
mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_b")]))
plot(density(bounds_a$TR_nn_end_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "IFADV")
lines(density(bounds_a$TR_TS_end_b, na.rm= TRUE), lty = "dashed")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "ecsd",]
mean(abs(bounds_a$TR_TS_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_b) < 0.02,c("TR_TS_end_b")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_b")]))
mean(abs(bounds_a$TR_nn_end_b), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_b) < 0.02,c("TR_nn_end_b")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_b")]))
plot(density(bounds_a$TR_nn_end_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "ECSD")
lines(density(bounds_a$TR_TS_end_b, na.rm= TRUE), lty = "dashed")
abline(v = 0)

plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left", legend = c("TR-TS", "TR-NN"),
       col = c("black", "black"),
       lty = c("dashed", "solid"), cex=1, box.lwd = 0)

mtext("End 1st boundary", side = 3, outer = TRUE)

#####################

par(mfrow = c(2,4), mar=c(2,2,2,1), oma=c(0,0,2,0))

bounds_a = bounds[bounds$corpus == "cgn-a",]
mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_e")]))
mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_e")]))
mean(abs(bounds_a$TR_wrd_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_wrd_end_e) < 0.02,c("TR_wrd_end_e")])) / NROW(na.omit(bounds_a[,c("TR_wrd_end_e")]))
mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(na.omit(bounds_a[,c("TR_kal_end_e")]))
plot(density(bounds_a$TR_nn_end_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-A")
lines(density(bounds_a$TR_TS_end_e, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_wrd_end_e, na.rm= TRUE), lty = "dotted")
lines(density(bounds_a$TR_kal_end_e, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-c",]
mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_e")]))
mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_e")]))
mean(abs(bounds_a$TR_wrd_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_wrd_end_e) < 0.02,c("TR_wrd_end_e")])) / NROW(na.omit(bounds_a[,c("TR_wrd_end_e")]))
mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(na.omit(bounds_a[,c("TR_kal_end_e")]))
plot(density(bounds_a$TR_nn_end_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-C")
lines(density(bounds_a$TR_TS_end_e, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_wrd_end_e, na.rm= TRUE), lty = "dotted")
lines(density(bounds_a$TR_kal_end_e, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-d",]
mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_e")]))
mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_e")]))
mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(na.omit(bounds_a[,c("TR_kal_end_e")]))
plot(density(bounds_a$TR_nn_end_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-D")
lines(density(bounds_a$TR_TS_end_e, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_kal_end_e, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-k",]
mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_e")]))
mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_e")]))
mean(abs(bounds_a$TR_wrd_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_wrd_end_e) < 0.02,c("TR_wrd_end_e")])) / NROW(na.omit(bounds_a[,c("TR_wrd_end_e")]))
mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(na.omit(bounds_a[,c("TR_kal_end_e")]))
plot(density(bounds_a$TR_nn_end_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-K")
lines(density(bounds_a$TR_TS_end_e, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_wrd_end_e, na.rm= TRUE), lty = "dotted")
lines(density(bounds_a$TR_kal_end_e, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "cgn-o",]
mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_e")]))
mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_e")]))
mean(abs(bounds_a$TR_wrd_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_wrd_end_e) < 0.02,c("TR_wrd_end_e")])) / NROW(na.omit(bounds_a[,c("TR_wrd_end_e")]))
mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(na.omit(bounds_a[,c("TR_kal_end_e")]))
plot(density(bounds_a$TR_nn_end_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "CGN-O")
lines(density(bounds_a$TR_TS_end_e, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_wrd_end_e, na.rm= TRUE), lty = "dotted")
lines(density(bounds_a$TR_kal_end_e, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "ifadv",]
mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_e")]))
mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_e")]))
mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(na.omit(bounds_a[,c("TR_kal_end_e")]))
plot(density(bounds_a$TR_nn_end_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "ifadv")
lines(density(bounds_a$TR_TS_end_e, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_kal_end_e, na.rm= TRUE), lty = "longdash")
abline(v = 0)

bounds_a = bounds[bounds$corpus == "ecsd",]
mean(abs(bounds_a$TR_TS_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_TS_end_e) < 0.02,c("TR_TS_end_e")])) / NROW(na.omit(bounds_a[,c("TR_TS_end_e")]))
mean(abs(bounds_a$TR_nn_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_nn_end_e) < 0.02,c("TR_nn_end_e")])) / NROW(na.omit(bounds_a[,c("TR_nn_end_e")]))
mean(abs(bounds_a$TR_kal_end_e), na.rm = TRUE)
NROW(na.omit(bounds_a[abs(bounds_a$TR_kal_end_e) < 0.02,c("TR_kal_end_e")])) / NROW(na.omit(bounds_a[,c("TR_kal_end_e")]))
plot(density(bounds_a$TR_nn_end_e, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "ECSD")
lines(density(bounds_a$TR_TS_end_e, na.rm= TRUE), lty = "dashed")
lines(density(bounds_a$TR_kal_end_e, na.rm= TRUE), lty = "longdash")
abline(v = 0)

plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left", legend = c("TR-TS", "TR-NN", "TR-WRD", "TR-KAL"),
       col = c("black", "black", "black", "black"),
       lty = c("dashed", "solid", "dotted", "longdash"), cex=1, box.lwd = 0)

mtext("End 2nd boundary", side = 3, outer = TRUE)

######################



