if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/classifier_evaluation/s/grid_search_output/"
} else {
  f_path = "/vol/tensusers/timzee/classifier_evaluation/s/grid_search_output/"
}

library(corrplot)

#bounds = read.csv(paste(f_path, "BLSTM_15_0.5_1_10_10_1_1.csv", sep = ""))
bounds = read.csv(paste(f_path, "FFNN_15_0.5_1_10_10_1_1.csv", sep = ""))
bounds$corpus = as.factor(ifelse(substr(bounds$wav, 1, 1) %in% c("a", "c", "d", "k", "o"), paste("cgn-", substr(bounds$wav, 1, 1), sep = ""), ifelse(substr(bounds$wav, 1, 1) == "D", "ifadv", "ecsd")))

bounds$nn_trace_points = lengths(regmatches(as.character(bounds$nn_trace), gregexpr(",", as.character(bounds$nn_trace)))) + 1

max_num_trace_points = max(bounds$nn_trace_points)

counter = 0
for (row_i in 1:nrow(bounds)){
  if (!is.na(bounds$nn_trace[row_i])){
    counter = counter + 1
    y_vals = eval(parse(text=as.character(bounds$nn_trace[row_i])))
    if (counter == 1){
#      plot(y_vals, type = "line", xlim = c(1, max_num_trace_points), ylim = c(0, 1), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.1), main = "Similar BLSTM configuration")
      plot(y_vals, type = "line", xlim = c(1, 50), ylim = c(0, 1), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.1), main = "Best FFNN configuration")
    } else {
      lines(y_vals, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
    }
  }
}

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

bounds$TR_start = bounds$TR_start_e - bounds$TR_start_b
bounds$TR_end = bounds$TR_end_e - bounds$TR_end_b
bounds$TR_max = bounds$TR_end_b - bounds$TR_start_e
bounds$TR_start_end = bounds$TR_start + bounds$TR_end
bounds$TR_start_max = bounds$TR_start + bounds$TR_max
bounds$TR_max_end = bounds$TR_max + bounds$TR_end
bounds$TR_start_max_end = bounds$TR_start + bounds$TR_max + bounds$TR_end
bounds$TR_start_prop = bounds$TR_start / (bounds$TR_start + bounds$TR_max)
bounds$TR_end_prop = bounds$TR_end / (bounds$TR_end + bounds$TR_max)
bounds$TR_start_end_prop = (bounds$TR_start + bounds$TR_end) / (bounds$TR_start + bounds$TR_end + bounds$TR_max)

bounds$nn_start = bounds$nn_start_e - bounds$nn_start_b
bounds$nn_end = bounds$nn_end_e - bounds$nn_end_b
bounds$nn_max = bounds$nn_end_b - bounds$nn_start_e
bounds$nn_start_end = bounds$nn_start + bounds$nn_end
bounds$nn_start_max = bounds$nn_start + bounds$nn_max
bounds$nn_max_end = bounds$nn_max + bounds$nn_end
bounds$nn_start_max_end = bounds$nn_start + bounds$nn_max + bounds$nn_end
bounds$nn_start_prop = bounds$nn_start / (bounds$nn_start + bounds$nn_max)
bounds$nn_end_prop = bounds$nn_end / (bounds$nn_end + bounds$nn_max)
bounds$nn_start_end_prop = (bounds$nn_start + bounds$nn_end) / (bounds$nn_start + bounds$nn_end + bounds$nn_max)

#bounds = na.omit(bounds)

par(mfrow = c(1,1))
corrplot(cor(bounds[, c("TR_start", "TR_max", "TR_end", "TR_start_end", "TR_start_max", "TR_max_end", "TR_start_max_end", "TR_start_prop", "TR_end_prop", "TR_start_end_prop")], use = "complete.obs"), method = "number")
corrplot(cor(na.omit(bounds)[, c("nn_start", "nn_max", "nn_end", "nn_start_end", "nn_start_max", "nn_max_end", "nn_start_max_end", "nn_start_prop", "nn_end_prop", "nn_start_end_prop")], use = "complete.obs"), method = "number")


library(ordinal)
bounds_m = bounds[bounds$TR_reduction != "D",]
bounds_m$TR_reduction = as.factor(as.character(bounds_m$TR_reduction))
m_TR_start = clm(TR_reduction ~ TR_start, data = bounds_m)
m_TR_end = clm(TR_reduction ~ TR_end, data = bounds_m)
m_TR_max = clm(TR_reduction ~ TR_max, data = bounds_m)
m_TR_start_end = clm(TR_reduction ~ TR_start_end, data = bounds_m)
m_TR_start_max = clm(TR_reduction ~ TR_start_max, data = bounds_m)
m_TR_max_end = clm(TR_reduction ~ TR_max_end, data = bounds_m)
m_TR_start_max_end = clm(TR_reduction ~ TR_start_max_end, data = bounds_m)
m_TR_start_prop = clm(TR_reduction ~ TR_start_prop, data = bounds_m)
m_TR_end_prop = clm(TR_reduction ~ TR_end_prop, data = bounds_m)
m_TR_start_end_prop = clm(TR_reduction ~ TR_start_end_prop, data = bounds_m)

exp((260.3652 - 296.6605)/2)

m_TR_combined = clm(TR_reduction ~ TR_start_max_end + TR_start_end_prop, data = bounds_m)
AIC(m_TR_combined)

m_nn_start = clm(TR_reduction ~ nn_start, data = bounds_m)
m_nn_end = clm(TR_reduction ~ nn_end, data = bounds_m)
m_nn_max = clm(TR_reduction ~ nn_max, data = bounds_m)
m_nn_start_end = clm(TR_reduction ~ nn_start_end, data = bounds_m)
m_nn_start_max = clm(TR_reduction ~ nn_start_max, data = bounds_m)
m_nn_max_end = clm(TR_reduction ~ nn_max_end, data = bounds_m)
m_nn_start_max_end = clm(TR_reduction ~ nn_start_max_end, data = bounds_m)
m_nn_start_prop = clm(TR_reduction ~ nn_start_prop, data = bounds_m)
m_nn_end_prop = clm(TR_reduction ~ nn_end_prop, data = bounds_m)
m_nn_start_end_prop = clm(TR_reduction ~ nn_start_end_prop, data = bounds_m)


par(mfrow = c(2,5))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$TR_start)
text(1, max(bounds_m$TR_start), labels = c(paste("AIC: ", as.character(round(AIC(m_TR_start), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$TR_end)
text(1, max(bounds_m$TR_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TR_end), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$TR_max)
text(1, max(bounds_m$TR_max), labels = c(paste("AIC: ", as.character(round(AIC(m_TR_max), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$TR_start_end)
text(1, max(bounds_m$TR_start_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TR_start_end), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$TR_start_max)
text(1, max(bounds_m$TR_start_max), labels = c(paste("AIC: ", as.character(round(AIC(m_TR_start_max), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$TR_max_end)
text(1, max(bounds_m$TR_max_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TR_max_end), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$TR_start_max_end, ylim=c(0,0.25))
text(1, 0.25, labels = c(paste("AIC: ", as.character(round(AIC(m_TR_start_max_end), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$TR_start_prop)
text(1, max(bounds_m$TR_start_prop), labels = c(paste("AIC: ", as.character(round(AIC(m_TR_start_prop), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$TR_end_prop)
text(1, max(bounds_m$TR_end_prop), labels = c(paste("AIC: ", as.character(round(AIC(m_TR_end_prop), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$TR_start_end_prop)
text(1, max(bounds_m$TR_start_end_prop), labels = c(paste("AIC: ", as.character(round(AIC(m_TR_start_end_prop), digits = 2)), sep = "")))

par(mfrow = c(2,5))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$nn_start)
text(1.2, max(bounds_m$nn_start), labels = c(paste("AIC: ", as.character(round(AIC(m_nn_start), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$nn_end)
text(1.2, max(na.omit(bounds_m$nn_end)), labels = c(paste("AIC: ", as.character(round(AIC(m_nn_end), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$nn_max)
text(1.2, max(na.omit(bounds_m$nn_max)), labels = c(paste("AIC: ", as.character(round(AIC(m_nn_max), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$nn_start_end)
text(1.2, max(na.omit(bounds_m$nn_start_end)), labels = c(paste("AIC: ", as.character(round(AIC(m_nn_start_end), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$nn_start_max)
text(1.2, max(na.omit(bounds_m$nn_start_max)), labels = c(paste("AIC: ", as.character(round(AIC(m_nn_start_max), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$nn_max_end)
text(1.2, max(na.omit(bounds_m$nn_max_end)), labels = c(paste("AIC: ", as.character(round(AIC(m_nn_max_end), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$nn_start_max_end)
text(1.2, max(na.omit(bounds_m$nn_start_max_end)), labels = c(paste("AIC: ", as.character(round(AIC(m_nn_start_max_end), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$nn_start_prop)
text(1.2, max(na.omit(bounds_m$nn_start_prop)), labels = c(paste("AIC: ", as.character(round(AIC(m_nn_start_prop), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$nn_end_prop)
text(1.2, max(na.omit(bounds_m$nn_end_prop)), labels = c(paste("AIC: ", as.character(round(AIC(m_nn_end_prop), digits = 2)), sep = "")))
plot(as.factor(as.character(bounds[bounds$TR_reduction != "D",]$TR_reduction)), bounds[bounds$TR_reduction != "D",]$nn_start_end_prop)
text(1.2, max(na.omit(bounds_m$nn_start_end_prop)), labels = c(paste("AIC: ", as.character(round(AIC(m_nn_start_end_prop), digits = 2)), sep = "")))

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
plot(density(bounds_a$TR_nn_start_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "IFADV")
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
plot(density(bounds_a$TR_nn_start_b, na.rm = TRUE), xlim = c(-0.1, 0.1), ylim = c(0, 100), main = "ECSD")
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

par(mfrow = c(2,4))

