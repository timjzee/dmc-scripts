if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/classifier_evaluation/en/"
} else {
  f_path = "/vol/tensusers/timzee/classifier_evaluation/en/"
}

gs_df <- read.csv(paste(f_path, "gs_summary3.csv", sep = ""))

gs_df$v_classifier_average_score <- (gs_df$v_perc20_MW_NN + gs_df$v_variant_accuracy_MW_NN + gs_df$v_variant_macro_Fscore_MW_NN) / 3
# gs_df$v_classifier_average_score <- (gs_df$v_perc20_MW_NN + gs_df$v_variant_accuracy_MW_NN) / 2
gs_df$v_classifier_average_score_TZ <- (gs_df$v_perc20_TZ_NN + gs_df$v_variant_accuracy_TZ_NN + gs_df$v_variant_macro_Fscore_TZ_NN) / 3
# gs_df$v_classifier_average_score_TZ <- (gs_df$v_perc20_TZ_NN + gs_df$v_variant_accuracy_TZ_NN) / 2

gs_df$t_classifier_average_score <- (gs_df$t_perc20_MW_NN + gs_df$t_variant_accuracy_MW_NN + gs_df$t_variant_macro_Fscore_MW_NN) / 3
# gs_df$t_classifier_average_score <- (gs_df$t_perc20_MW_NN + gs_df$t_variant_accuracy_MW_NN) / 2
gs_df$t_classifier_average_score_TZ <- (gs_df$t_perc20_TZ_NN + gs_df$t_variant_accuracy_TZ_NN + gs_df$t_variant_macro_Fscore_TZ_NN) / 3
# gs_df$t_classifier_average_score_TZ <- (gs_df$t_perc20_TZ_NN + gs_df$t_variant_accuracy_TZ_NN) / 2

# compared to MW
par(mfrow = c(1,1), mar=c(12,6,1,2), oma=c(0,0,2,0))

gs_df = gs_df[order(gs_df$v_perc20_MW_NN, decreasing = T),]

best_ind <- as.integer(seq(from=1, to=nrow(gs_df)/2, length.out = 100))
plot(best_ind - 0.25*((nrow(gs_df)/2) %/% 100), gs_df$v_perc20_MW_NN[best_ind], xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df$t_perc20_MW_NN[best_ind])-0.002, 
              max(gs_df$v_perc20_TZ_MW[1],gs_df$v_perc20_MW_NN[1],gs_df$t_perc20_TZ_MW[1])+0.002))
points(best_ind + 0.25*((nrow(gs_df)/2) %/% 100), gs_df$t_perc20_MW_NN[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = gs_df$v_perc20_TZ_MW[1], lty="dashed", col="black")
abline(h = gs_df$t_perc20_TZ_MW[1], lty="dashed", col="red")

# best 100
best_ind <- 1:100
plot(best_ind-0.25, gs_df$v_perc20_MW_NN[best_ind], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df$t_perc20_MW_NN[best_ind])-0.002, max(gs_df$v_perc20_TZ_MW[1],gs_df$v_perc20_MW_NN[1])+0.002))
points(best_ind + 0.25, gs_df$t_perc20_MW_NN[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = gs_df$v_perc20_TZ_MW[1], lty="dashed", col="black")
abline(h = gs_df$t_perc20_TZ_MW[1], lty="dashed", col="red")

# TZ
gs_df = gs_df[order(gs_df$v_perc20_TZ_NN, decreasing = T),]

best_ind <- as.integer(seq(from=1, to=nrow(gs_df)/2, length.out = 100))
plot(best_ind - 0.25*((nrow(gs_df)/2) %/% 100), gs_df$v_perc20_TZ_NN[best_ind], xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(min(gs_df$t_perc20_TZ_NN[best_ind]), min(gs_df[order(gs_df$v_perc20_MW_NN, decreasing = T),]$t_perc20_MW_NN[best_ind]))-0.002, 
              max(gs_df$v_perc20_TZ_MW[1],gs_df$v_perc20_TZ_NN[1],gs_df$t_perc20_TZ_MW[1])+0.002))
points(best_ind + 0.25*((nrow(gs_df)/2) %/% 100), gs_df$t_perc20_TZ_NN[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = gs_df$v_perc20_TZ_MW[1], lty="dashed", col="black")
abline(h = gs_df$t_perc20_TZ_MW[1], lty="dashed", col="red")

# best 100
best_ind <- 1:100
plot(best_ind-0.25, gs_df$v_perc20_TZ_NN[best_ind], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df$t_perc20_TZ_NN[best_ind])-0.002, max(gs_df$v_perc20_TZ_MW[1],gs_df$v_perc20_TZ_NN[1])+0.002))
points(best_ind + 0.25, gs_df$t_perc20_TZ_NN[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = gs_df$v_perc20_TZ_MW[1], lty="dashed", col="black")
abline(h = gs_df$t_perc20_TZ_MW[1], lty="dashed", col="red")

# accuracy
# MW
gs_df = gs_df[order(gs_df$v_variant_accuracy_MW_NN, decreasing = T),]

best_ind <- as.integer(seq(from=1, to=nrow(gs_df)/2, length.out = 100))
plot(best_ind - 0.25*((nrow(gs_df)/2) %/% 100), gs_df$v_variant_accuracy_MW_NN[best_ind], xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df$t_variant_accuracy_MW_NN[best_ind],
                  min(gs_df[order(gs_df$v_variant_accuracy_TZ_NN, decreasing = T),]$t_variant_accuracy_TZ_NN[best_ind]))-0.002, 
              max(gs_df$v_variant_accuracy_TZ_MW[1],
                  gs_df$v_variant_accuracy_MW_NN[1],
                  gs_df$t_variant_accuracy_MW_NN[1],
                  gs_df$t_variant_accuracy_TZ_MW[1],
                  gs_df[order(gs_df$v_variant_accuracy_TZ_NN, decreasing = T),]$v_variant_accuracy_TZ_NN[best_ind[1]])+0.002))
points(best_ind + 0.25*((nrow(gs_df)/2) %/% 100), gs_df$t_variant_accuracy_MW_NN[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = gs_df$v_variant_accuracy_TZ_MW[1], lty="dashed", col="black")
abline(h = gs_df$t_variant_accuracy_TZ_MW[1], lty="dashed", col="red")
# TZ
gs_df = gs_df[order(gs_df$v_variant_accuracy_TZ_NN, decreasing = T),]

best_ind <- as.integer(seq(from=1, to=nrow(gs_df)/2, length.out = 100))
plot(best_ind - 0.25*((nrow(gs_df)/2) %/% 100), gs_df$v_variant_accuracy_TZ_NN[best_ind], xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df$t_variant_accuracy_TZ_NN[best_ind])-0.002, 
              max(gs_df$v_variant_accuracy_TZ_MW[1],gs_df$v_variant_accuracy_TZ_NN[1],gs_df$t_variant_accuracy_TZ_NN[1],gs_df$t_variant_accuracy_TZ_MW[1])+0.002))
points(best_ind + 0.25*((nrow(gs_df)/2) %/% 100), gs_df$t_variant_accuracy_TZ_NN[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = gs_df$v_variant_accuracy_TZ_MW[1], lty="dashed", col="black")
abline(h = gs_df$t_variant_accuracy_TZ_MW[1], lty="dashed", col="red")


# combined score
v_human_average_score <- (gs_df$v_perc20_TZ_MW[1] + gs_df$v_variant_accuracy_TZ_MW + gs_df$v_variant_macro_Fscore_TZ_MW) / 3
# v_human_average_score <- (gs_df$v_perc20_TZ_MW[1] + gs_df$v_variant_accuracy_TZ_MW) / 2
t_human_average_score <- (gs_df$t_perc20_TZ_MW[1] + gs_df$t_variant_accuracy_TZ_MW + gs_df$t_variant_macro_Fscore_TZ_MW) / 3
# t_human_average_score <- (gs_df$t_perc20_TZ_MW[1] + gs_df$t_variant_accuracy_TZ_MW) / 2

best_ind <- as.integer(seq(from=1, to=nrow(gs_df), length.out = 100))
plot(best_ind - 0.25*(nrow(gs_df) %/% 100), gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_classifier_average_score[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_classifier_average_score[best_ind])-0.002, max(c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_classifier_average_score[best_ind], v_human_average_score[1], t_human_average_score[1]))+0.002),
     main = "Matching variants (Average Score)")
points(best_ind + 0.25*(nrow(gs_df) %/% 100), gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_classifier_average_score[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = v_human_average_score, lty="dashed", col="black")
abline(h = t_human_average_score, lty="dashed", col="red")

best_ind <- 1:100
plot(best_ind - 0.25, gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_classifier_average_score[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_classifier_average_score[best_ind])-0.002, max(c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_classifier_average_score[best_ind], v_human_average_score[1], t_human_average_score[1]))+0.002),
     main = "Matching variants (Average Score)")
points(best_ind + 0.25, gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_classifier_average_score[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

# combined score TZ
best_ind <- as.integer(seq(from=1, to=nrow(gs_df), length.out = 100))
plot(best_ind - 0.25*(nrow(gs_df) %/% 100), gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$v_classifier_average_score_TZ[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(c(gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$t_classifier_average_score_TZ[best_ind], 
                    gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$v_classifier_average_score_TZ[best_ind]))-0.002, 
              max(c(gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$v_classifier_average_score_TZ[best_ind], v_human_average_score[1], t_human_average_score[1]))+0.002),
     main = "Matching variants (Average score_TZ)")
points(best_ind + 0.25*(nrow(gs_df) %/% 100), gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$t_classifier_average_score_TZ[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = v_human_average_score, lty="dashed", col="black")
abline(h = t_human_average_score, lty="dashed", col="red")

best_ind <- 1:100
plot(best_ind - 0.25, gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$v_classifier_average_score_TZ[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$t_classifier_average_score_TZ[best_ind])-0.002, max(c(gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$v_classifier_average_score_TZ[best_ind], v_human_average_score[1], t_human_average_score[1]))+0.002),
     main = "Matching variants (Average score_TZ)")
points(best_ind + 0.25, gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$t_classifier_average_score_TZ[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)




# vali set
best_MW_NN_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_schwa_perc20_MW_NN[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_nasal_perc20_MW_NN[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_nasalization_perc20_MW_NN[1])
TZ_MW_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_schwa_perc20_TZ_MW[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_nasal_perc20_TZ_MW[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_nasalization_perc20_TZ_MW[1])
prec_labels <- c("schwa", "nasal", "nasalization")

par(mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0), mfrow=c(1,2))

plot(NULL, xlim = c(0.5,3.5), ylim = c(0,1), xaxt="n", ylab="Percentage", xlab = "Classifier", main = "Validation set")
for (i in 1:3){
  lines(x = c(i - 0.1, i - 0.1), y = c(0, TZ_MW_perc[i]), col="black", lwd=4)
  lines(x = c(i + 0.1, i + 0.1), y = c(0, best_MW_NN_perc[i]), col="cadetblue", lwd=4)
}
lines(x = c(2.8, 3), y = c(1,1), col="black", lwd=4)
text(x = 3.3, y = 1, "Human")
lines(x = c(2.8, 3), y = c(.9,.9), col="cadetblue", lwd=4)
text(x = 3.3, y = .9, "NN")
axis(side=1, col="black", at=1:3, labels=prec_labels)

# test set
best_MW_NN_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_schwa_perc20_MW_NN[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_nasal_perc20_MW_NN[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_nasalization_perc20_MW_NN[1])
TZ_MW_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_schwa_perc20_TZ_MW[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_nasal_perc20_TZ_MW[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_nasalization_perc20_TZ_MW[1])
prec_labels <- c("schwa", "nasal", "nasalization")


plot(NULL, xlim = c(0.5,3.5), ylim = c(0,1), xaxt="n", ylab="Percentage", xlab = "Classifier", main = "Test set")
for (i in 1:3){
  lines(x = c(i - 0.1, i - 0.1), y = c(0, TZ_MW_perc[i]), col="black", lwd=4)
  lines(x = c(i + 0.1, i + 0.1), y = c(0, best_MW_NN_perc[i]), col="cadetblue", lwd=4)
}
lines(x = c(2.8, 3), y = c(1,1), col="black", lwd=4)
text(x = 3.3, y = 1, "Human")
lines(x = c(2.8, 3), y = c(.9,.9), col="cadetblue", lwd=4)
text(x = 3.3, y = .9, "NN")
axis(side=1, col="black", at=1:3, labels=prec_labels)

# vali set
best_MW_NN_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_variant_macro_Fscore_MW_NN[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_variant_accuracy_MW_NN[1])
TZ_MW_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_variant_macro_Fscore_TZ_MW[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_variant_accuracy_TZ_MW[1])
prec_labels <- c("Macro F-Score", "Accuracy")

plot(NULL, xlim = c(0.5,3.5), ylim = c(0,1), xaxt="n", ylab="Percentage", xlab = "Classifier", main = "Validation set")
for (i in 1:2){
  lines(x = c(i - 0.1, i - 0.1), y = c(0, TZ_MW_perc[i]), col="black", lwd=4)
  lines(x = c(i + 0.1, i + 0.1), y = c(0, best_MW_NN_perc[i]), col="cadetblue", lwd=4)
}
lines(x = c(2.8, 3), y = c(1,1), col="black", lwd=4)
text(x = 3.3, y = 1, "Human")
lines(x = c(2.8, 3), y = c(.9,.9), col="cadetblue", lwd=4)
text(x = 3.3, y = .9, "NN")
axis(side=1, col="black", at=1:2, labels=prec_labels)

# test set
best_MW_NN_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_variant_macro_Fscore_MW_NN[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_variant_accuracy_MW_NN[1])
TZ_MW_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_variant_macro_Fscore_TZ_MW[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_variant_accuracy_TZ_MW[1])
prec_labels <- c("Macro F-Score", "Accuracy")

plot(NULL, xlim = c(0.5,3.5), ylim = c(0,1), xaxt="n", ylab="Percentage", xlab = "Classifier", main = "Test set")
for (i in 1:2){
  lines(x = c(i - 0.1, i - 0.1), y = c(0, TZ_MW_perc[i]), col="black", lwd=4)
  lines(x = c(i + 0.1, i + 0.1), y = c(0, best_MW_NN_perc[i]), col="cadetblue", lwd=4)
}
lines(x = c(2.8, 3), y = c(1,1), col="black", lwd=4)
text(x = 3.3, y = 1, "Human")
lines(x = c(2.8, 3), y = c(.9,.9), col="cadetblue", lwd=4)
text(x = 3.3, y = .9, "NN")
axis(side=1, col="black", at=1:2, labels=prec_labels)

par(mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0), mfrow=c(1,1))

# inspect confusion matrices
bounds <- read.table(paste(f_path, "gs_summary3_example.csv", sep = ""), header = T, quote = "", sep = "\t")

bounds$TZ_variant <- factor(x = bounds$TZ_variant, levels = c("@", "@n", "~", "0", "n"))
bounds$MW_variant <- factor(x = bounds$MW_variant, levels = c("@", "@n", "~", "0", "n"))


vali_indices <- 1:71
test_indices <- 72:142

v_cm <- table(bounds[vali_indices,]$TZ_variant, bounds[vali_indices,]$MW_variant, dnn = c("TZ", "MW"))
v_recall <- diag(v_cm) / colSums(v_cm)
v_macro.recall <- mean(na.omit(v_recall))
v_precision <- diag(v_cm) / rowSums(v_cm)
v_macro.precision <- mean(na.omit(v_precision))
v_macro.Fscore <- 2*((v_macro.precision * v_macro.recall) / (v_macro.precision + v_macro.recall))
v_Accuracy <- sum(diag(v_cm)) / sum(v_cm)

t_cm <- table(bounds[test_indices,]$TZ_variant, bounds[test_indices,]$MW_variant, dnn = c("TZ", "MW"))
t_recall <- diag(t_cm) / colSums(t_cm)
t_macro.recall <- mean(na.omit(t_recall))
t_precision <- diag(t_cm) / rowSums(t_cm)
t_macro.precision <- mean(na.omit(t_precision))
t_macro.Fscore <- 2*((t_macro.precision * t_macro.recall) / (t_macro.precision + t_macro.recall))
t_Accuracy <- sum(diag(t_cm)) / sum(t_cm)

# check MW TZ agreement for starting boundaries of N in @n variants
nasalization_perc20_TZ_MW_b <- NROW(na.omit(bounds[(bounds$TZ_variant == "@n" | bounds$MW_variant == "@n") & abs(bounds$TZ_MW_N_b) < 0.02,c("TZ_MW_N_b")])) / NROW(bounds[bounds$TZ_variant == "@n" | bounds$MW_variant == "@n", c("TZ_MW_N_b")])
nasalization_perc20_TZ_MW_e <- NROW(na.omit(bounds[(bounds$TZ_variant == "@n" | bounds$MW_variant == "@n") & abs(bounds$TZ_MW_N_e) < 0.02,c("TZ_MW_N_e")])) / NROW(bounds[bounds$TZ_variant == "@n" | bounds$MW_variant == "@n", c("TZ_MW_N_e")])

bounds[(bounds$TZ_variant == "@n" | bounds$MW_variant == "@n") & abs(bounds$TZ_MW_N_b) >= 0.02,]

# check schwas that MW thinks are nasalized but TZ thinks are not
bounds[bounds$TZ_variant == "@" & bounds$MW_variant == "~", c("wav", "chan", "chunk_start", "chunk_end", "tier", "word_chunk_i", "word_ort", "speaker")]
bounds[bounds$TZ_variant == "~" & bounds$MW_variant == "@", c("wav", "chan", "chunk_start", "chunk_end", "tier", "word_chunk_i", "word_ort", "speaker")]


# visualize <20 for each boundary


# get nn data for a selected combination

bounds_all <- read.csv(paste(f_path, "grid_search_output3/", gs_df$gs_name[1], ".csv", sep = ""))
# change this to the configuration that we selected
bounds_all <- read.csv(paste(f_path, "grid_search_output3/", "e-BLSTM-15_n-BLSTM-15_N-BLSTM-5_1_1_0_0_2_0_0.2_0.5_0.5_1", ".csv", sep = ""))


getGsData <- function(indices){
  bounds <- bounds_all[indices,]
  ### schwa
  ## Human-Human
  bounds$TZ_MW_e_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_start_b, bounds$MW_e_start_b)
  schwa_perc20_TZ_MW_startb <- NROW(na.omit(bounds[abs(bounds$TZ_MW_e_start_b) < 0.02,c("TZ_MW_e_start_b")])) / NROW(bounds[,c("TZ_MW_e_start_b")])
  bounds$TZ_MW_e_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_start_e, bounds$MW_e_start_e)
  schwa_perc20_TZ_MW_starte <- NROW(na.omit(bounds[abs(bounds$TZ_MW_e_start_e) < 0.02,c("TZ_MW_e_start_e")])) / NROW(bounds[,c("TZ_MW_e_start_e")])
  bounds$TZ_MW_e_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_end_b, bounds$MW_e_end_b)
  schwa_perc20_TZ_MW_endb <- NROW(na.omit(bounds[abs(bounds$TZ_MW_e_end_b) < 0.02,c("TZ_MW_e_end_b")])) / NROW(bounds[,c("TZ_MW_e_end_b")])
  bounds$TZ_MW_e_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_end_e, bounds$MW_e_end_e)
  schwa_perc20_TZ_MW_ende <- NROW(na.omit(bounds[abs(bounds$TZ_MW_e_end_e) < 0.02,c("TZ_MW_e_end_e")])) / NROW(bounds[,c("TZ_MW_e_end_e")])
  bounds_TZ_MW_e_long <- c(bounds$TZ_MW_e_start_b, bounds$TZ_MW_e_start_e, bounds$TZ_MW_e_end_b, bounds$TZ_MW_e_end_e)
  schwa_perc20_TZ_MW <- NROW(na.omit(bounds_TZ_MW_e_long[abs(bounds_TZ_MW_e_long) < 0.02])) / NROW(bounds_TZ_MW_e_long)
  ## Human-Computer
  # check for incomplete and early schwas
  bounds$contains_schwa <- !is.na(bounds$nn_e_start_b + bounds$nn_e_start_e + bounds$nn_e_end_b + bounds$nn_e_end_e)
  bounds$frag_dur <- bounds$kal_end - bounds$kal_start + .5
  bounds$end_first_quart <- bounds$kal_start - .3 + .33*bounds$frag_dur
  bounds$early_schwa <- bounds$nn_e_start_b < bounds$end_first_quart
  if (length(bounds[bounds$early_schwa & !is.na(bounds$early_schwa),]$contains_schwa) > 0){
    bounds[bounds$early_schwa & !is.na(bounds$early_schwa),]$contains_schwa <- FALSE
  }
  # remove boundaries of incomplete schwas and early schwas
  if(sum(!bounds$contains_schwa) > 0){
    bounds[!bounds$contains_schwa,]$nn_e_start_b <- NA
    bounds[!bounds$contains_schwa,]$nn_e_start_e <- NA
    bounds[!bounds$contains_schwa,]$nn_e_end_b <- NA
    bounds[!bounds$contains_schwa,]$nn_e_end_e <- NA
  }
  # compute differences and percentages < 20ms
  bounds$TZ_nn_e_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_start_b, bounds$nn_e_start_b)
  schwa_perc20_TZ_NN_startb <- NROW(na.omit(bounds[abs(bounds$TZ_nn_e_start_b) < 0.02,c("TZ_nn_e_start_b")])) / NROW(bounds[,c("TZ_nn_e_start_b")])
  bounds$TZ_nn_e_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_start_e, bounds$nn_e_start_e)
  schwa_perc20_TZ_NN_starte <- NROW(na.omit(bounds[abs(bounds$TZ_nn_e_start_e) < 0.02,c("TZ_nn_e_start_e")])) / NROW(bounds[,c("TZ_nn_e_start_e")])
  bounds$TZ_nn_e_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_end_b, bounds$nn_e_end_b)
  schwa_perc20_TZ_NN_endb <- NROW(na.omit(bounds[abs(bounds$TZ_nn_e_end_b) < 0.02,c("TZ_nn_e_end_b")])) / NROW(bounds[,c("TZ_nn_e_end_b")])
  bounds$TZ_nn_e_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_end_e, bounds$nn_e_end_e)
  schwa_perc20_TZ_NN_ende <- NROW(na.omit(bounds[abs(bounds$TZ_nn_e_end_e) < 0.02,c("TZ_nn_e_end_e")])) / NROW(bounds[,c("TZ_nn_e_end_e")])
  bounds_TZ_NN_e_long <- c(bounds$TZ_nn_e_start_b, bounds$TZ_nn_e_start_e, bounds$TZ_nn_e_end_b, bounds$TZ_nn_e_end_e)
  schwa_perc20_TZ_NN <- NROW(na.omit(bounds_TZ_NN_e_long[abs(bounds_TZ_NN_e_long) < 0.02])) / NROW(bounds_TZ_NN_e_long)
  #
  bounds$MW_nn_e_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_e_start_b, bounds$nn_e_start_b)
  schwa_perc20_MW_NN_startb <- NROW(na.omit(bounds[abs(bounds$MW_nn_e_start_b) < 0.02,c("MW_nn_e_start_b")])) / NROW(bounds[,c("MW_nn_e_start_b")])
  bounds$MW_nn_e_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_e_start_e, bounds$nn_e_start_e)
  schwa_perc20_MW_NN_starte <- NROW(na.omit(bounds[abs(bounds$MW_nn_e_start_e) < 0.02,c("MW_nn_e_start_e")])) / NROW(bounds[,c("MW_nn_e_start_e")])
  bounds$MW_nn_e_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_e_end_b, bounds$nn_e_end_b)
  schwa_perc20_MW_NN_endb <- NROW(na.omit(bounds[abs(bounds$MW_nn_e_end_b) < 0.02,c("MW_nn_e_end_b")])) / NROW(bounds[,c("MW_nn_e_end_b")])
  bounds$MW_nn_e_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_e_end_e, bounds$nn_e_end_e)
  schwa_perc20_MW_NN_ende <- NROW(na.omit(bounds[abs(bounds$MW_nn_e_end_e) < 0.02,c("MW_nn_e_end_e")])) / NROW(bounds[,c("MW_nn_e_end_e")])
  bounds_MW_NN_e_long <- c(bounds$MW_nn_e_start_b, bounds$MW_nn_e_start_e, bounds$MW_nn_e_end_b, bounds$MW_nn_e_end_e)
  schwa_perc20_MW_NN <- NROW(na.omit(bounds_MW_NN_e_long[abs(bounds_MW_NN_e_long) < 0.02])) / NROW(bounds_MW_NN_e_long)
  ### nasal
  ## Human-Human
  bounds$TZ_MW_n_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_start_b, bounds$MW_n_start_b)
  nasal_perc20_TZ_MW_startb <- NROW(na.omit(bounds[abs(bounds$TZ_MW_n_start_b) < 0.02,c("TZ_MW_n_start_b")])) / NROW(bounds[,c("TZ_MW_n_start_b")])
  bounds$TZ_MW_n_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_start_e, bounds$MW_n_start_e)
  nasal_perc20_TZ_MW_starte <- NROW(na.omit(bounds[abs(bounds$TZ_MW_n_start_e) < 0.02,c("TZ_MW_n_start_e")])) / NROW(bounds[,c("TZ_MW_n_start_e")])
  bounds$TZ_MW_n_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_end_b, bounds$MW_n_end_b)
  nasal_perc20_TZ_MW_endb <- NROW(na.omit(bounds[abs(bounds$TZ_MW_n_end_b) < 0.02,c("TZ_MW_n_end_b")])) / NROW(bounds[,c("TZ_MW_n_end_b")])
  bounds$TZ_MW_n_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_end_e, bounds$MW_n_end_e)
  nasal_perc20_TZ_MW_ende <- NROW(na.omit(bounds[abs(bounds$TZ_MW_n_end_e) < 0.02,c("TZ_MW_n_end_e")])) / NROW(bounds[,c("TZ_MW_n_end_e")])
  bounds_TZ_MW_n_long <- c(bounds$TZ_MW_n_start_b, bounds$TZ_MW_n_start_e, bounds$TZ_MW_n_end_b, bounds$TZ_MW_n_end_e)
  nasal_perc20_TZ_MW <- NROW(na.omit(bounds_TZ_MW_n_long[abs(bounds_TZ_MW_n_long) < 0.02])) / NROW(bounds_TZ_MW_n_long)
  ## Human-Computer
  # check for incomplete and early nasals
  bounds$contains_nasal <- !is.na(bounds$nn_n_start_b + bounds$nn_n_start_e + bounds$nn_n_end_b + bounds$nn_n_end_e)
  bounds$early_nasal <- bounds$nn_n_start_b < bounds$end_first_quart
  if (length(bounds[bounds$early_nasal & !is.na(bounds$early_nasal),]$contains_nasal) > 0){
    bounds[bounds$early_nasal & !is.na(bounds$early_nasal),]$contains_nasal <- FALSE
  }
  # remove boundaries of incomplete nasals and early nasals
  if(sum(!bounds$contains_nasal) > 0){
    bounds[!bounds$contains_nasal,]$nn_n_start_b <- NA
    bounds[!bounds$contains_nasal,]$nn_n_start_e <- NA
    bounds[!bounds$contains_nasal,]$nn_n_end_b <- NA
    bounds[!bounds$contains_nasal,]$nn_n_end_e <- NA
  }
  # compute differences and percentages < 20ms
  bounds$TZ_nn_n_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_start_b, bounds$nn_n_start_b)
  nasal_perc20_TZ_NN_startb <- NROW(na.omit(bounds[abs(bounds$TZ_nn_n_start_b) < 0.02,c("TZ_nn_n_start_b")])) / NROW(bounds[,c("TZ_nn_n_start_b")])
  bounds$TZ_nn_n_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_start_e, bounds$nn_n_start_e)
  nasal_perc20_TZ_NN_starte <- NROW(na.omit(bounds[abs(bounds$TZ_nn_n_start_e) < 0.02,c("TZ_nn_n_start_e")])) / NROW(bounds[,c("TZ_nn_n_start_e")])
  bounds$TZ_nn_n_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_end_b, bounds$nn_n_end_b)
  nasal_perc20_TZ_NN_endb <- NROW(na.omit(bounds[abs(bounds$TZ_nn_n_end_b) < 0.02,c("TZ_nn_n_end_b")])) / NROW(bounds[,c("TZ_nn_n_end_b")])
  bounds$TZ_nn_n_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_end_e, bounds$nn_n_end_e)
  nasal_perc20_TZ_NN_ende <- NROW(na.omit(bounds[abs(bounds$TZ_nn_n_end_e) < 0.02,c("TZ_nn_n_end_e")])) / NROW(bounds[,c("TZ_nn_n_end_e")])
  bounds_TZ_NN_n_long <- c(bounds$TZ_nn_n_start_b, bounds$TZ_nn_n_start_e, bounds$TZ_nn_n_end_b, bounds$TZ_nn_n_end_e)
  nasal_perc20_TZ_NN <- NROW(na.omit(bounds_TZ_NN_n_long[abs(bounds_TZ_NN_n_long) < 0.02])) / NROW(bounds_TZ_NN_n_long)
  #
  bounds$MW_nn_n_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_n_start_b, bounds$nn_n_start_b)
  nasal_perc20_MW_NN_startb <- NROW(na.omit(bounds[abs(bounds$MW_nn_n_start_b) < 0.02,c("MW_nn_n_start_b")])) / NROW(bounds[,c("MW_nn_n_start_b")])
  bounds$MW_nn_n_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_n_start_e, bounds$nn_n_start_e)
  nasal_perc20_MW_NN_starte <- NROW(na.omit(bounds[abs(bounds$MW_nn_n_start_e) < 0.02,c("MW_nn_n_start_e")])) / NROW(bounds[,c("MW_nn_n_start_e")])
  bounds$MW_nn_n_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_n_end_b, bounds$nn_n_end_b)
  nasal_perc20_MW_NN_endb <- NROW(na.omit(bounds[abs(bounds$MW_nn_n_end_b) < 0.02,c("MW_nn_n_end_b")])) / NROW(bounds[,c("MW_nn_n_end_b")])
  bounds$MW_nn_n_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_n_end_e, bounds$nn_n_end_e)
  nasal_perc20_MW_NN_ende <- NROW(na.omit(bounds[abs(bounds$MW_nn_n_end_e) < 0.02,c("MW_nn_n_end_e")])) / NROW(bounds[,c("MW_nn_n_end_e")])
  bounds_MW_NN_n_long <- c(bounds$MW_nn_n_start_b, bounds$MW_nn_n_start_e, bounds$MW_nn_n_end_b, bounds$MW_nn_n_end_e)
  nasal_perc20_MW_NN <- NROW(na.omit(bounds_MW_NN_n_long[abs(bounds_MW_NN_n_long) < 0.02])) / NROW(bounds_MW_NN_n_long)
  ### nasalization
  ## only for humans, classifier is handled in praat script (or schwa end if no schwa present)
  bounds$TZ_contains_schwa <- !is.na(bounds$TZ_e_start_b + bounds$TZ_e_start_e + bounds$TZ_e_end_b + bounds$TZ_e_end_e)
  bounds$TZ_contains_nasal <- !is.na(bounds$TZ_n_start_b + bounds$TZ_n_start_e + bounds$TZ_n_end_b + bounds$TZ_n_end_e)
  bounds$TZ_contains_nasalization <- !is.na(bounds$TZ_N_b + bounds$TZ_N_e)
  bounds$MW_contains_schwa <- !is.na(bounds$MW_e_start_b + bounds$MW_e_start_e + bounds$MW_e_end_b + bounds$MW_e_end_e)
  bounds$MW_contains_nasal <- !is.na(bounds$MW_n_start_b + bounds$MW_n_start_e + bounds$MW_n_end_b + bounds$MW_n_end_e)
  bounds$MW_contains_nasalization <- !is.na(bounds$MW_N_b + bounds$MW_N_e)
  ## Change N start boundary to schwa start if N start < schwa start (or nasal start if no schwa present)
  bounds$TZ_N_b <- ifelse(bounds$TZ_contains_schwa, 
                          ifelse(bounds$TZ_N_b < bounds$TZ_e_start_b,
                                 bounds$TZ_e_start_b, 
                                 bounds$TZ_N_b
                          ),
                          ifelse(bounds$TZ_contains_nasal,
                                 ifelse(bounds$TZ_N_b < bounds$TZ_n_start_b, 
                                        bounds$TZ_n_start_b, 
                                        bounds$TZ_N_b
                                 ),
                                 NA
                          ))
  bounds$MW_N_b <- ifelse(bounds$MW_contains_schwa, 
                          ifelse(bounds$MW_N_b < bounds$MW_e_start_b,
                                 bounds$MW_e_start_b, 
                                 bounds$MW_N_b
                          ),
                          ifelse(bounds$MW_contains_nasal,
                                 ifelse(bounds$MW_N_b < bounds$MW_n_start_b, 
                                        bounds$MW_n_start_b, 
                                        bounds$MW_N_b
                                 ),
                                 NA
                          ))
  ## Change N end boundary to nasal end if N end > nasal end (or schwa end if no nasal present)
  bounds$TZ_N_e <- ifelse(bounds$TZ_contains_nasal, 
                          ifelse(bounds$TZ_N_e > bounds$TZ_n_end_e,
                                 bounds$TZ_n_end_e, 
                                 bounds$TZ_N_e
                          ),
                          ifelse(bounds$TZ_contains_schwa,
                                 ifelse(bounds$TZ_N_e > bounds$TZ_e_end_e, 
                                        bounds$TZ_e_end_e, 
                                        bounds$TZ_N_e
                                 ),
                                 NA
                          ))
  bounds$MW_N_e <- ifelse(bounds$MW_contains_nasal, 
                          ifelse(bounds$MW_N_e > bounds$MW_n_end_e,
                                 bounds$MW_n_end_e, 
                                 bounds$MW_N_e
                          ),
                          ifelse(bounds$MW_contains_schwa,
                                 ifelse(bounds$MW_N_e > bounds$MW_e_end_e, 
                                        bounds$MW_e_end_e, 
                                        bounds$MW_N_e
                                 ),
                                 NA
                          ))
  ## Human-Human
  bounds$TZ_MW_N_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_N_b, bounds$MW_N_b)
  nasalization_perc20_TZ_MW_b <- NROW(na.omit(bounds[abs(bounds$TZ_MW_N_b) < 0.02,c("TZ_MW_N_b")])) / NROW(bounds[,c("TZ_MW_N_b")])
  bounds$TZ_MW_N_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_N_e, bounds$MW_N_e)
  nasalization_perc20_TZ_MW_e <- NROW(na.omit(bounds[abs(bounds$TZ_MW_N_e) < 0.02,c("TZ_MW_N_e")])) / NROW(bounds[,c("TZ_MW_N_e")])
  bounds_TZ_MW_N_long <- c(bounds$TZ_MW_N_b, bounds$TZ_MW_N_e)
  nasalization_perc20_TZ_MW <- NROW(na.omit(bounds_TZ_MW_N_long[abs(bounds_TZ_MW_N_long) < 0.02])) / NROW(bounds_TZ_MW_N_long)
  ## Human-Computer
  # perhaps change N boundaries to n boundaries if the end n boundary is later or the start n boundary is earlier
  # but see notes 7oct.txt
  # for now just remove incomplete nasalizations
  bounds$contains_nasalization <- !is.na(bounds$nn_N_start_b + bounds$nn_N_start_e + bounds$nn_N_end_b + bounds$nn_N_end_e)
  # remove boundaries of incomplete nasalizations
  if(sum(!bounds$contains_nasalization) > 0){
    bounds[!bounds$contains_nasalization,]$nn_N_start_b <- NA
    bounds[!bounds$contains_nasalization,]$nn_N_start_e <- NA
    bounds[!bounds$contains_nasalization,]$nn_N_end_b <- NA
    bounds[!bounds$contains_nasalization,]$nn_N_end_e <- NA
  }
  # compute differences and percentages < 20ms
  bounds$TZ_nn_N_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_N_b, bounds$nn_N_start_b)
  nasalization_perc20_TZ_NN_b <- NROW(na.omit(bounds[abs(bounds$TZ_nn_N_b) < 0.02,c("TZ_nn_N_b")])) / NROW(bounds[,c("TZ_nn_N_b")])
  bounds$TZ_nn_N_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_N_e, bounds$nn_N_end_e)
  nasalization_perc20_TZ_NN_e <- NROW(na.omit(bounds[abs(bounds$TZ_nn_N_e) < 0.02,c("TZ_nn_N_e")])) / NROW(bounds[,c("TZ_nn_N_e")])
  bounds_TZ_NN_N_long <- c(bounds$TZ_nn_N_b, bounds$TZ_nn_N_e)
  nasalization_perc20_TZ_NN <- NROW(na.omit(bounds_TZ_NN_N_long[abs(bounds_TZ_NN_N_long) < 0.02])) / NROW(bounds_TZ_NN_N_long)
  #
  bounds$MW_nn_N_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_N_b, bounds$nn_N_start_b)
  nasalization_perc20_MW_NN_b <- NROW(na.omit(bounds[abs(bounds$MW_nn_N_b) < 0.02,c("MW_nn_N_b")])) / NROW(bounds[,c("MW_nn_N_b")])
  bounds$MW_nn_N_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_N_e, bounds$nn_N_end_e)
  nasalization_perc20_MW_NN_e <- NROW(na.omit(bounds[abs(bounds$MW_nn_N_e) < 0.02,c("MW_nn_N_e")])) / NROW(bounds[,c("MW_nn_N_e")])
  bounds_MW_NN_N_long <- c(bounds$MW_nn_N_b, bounds$MW_nn_N_e)
  nasalization_perc20_MW_NN <- NROW(na.omit(bounds_MW_NN_N_long[abs(bounds_MW_NN_N_long) < 0.02])) / NROW(bounds_MW_NN_N_long)
  
  bounds_TZ_MW_long <- c(bounds_TZ_MW_e_long, bounds_TZ_MW_n_long, bounds_TZ_MW_N_long)
  perc20_TZ_MW <- NROW(na.omit(bounds_TZ_MW_long[abs(bounds_TZ_MW_long) < 0.02])) / NROW(bounds_TZ_MW_long)
  bounds_TZ_NN_long <- c(bounds_TZ_NN_e_long, bounds_TZ_NN_n_long, bounds_TZ_NN_N_long)
  perc20_TZ_NN <- NROW(na.omit(bounds_TZ_NN_long[abs(bounds_TZ_NN_long) < 0.02])) / NROW(bounds_TZ_NN_long)
  bounds_MW_NN_long <- c(bounds_MW_NN_e_long, bounds_MW_NN_n_long, bounds_MW_NN_N_long)
  perc20_MW_NN <- NROW(na.omit(bounds_MW_NN_long[abs(bounds_MW_NN_long) < 0.02])) / NROW(bounds_MW_NN_long)
  
  # get F-score
  bounds$classifier_variant <- mapply(function(x,y,z) ifelse(x, ifelse(y, "@n", ifelse(z, "~", "@")), ifelse(y, "n", "0")),
                                      bounds$contains_schwa,
                                      bounds$contains_nasal,
                                      bounds$contains_nasalization)
  bounds$classifier_variant <- factor(x = bounds$classifier_variant, levels = c("@", "@n", "~", "0", "n"))
  bounds$TZ_variant <- mapply(function(x,y,z) ifelse(x, ifelse(y, "@n", ifelse(z, "~", "@")), ifelse(y, "n", "0")),
                              bounds$TZ_contains_schwa,
                              bounds$TZ_contains_nasal,
                              bounds$TZ_contains_nasalization)
  bounds$TZ_variant <- factor(x = bounds$TZ_variant, levels = c("@", "@n", "~", "0", "n"))
  #
  bounds$MW_variant <- mapply(function(x,y,z) ifelse(x, ifelse(y, "@n", ifelse(z, "~", "@")), ifelse(y, "n", "0")),
                              bounds$MW_contains_schwa,
                              bounds$MW_contains_nasal,
                              bounds$MW_contains_nasalization)
  bounds$MW_variant <- factor(x = bounds$MW_variant, levels = c("@", "@n", "~", "0", "n"))
  
  cm <- table(bounds$TZ_variant, bounds$MW_variant, 
              dnn = c("TZ", "MW"))
  
  recall <- diag(cm) / colSums(cm)
  macro.recall <- mean(na.omit(recall))
  precision <- diag(cm) / rowSums(cm)
  macro.precision <- mean(na.omit(precision))
  Accuracy <- sum(diag(cm)) / sum(cm)
  macro.Fscore <- 2*((macro.precision * macro.recall) / (macro.precision + macro.recall))
  
  cm2 <- table(bounds$classifier_variant, bounds$TZ_variant, 
               dnn = c("NN", "TZ"))
  
  recall2 <- diag(cm2) / colSums(cm2)
  macro.recall2 <- mean(na.omit(recall2))
  precision2 <- diag(cm2) / rowSums(cm2)
  macro.precision2 <- mean(na.omit(precision2))
  Accuracy2 <- sum(diag(cm2)) / sum(cm2)
  macro.Fscore2 <- 2*((macro.precision2 * macro.recall2) / (macro.precision2 + macro.recall2))
  
  cm3 <- table(bounds$classifier_variant, bounds$MW_variant, 
               dnn = c("NN", "MW"))
  
  recall3 <- diag(cm3) / colSums(cm3)
  macro.recall3 <- mean(na.omit(recall3))
  precision3 <- diag(cm3) / rowSums(cm3)
  macro.precision3 <- mean(na.omit(precision3))
  Accuracy3 <- sum(diag(cm3)) / sum(cm3)
  macro.Fscore3 <- 2*((macro.precision3 * macro.recall3) / (macro.precision3 + macro.recall3))
  
  print(c(
           schwa_perc20_TZ_MW_startb, 
           schwa_perc20_TZ_NN_startb, 
           schwa_perc20_MW_NN_startb,
           schwa_perc20_TZ_MW_starte, 
           schwa_perc20_TZ_NN_starte,
           schwa_perc20_MW_NN_starte,
           schwa_perc20_TZ_MW_endb, 
           schwa_perc20_TZ_NN_endb,
           schwa_perc20_MW_NN_endb,
           schwa_perc20_TZ_MW_ende, 
           schwa_perc20_TZ_NN_ende,
           schwa_perc20_MW_NN_ende,
           schwa_perc20_TZ_MW, 
           schwa_perc20_TZ_NN,
           schwa_perc20_MW_NN,
           nasal_perc20_TZ_MW_startb, 
           nasal_perc20_TZ_NN_startb, 
           nasal_perc20_MW_NN_startb,
           nasal_perc20_TZ_MW_starte, 
           nasal_perc20_TZ_NN_starte,
           nasal_perc20_MW_NN_starte,
           nasal_perc20_TZ_MW_endb, 
           nasal_perc20_TZ_NN_endb,
           nasal_perc20_MW_NN_endb,
           nasal_perc20_TZ_MW_ende, 
           nasal_perc20_TZ_NN_ende,
           nasal_perc20_MW_NN_ende,
           nasal_perc20_TZ_MW, 
           nasal_perc20_TZ_NN,
           nasal_perc20_MW_NN,
           nasalization_perc20_TZ_MW_b,
           nasalization_perc20_TZ_NN_b,
           nasalization_perc20_MW_NN_b,
           nasalization_perc20_TZ_MW_e,
           nasalization_perc20_TZ_NN_e,
           nasalization_perc20_MW_NN_e,
           nasalization_perc20_TZ_MW,
           nasalization_perc20_TZ_NN,
           nasalization_perc20_MW_NN,
           perc20_TZ_MW,
           perc20_TZ_NN,
           perc20_MW_NN,
           Accuracy,
           macro.Fscore,
           Accuracy2,
           macro.Fscore2,
           Accuracy3,
           macro.Fscore3))
  return(bounds)
}

bounds_val <- getGsData(vali_indices)
bounds_test <- getGsData(test_indices)

bounds_val[bounds_val$classifier_variant == "@n", c("nn_e_end_e", "nn_n_start_b")]
bounds_val[bounds_val$classifier_variant == "@n", c("nn_N_end_e", "nn_n_end_e")]
bounds_val[bounds_val$classifier_variant == "@n", c("nn_N_start_b", "nn_n_start_b")]

table(bounds_test$TZ_variant, bounds_test$kal_type, dnn = c("TZ", "KA"))
table(bounds_test$TZ_variant, bounds_test$classifier_variant, dnn = c("TZ", "NN"))

bla1 <- bounds_val[bounds_val$classifier_variant == "@n",]$nn_n_start_b - bounds_val[bounds_val$classifier_variant == "@n",]$nn_N_start_b
bla2 <- bounds_val[bounds_val$classifier_variant == "@n",]$TZ_n_start_b - bounds_val[bounds_val$classifier_variant == "@n",]$TZ_N_b

cor(bla1, bla2, use = "pairwise.complete.obs")

bla1 <- bounds_test[bounds_test$classifier_variant == "@n",]$nn_n_start_b - bounds_test[bounds_test$classifier_variant == "@n",]$nn_N_start_b
bla2 <- bounds_test[bounds_test$classifier_variant == "@n",]$TZ_n_start_b - bounds_test[bounds_test$classifier_variant == "@n",]$TZ_N_b

cor(bla1, bla2, use = "pairwise.complete.obs")

