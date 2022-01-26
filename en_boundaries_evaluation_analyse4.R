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

bounds[(bounds$TZ_variant == "@n" | bounds$MW_variant == "@n") & abs(bounds$TZ_MW_N_b) > 0.03,]

# check schwas that MW thinks are nasalized but TZ thinks are not
bounds[bounds$TZ_variant == "@" & bounds$MW_variant == "~", c("wav", "chan", "chunk_start", "chunk_end", "tier", "word_chunk_i", "word_ort", "speaker")]
bounds[bounds$TZ_variant == "~" & bounds$MW_variant == "@", c("wav", "chan", "chunk_start", "chunk_end", "tier", "word_chunk_i", "word_ort", "speaker")]

