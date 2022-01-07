if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/classifier_evaluation/en/"
} else {
  f_path = "/vol/tensusers/timzee/classifier_evaluation/en/"
}

gs_df <- read.csv(paste(f_path, "gs_summary_mw.csv", sep = ""))

gs_df$v_classifier_average_score <- (gs_df$v_perc20_MW_NN + gs_df$v_variant_accuracy_MW + gs_df$v_variant_macro_Fscore_MW) / 3
gs_df$v_classifier_average_score_TZ <- (gs_df$v_perc20_TR_NN + gs_df$v_variant_accuracy_TZ + gs_df$v_variant_macro_Fscore_TZ) / 3

gs_df$t_classifier_average_score <- (gs_df$t_perc20_MW_NN + gs_df$t_variant_accuracy_MW + gs_df$t_variant_macro_Fscore_MW) / 3
gs_df$t_classifier_average_score_TZ <- (gs_df$t_perc20_TR_NN + gs_df$t_variant_accuracy_TZ + gs_df$t_variant_macro_Fscore_TZ) / 3


# compared to MW
par(mfrow = c(1,1), mar=c(12,6,1,2), oma=c(0,0,2,0))

gs_df = gs_df[order(gs_df$v_perc20_MW_NN, decreasing = T),]

best_ind <- as.integer(seq(from=1, to=nrow(gs_df)/2, length.out = 100))
plot(best_ind - 0.25*((nrow(gs_df)/2) %/% 100), gs_df$v_perc20_MW_NN[best_ind], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df$t_perc20_MW_NN[best_ind])-0.002, max(gs_df$v_perc20_TR_MW[1],gs_df$v_perc20_MW_NN[1])+0.002))
points(best_ind + 0.25*((nrow(gs_df)/2) %/% 100), gs_df$t_perc20_MW_NN[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = gs_df$v_perc20_TR_MW[1], lty="dashed", col="black")
abline(h = gs_df$t_perc20_TR_MW[1], lty="dashed", col="red")

# best 100
best_ind <- 1:100
plot(best_ind-0.25, gs_df$v_perc20_MW_NN[best_ind], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df$t_perc20_MW_NN[best_ind])-0.002, max(gs_df$v_perc20_TR_MW[1],gs_df$v_perc20_MW_NN[1])+0.002))
points(best_ind + 0.25, gs_df$t_perc20_MW_NN[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = gs_df$v_perc20_TR_MW[1], lty="dashed", col="black")
abline(h = gs_df$t_perc20_TR_MW[1], lty="dashed", col="red")


# combined score
best <- read.csv(paste(f_path, "grid_search_output/", gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$gs_name[1], ".csv", sep = ""))

best$contains_schwa <- !is.na(best$nn_e_start_b + best$nn_e_start_e +
                                best$nn_e_end_b + best$nn_e_end_e)
best$TR_contains_schwa <- !is.na(best$TR_e_start_b + best$TR_e_start_e +
                                   best$TR_e_end_b + best$TR_e_end_e)
best$MW_contains_schwa <- !is.na(best$MW_e_start_b + best$MW_e_start_e +
                                   best$MW_e_end_b + best$MW_e_end_e)

best$contains_nasal <- !is.na(best$nn_n_start_b + best$nn_n_start_e +
                                best$nn_n_end_b + best$nn_n_end_e)
best$TR_contains_nasal <- !is.na(best$TR_n_start_b + best$TR_n_start_e +
                                   best$TR_n_end_b + best$TR_n_end_e)
best$MW_contains_nasal <- !is.na(best$MW_n_start_b + best$MW_n_start_e +
                                   best$MW_n_end_b + best$MW_n_end_e)

best$contains_nasalization <- !is.na(best$nn_N_start_b + best$nn_N_start_e +
                                       best$nn_N_end_b + best$nn_N_end_e)
best$TR_contains_nasalization <- !is.na(best$TR_N_b + best$TR_N_e)
best$MW_contains_nasalization <- !is.na(best$MW_N_b + best$MW_N_e)

best$TR_variant <- mapply(function(x,y,z) ifelse(x,
                                                 ifelse(y,"@n",
                                                        ifelse(z,"~","@")),
                                                 ifelse(y,"n","0")),
                          best$TR_contains_schwa,
                          best$TR_contains_nasal,
                          best$TR_contains_nasalization)
best$TR_variant <- factor(x = best$TR_variant, levels = c("@", "@n", "~", "0", "n"))

best$MW_variant <- mapply(function(x,y,z) ifelse(x,
                                                 ifelse(y,"@n",
                                                        ifelse(z,"~","@")),
                                                 ifelse(y,"n","0")),
                          best$MW_contains_schwa,
                          best$MW_contains_nasal,
                          best$MW_contains_nasalization)
best$MW_variant <- factor(x = best$MW_variant, levels = c("@", "@n", "~", "0", "n"))

best$frag_dur <- best$kal_end - best$kal_start + .5
best$end_first_quart <- best$kal_start - .3 + .33*best$frag_dur
best$early_schwa <- best$nn_e_start_b < best$end_first_quart
if (length(best[best$early_schwa & !is.na(best$early_schwa),]$contains_schwa) > 0){
  best[best$early_schwa & !is.na(best$early_schwa),]$contains_schwa <- FALSE
}

best$classifier_variant <- mapply(function(x,y,z) ifelse(x,
                                                         ifelse(y,"@n",
                                                                ifelse(z,"~","@")),
                                                         ifelse(y,"n","0")),
                                  best$contains_schwa,
                                  best$contains_nasal,
                                  best$contains_nasalization)
best$classifier_variant <- factor(x = best$classifier_variant, levels = c("@", "@n", "~", "0", "n"))

vali_indices <- c(35, 55, 17, 38, 18, 42, 16, 27, 43, 6, 48, 1, 45, 32, 41, 51, 49, 31, 28, 2, 4, 14, 12, 19, 47, 10, 22, 13, 56, 24)
test_indices <- c(5, 11, 15, 25, 34, 37, 46, 50, 52, 53, 57, 58, 7, 8, 9, 20, 21, 29, 33, 36, 40, 44, 54, 39, 3, 23, 26, 30, 59, 60)

v_cm <- table(best[vali_indices,]$MW_variant, best[vali_indices,]$TR_variant, dnn = c("MW", "TR"))
v_recall <- diag(v_cm) / colSums(v_cm)
v_macro.recall <- mean(na.omit(v_recall))
v_precision <- diag(v_cm) / rowSums(v_cm)
v_macro.precision <- mean(na.omit(v_precision))
v_macro.Fscore <- 2*((v_macro.precision * v_macro.recall) / (v_macro.precision + v_macro.recall))
v_Accuracy <- sum(diag(v_cm)) / sum(v_cm)
v_human_average_score <- (gs_df$v_perc20_TR_MW[1] + v_Accuracy + v_macro.Fscore) / 3

t_cm <- table(best[test_indices,]$MW_variant, best[test_indices,]$TR_variant, dnn = c("MW", "TR"))
t_recall <- diag(t_cm) / colSums(t_cm)
t_macro.recall <- mean(na.omit(t_recall))
t_precision <- diag(t_cm) / rowSums(t_cm)
t_macro.precision <- mean(na.omit(t_precision))
t_macro.Fscore <- 2*((t_macro.precision * t_macro.recall) / (t_macro.precision + t_macro.recall))
t_Accuracy <- sum(diag(t_cm)) / sum(t_cm)
t_human_average_score <- (gs_df$t_perc20_TR_MW[1] + t_Accuracy + t_macro.Fscore) / 3


v_cm_TZ <- table(best[vali_indices,]$TR_variant, best[vali_indices,]$suffix_variant, dnn = c("TR", "TZ"))
v_recall_TZ <- diag(v_cm_TZ) / colSums(v_cm_TZ)
v_macro.recall_TZ <- mean(na.omit(v_recall_TZ))
v_precision_TZ <- diag(v_cm_TZ) / rowSums(v_cm_TZ)
v_macro.precision_TZ <- mean(na.omit(v_precision_TZ))
v_macro.Fscore_TZ <- 2*((v_macro.precision_TZ * v_macro.recall_TZ) / (v_macro.precision_TZ + v_macro.recall_TZ))
v_Accuracy_TZ <- sum(diag(v_cm_TZ)) / sum(v_cm_TZ)
v_human_average_score_TZ <- (gs_df$v_perc20_TR_MW[1] + v_Accuracy_TZ + v_macro.Fscore_TZ) / 3

t_cm_TZ <- table(best[test_indices,]$TR_variant, best[test_indices,]$suffix_variant, dnn = c("TR", "TZ"))
t_recall_TZ <- diag(t_cm_TZ) / colSums(t_cm_TZ)
t_macro.recall_TZ <- mean(na.omit(t_recall_TZ))
t_precision_TZ <- diag(t_cm_TZ) / rowSums(t_cm_TZ)
t_macro.precision_TZ <- mean(na.omit(t_precision_TZ))
t_macro.Fscore_TZ <- 2*((t_macro.precision_TZ * t_macro.recall_TZ) / (t_macro.precision_TZ + t_macro.recall_TZ))
t_Accuracy_TZ <- sum(diag(t_cm_TZ)) / sum(t_cm_TZ)
t_human_average_score_TZ <- (gs_df$t_perc20_TR_MW[1] + t_Accuracy_TZ + t_macro.Fscore_TZ) / 3

v_cm_NN <- table(best[vali_indices,]$classifier_variant, best[vali_indices,]$MW_variant, dnn = c("NN", "MW"))
v_recall_NN <- diag(v_cm_NN) / colSums(v_cm_NN)
v_macro.recall_NN <- mean(na.omit(v_recall_NN))
v_precision_NN <- diag(v_cm_NN) / rowSums(v_cm_NN)
v_macro.precision_NN <- mean(na.omit(v_precision_NN))
v_macro.Fscore_NN <- 2*((v_macro.precision_NN * v_macro.recall_NN) / (v_macro.precision_NN + v_macro.recall_NN))
v_Accuracy_NN <- sum(diag(v_cm_NN)) / sum(v_cm_NN)

t_cm_NN <- table(best[test_indices,]$classifier_variant, best[test_indices,]$MW_variant, dnn = c("NN", "MW"))
t_recall_NN <- diag(t_cm_NN) / colSums(t_cm_NN)
t_macro.recall_NN <- mean(na.omit(t_recall_NN))
t_precision_NN <- diag(t_cm_NN) / rowSums(t_cm_NN)
t_macro.precision_NN <- mean(na.omit(t_precision_NN))
t_macro.Fscore_NN <- 2*((t_macro.precision_NN * t_macro.recall_NN) / (t_macro.precision_NN + t_macro.recall_NN))
t_Accuracy_NN <- sum(diag(t_cm_NN)) / sum(t_cm_NN)


best_ind <- as.integer(seq(from=1, to=nrow(gs_df), length.out = 100))
plot(best_ind - 0.25*(nrow(gs_df) %/% 100), gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_classifier_average_score[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_classifier_average_score[best_ind])-0.002, max(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_classifier_average_score[best_ind])+0.002),
     main = "Matching variants (Average Score)")
points(best_ind + 0.25*(nrow(gs_df) %/% 100), gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_classifier_average_score[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = v_human_average_score, lty="dashed", col="black")
abline(h = t_human_average_score, lty="dashed", col="red")

best_ind <- 1:100
plot(best_ind - 0.25, gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_classifier_average_score[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_classifier_average_score[best_ind])-0.002, max(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_classifier_average_score[best_ind])+0.002),
     main = "Matching variants (Average Score)")
points(best_ind + 0.25, gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_classifier_average_score[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

# combined score TZ
best_ind <- as.integer(seq(from=1, to=nrow(gs_df), length.out = 100))
plot(best_ind - 0.25*(nrow(gs_df) %/% 100), gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$v_classifier_average_score_TZ[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(c(gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$t_classifier_average_score_TZ[best_ind], 
                    gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$v_classifier_average_score_TZ[best_ind]))-0.002, 
              max(gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$v_classifier_average_score_TZ[best_ind])+0.002),
     main = "Matching variants (Average score_TZ)")
points(best_ind + 0.25*(nrow(gs_df) %/% 100), gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$t_classifier_average_score_TZ[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = v_human_average_score_TZ, lty="dashed", col="black")
abline(h = t_human_average_score_TZ, lty="dashed", col="red")

best_ind <- 1:100
plot(best_ind - 0.25, gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$v_classifier_average_score_TZ[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$t_classifier_average_score_TZ[best_ind])-0.002, max(gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$v_classifier_average_score_TZ[best_ind])+0.002),
     main = "Matching variants (Average score_TZ)")
points(best_ind + 0.25, gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$t_classifier_average_score_TZ[best_ind], type = "h", col="red")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)




# vali set
best_MW_NN_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_schwa_perc20_MW_NN[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_nasal_perc20_MW_NN[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_nasalization_perc20_MW_NN[1])
TR_MW_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_schwa_perc20_TR_MW[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_nasal_perc20_TR_MW[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$v_nasalization_perc20_TR_MW[1])
prec_labels <- c("schwa", "nasal", "nasalization")

par(mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0), mfrow=c(1,2))

plot(NULL, xlim = c(0.5,3.5), ylim = c(0,1), xaxt="n", ylab="Percentage", xlab = "Classifier", main = "Validation set")
for (i in 1:3){
  lines(x = c(i - 0.1, i - 0.1), y = c(0, TR_MW_perc[i]), col="black", lwd=4)
  lines(x = c(i + 0.1, i + 0.1), y = c(0, best_MW_NN_perc[i]), col="cadetblue", lwd=4)
}
lines(x = c(2.8, 3), y = c(1,1), col="black", lwd=4)
text(x = 3.3, y = 1, "Human")
lines(x = c(2.8, 3), y = c(.9,.9), col="cadetblue", lwd=4)
text(x = 3.3, y = .9, "NN")
axis(side=1, col="black", at=1:3, labels=prec_labels)

# test set
best_MW_NN_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_schwa_perc20_MW_NN[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_nasal_perc20_MW_NN[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_nasalization_perc20_MW_NN[1])
TR_MW_perc <- c(gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_schwa_perc20_TR_MW[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_nasal_perc20_TR_MW[1], gs_df[order(gs_df$v_classifier_average_score, decreasing = T),]$t_nasalization_perc20_TR_MW[1])
prec_labels <- c("schwa", "nasal", "nasalization")


plot(NULL, xlim = c(0.5,3.5), ylim = c(0,1), xaxt="n", ylab="Percentage", xlab = "Classifier", main = "Test set")
for (i in 1:3){
  lines(x = c(i - 0.1, i - 0.1), y = c(0, TR_MW_perc[i]), col="black", lwd=4)
  lines(x = c(i + 0.1, i + 0.1), y = c(0, best_MW_NN_perc[i]), col="cadetblue", lwd=4)
}
lines(x = c(2.8, 3), y = c(1,1), col="black", lwd=4)
text(x = 3.3, y = 1, "Human")
lines(x = c(2.8, 3), y = c(.9,.9), col="cadetblue", lwd=4)
text(x = 3.3, y = .9, "NN")
axis(side=1, col="black", at=1:3, labels=prec_labels)

# vali set
best_MW_NN_perc <- c(v_macro.Fscore_NN, v_Accuracy_NN)
TR_MW_perc <- c(v_macro.Fscore, v_Accuracy)
prec_labels <- c("Accuracy", "Macro F-Score")


plot(NULL, xlim = c(0.5,3.5), ylim = c(0,1), xaxt="n", ylab="Percentage", xlab = "Classifier", main = "Validation set")
for (i in 1:2){
  lines(x = c(i - 0.1, i - 0.1), y = c(0, TR_MW_perc[i]), col="black", lwd=4)
  lines(x = c(i + 0.1, i + 0.1), y = c(0, best_MW_NN_perc[i]), col="cadetblue", lwd=4)
}
lines(x = c(2.8, 3), y = c(1,1), col="black", lwd=4)
text(x = 3.3, y = 1, "Human")
lines(x = c(2.8, 3), y = c(.9,.9), col="cadetblue", lwd=4)
text(x = 3.3, y = .9, "NN")
axis(side=1, col="black", at=1:2, labels=prec_labels)

# test set
best_MW_NN_perc <- c(t_macro.Fscore_NN, t_Accuracy_NN)
TR_MW_perc <- c(t_macro.Fscore, t_Accuracy)
prec_labels <- c("Accuracy", "Macro F-Score")


plot(NULL, xlim = c(0.5,3.5), ylim = c(0,1), xaxt="n", ylab="Percentage", xlab = "Classifier", main = "Test set")
for (i in 1:2){
  lines(x = c(i - 0.1, i - 0.1), y = c(0, TR_MW_perc[i]), col="black", lwd=4)
  lines(x = c(i + 0.1, i + 0.1), y = c(0, best_MW_NN_perc[i]), col="cadetblue", lwd=4)
}
lines(x = c(2.8, 3), y = c(1,1), col="black", lwd=4)
text(x = 3.3, y = 1, "Human")
lines(x = c(2.8, 3), y = c(.9,.9), col="cadetblue", lwd=4)
text(x = 3.3, y = .9, "NN")
axis(side=1, col="black", at=1:2, labels=prec_labels)

par(mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0), mfrow=c(1,1))
