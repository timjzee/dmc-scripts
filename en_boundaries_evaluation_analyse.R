if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/classifier_evaluation/en/"
} else {
  f_path = "/vol/tensusers/timzee/classifier_evaluation/en/"
}

gs_df <- read.csv(paste(f_path, "gs_summary.csv", sep = ""))

gs_df$classifier_average_score <- (gs_df$perc20_TR_NN + gs_df$variant_accuracy + gs_df$variant_macro_Fscore) / 3
gs_df$classifier_average_score_TZ <- (gs_df$perc20_TR_NN + gs_df$variant_accuracy_TZ + gs_df$variant_macro_Fscore_TZ) / 3


# compared to TR
par(mfrow = c(1,1), mar=c(12,6,1,2), oma=c(0,0,2,0))

best_ind <- as.integer(seq(from=1, to=nrow(gs_df)/2, length.out = 100))
plot(best_ind, gs_df$perc20_TR_NN[best_ind], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df$perc20_TR_NN[best_ind])-0.002, max(gs_df$perc20_TR_TS[1],gs_df$perc20_TR_NN[1])+0.002))
text(best_ind, par("usr")[3], labels = gs_df$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = gs_df$perc20_TR_TS[1], lty="dashed", col="red")
text(x = 4000, y = .72, "Annotator agreement", col = "red")

# best 100
best_ind <- 1:100
plot(best_ind, gs_df$perc20_TR_NN[best_ind], xlab = "", ylab = "", xaxt='n', type = "h", ylim = c(min(gs_df$perc20_TR_NN[best_ind])-0.002, max(gs_df$perc20_TR_TS[1],gs_df$perc20_TR_NN[1])+0.002))
text(best_ind, par("usr")[3], labels = gs_df$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = gs_df$perc20_TR_TS[1], lty="dashed", col="red")
text(x = 10, y = .727, "Annotator agreement", col = "red")

#mtext("[Network Type]_[Context Frames]_[KALDI weight]_[Apply Penalty]_[Preceding Context]_[Subsequent Context]_[Smoothing]_[Take sqrt]", side = 3, outer = TRUE)

best_TR_NN_perc <- c(gs_df$schwa_perc20_TR_NN[1], gs_df$nasal_perc20_TR_NN[1], gs_df$nasalization_perc20_TR_NN[1])
TR_TS_perc <- c(gs_df$schwa_perc20_TR_TS[1], gs_df$nasal_perc20_TR_TS[1], gs_df$nasalization_perc20_TR_TS[1])
prec_labels <- c("schwa", "nasal", "nasalization")

par(mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0))

plot(NULL, xlim = c(0.5,3.5), ylim = c(0,1), xaxt="n", ylab="Percentage", xlab = "Classifier")
for (i in 1:3){
  lines(x = c(i - 0.1, i - 0.1), y = c(0, TR_TS_perc[i]), col="black", lwd=4)
  lines(x = c(i + 0.1, i + 0.1), y = c(0, best_TR_NN_perc[i]), col="cadetblue", lwd=4)
}
lines(x = c(2.8, 3), y = c(1,1), col="black", lwd=4)
text(x = 3.3, y = 1, "Human")
lines(x = c(2.8, 3), y = c(.9,.9), col="cadetblue", lwd=4)
text(x = 3.3, y = .9, "NN")
axis(side=1, col="black", at=1:3, labels=prec_labels)

# load output of best classifiers


best <- read.csv(paste(f_path, "grid_search_output/", gs_df$gs_name[1], ".csv", sep = ""))
best <- read.csv(paste(f_path, "grid_search_output/", gs_df[order(gs_df$variant_accuracy_TZ, decreasing = T),]$gs_name[1], ".csv", sep = ""))
best <- read.csv(paste(f_path, "grid_search_output/", gs_df[order(gs_df$variant_macro_Fscore_TZ, decreasing = T),]$gs_name[1], ".csv", sep = ""))
best <- read.csv(paste(f_path, "grid_search_output/", gs_df[order(gs_df$classifier_average_score, decreasing = T),]$gs_name[1], ".csv", sep = ""))

best$contains_schwa <- !is.na(best$nn_e_start_b + best$nn_e_start_e +
                                best$nn_e_end_b + best$nn_e_end_e)
best$TR_contains_schwa <- !is.na(best$TR_e_start_b + best$TR_e_start_e +
                                   best$TR_e_end_b + best$TR_e_end_e)
best$TS_contains_schwa <- !is.na(best$TS_e_start_b + best$TS_e_start_e +
                                   best$TS_e_end_b + best$TS_e_end_e)

best$contains_nasal <- !is.na(best$nn_n_start_b + best$nn_n_start_e +
                                best$nn_n_end_b + best$nn_n_end_e)
best$TR_contains_nasal <- !is.na(best$TR_n_start_b + best$TR_n_start_e +
                                   best$TR_n_end_b + best$TR_n_end_e)
best$TS_contains_nasal <- !is.na(best$TS_n_start_b + best$TS_n_start_e +
                                   best$TS_n_end_b + best$TS_n_end_e)

best$contains_nasalization <- !is.na(best$nn_N_start_b + best$nn_N_start_e +
                                       best$nn_N_end_b + best$nn_N_end_e)
best$TR_contains_nasalization <- !is.na(best$TR_N_b + best$TR_N_e)
best$TS_contains_nasalization <- !is.na(best$TS_N_b + best$TS_N_e)


best$TR_variant <- mapply(function(x,y,z) ifelse(x,
                                                 ifelse(y,"@n",
                                                        ifelse(z,"~","@")),
                                                 ifelse(y,"n","0")),
                          best$TR_contains_schwa,
                          best$TR_contains_nasal,
                          best$TR_contains_nasalization)
best$TR_variant <- factor(x = best$TR_variant, levels = c("@", "@n", "~", "0", "n"))

best$TS_variant <- mapply(function(x,y,z) ifelse(x,
                                                 ifelse(y,"@n",
                                                        ifelse(z,"~","@")),
                                                 ifelse(y,"n","0")),
                          best$TS_contains_schwa,
                          best$TS_contains_nasal,
                          best$TS_contains_nasalization)
best$TS_variant <- factor(x = best$TS_variant, levels = c("@", "@n", "~", "0", "n"))

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

# confusion matrix
cm <- table(best$TS_variant, best$TR_variant, 
            dnn = c("TS", "TR"))
cm <- table(best$TR_variant, best$suffix_variant, 
            dnn = c("TR", "TZ"))

# calculate macro F-score
# first calculate Recall & Precision scores
recall <- diag(cm) / colSums(cm)
macro.recall <- mean(na.omit(recall))
precision <- diag(cm) / rowSums(cm)
macro.precision <- mean(na.omit(precision))
macro.Fscore <- 2*((macro.precision * macro.recall) / (macro.precision + macro.recall))

# Micro F-score amounts to Accuracy if applied to confusion matrix
Accuracy <- sum(diag(cm)) / sum(cm)

cm2 <- table(best$classifier_variant, best$TR_variant, 
             dnn = c("NN", "TR"))
cm2 <- table(best$classifier_variant, best$suffix_variant, 
             dnn = c("NN", "TZ"))

# calculate macro F-score
# first calculate Recall & Precision scores
recall2 <- diag(cm2) / colSums(cm2)
macro.recall2 <- mean(na.omit(recall2))
precision2 <- diag(cm2) / rowSums(cm2)
macro.precision2 <- mean(na.omit(precision2))
macro.Fscore2 <- 2*((macro.precision2 * macro.recall2) / (macro.precision2 + macro.recall2))

# Micro F-score amounts to Accuracy if applied to confusion matrix
Accuracy2 <- sum(diag(cm2)) / sum(cm2)

#
par(mfrow = c(1,1), mar=c(12,6,1,2), oma=c(0,0,2,0))

best_ind <- as.integer(seq(from=1, to=nrow(gs_df)/2, length.out = 100))
plot(best_ind, gs_df[order(gs_df$variant_accuracy, decreasing = T),]$variant_accuracy[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df[order(gs_df$variant_accuracy, decreasing = T),]$variant_accuracy[best_ind])-0.002, max(gs_df$variant_accuracy[best_ind])+0.002),
     main = "Matching variants (Accuracy)")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$variant_accuracy, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = Accuracy, lty="dashed", col="red")
text(x = 10000, y = Accuracy-0.02, "Annotator agreement", col = "red")

#
par(mfrow = c(1,1), mar=c(12,6,1,2), oma=c(0,0,2,0))

best_ind <- as.integer(seq(from=1, to=nrow(gs_df)/2, length.out = 100))
plot(best_ind, gs_df[order(gs_df$variant_macro_Fscore, decreasing = T),]$variant_macro_Fscore[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df[order(gs_df$variant_macro_Fscore, decreasing = T),]$variant_macro_Fscore[best_ind])-0.002, max(c(gs_df$variant_macro_Fscore[best_ind], macro.Fscore))+0.002),
     main = "Matching variants (Macro F-Score)")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$variant_macro_Fscore, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = macro.Fscore, lty="dashed", col="red")
text(x = 10000, y = macro.Fscore-0.02, "Annotator agreement", col = "red")

# Average score
par(mfrow = c(1,1), mar=c(12,6,1,2), oma=c(0,0,2,0))
human_average_score <- (gs_df$perc20_TR_TS[1] + Accuracy + macro.Fscore) / 3

#best_ind <- as.integer(seq(from=1, to=nrow(gs_df)/2, length.out = 100))
best_ind <- 1:100

plot(best_ind, gs_df[order(gs_df$classifier_average_score, decreasing = T),]$classifier_average_score[best_ind], 
     xlab = "", ylab = "", xaxt='n', type = "h", 
     ylim = c(min(gs_df[order(gs_df$classifier_average_score, decreasing = T),]$classifier_average_score[best_ind])-0.002, max(c(gs_df[order(gs_df$classifier_average_score, decreasing = T),]$classifier_average_score[best_ind], human_average_score))+0.002),
     main = "Matching variants (Average Score)")
text(best_ind, par("usr")[3], labels = gs_df[order(gs_df$classifier_average_score, decreasing = T),]$gs_name[best_ind], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.4)

abline(h = human_average_score, lty="dashed", col="red")
text(x = 10000, y = human_average_score-0.02, "Annotator agreement", col = "red")


# check fragments classified as @n that are actually @, to see whether any /n/s
# are recognized which do not belong to the -en
best[best$TS_variant %in% c("@", "~") & best$classifier_variant == "@n",]
best[best$TS_variant == "@" & best$classifier_variant == "~",]

best[best$TR_variant == "~" & best$classifier_variant == "@",]

best[best$TR_variant == "0" & best$classifier_variant == "@",]

best$nn_schwa_dur <- best$nn_e_end_e - best$nn_e_start_b
best[best$nn_schwa_dur > 0.15 & !is.na(best$nn_schwa_dur),]

# Check for features that start in 1st quarter of fragment
best$frag_dur <- best$kal_end - best$kal_start + .5
best$end_first_quart <- best$kal_start - .3 + .33*best$frag_dur
best$early_schwa <- best$nn_e_start_b < best$end_first_quart
best[best$early_schwa & !is.na(best$early_schwa),]
# would work to exclude long vowels


best$early_n <- best$nn_n_start_b < best$end_first_quart
best[best$early_n & !is.na(best$early_n),]


# check for feature boundaries that are temporally inconsistent

best$nn_e_consistent <- best$nn_e_end_e > best$nn_e_end_b & 
  best$nn_e_end_b >= best$nn_e_start_e & 
  best$nn_e_start_e > best$nn_e_start_b

best$nn_n_consistent <- best$nn_n_end_e > best$nn_n_end_b & 
  best$nn_n_end_b >= best$nn_n_start_e & 
  best$nn_n_start_e > best$nn_n_start_b

best$nn_N_consistent <- best$nn_N_end_e > best$nn_N_end_b & 
  best$nn_N_end_b >= best$nn_N_start_e & 
  best$nn_N_start_e > best$nn_N_start_b


#

library(dagitty)
library(rethinking)

par(mfrow=c(2,3))

dagEN <- dagitty("dag {
  SS [unobserved]
  PL [unobserved]
  M [exposure]
  V [outcome]
  SS <- R -> V
  SS -> W -> M -> V
  W -> Ph -> V
  W -> F -> V
  SP -> V
  SS -> PR
  PL -> PR
  PR -> V
  SS -> nW -> nPh -> V
  nW -> nF -> V
}")

#   M -> V
coordinates(dagEN) <- list( x=c(M=2,SS=1,W=1,F=3,Ph=3,nW=2,nPh=3,nF=3,V=5,PR=3,PL=4,R=4,SP=4), 
                            y=c(M=3,SS=1,W=4,F=5,Ph=4,nW=2,nPh=2,nF=3,V=4,PR=1,PL=1,R=0,SP=5) ) 
drawdag( dagEN )

adjustmentSets( dagEN , effect = "direct")
adjustmentSets( dagEN , effect = "total")

dagEN <- dagitty("dag {
  SS [unobserved]
  PL [unobserved]
  M [exposure]
  V [outcome]
  SS <- R -> V
  SS -> W -> M -> V
  SS -> M
  W -> Ph -> V
  W -> F -> V
  SP -> V
  SS -> PR
  PL -> PR
  PR -> V
  SS -> nW -> nPh -> V
  nW -> nF -> V
}")

#   M -> V
coordinates(dagEN) <- list( x=c(M=2,SS=1,W=1,F=3,Ph=3,nW=2,nPh=3,nF=3,V=5,PR=3,PL=4,R=4,SP=4), 
                            y=c(M=3,SS=1,W=4,F=5,Ph=4,nW=2,nPh=2,nF=3,V=4,PR=1,PL=1,R=0,SP=5) ) 
drawdag( dagEN )

adjustmentSets( dagEN , effect = "direct")
adjustmentSets( dagEN , effect = "total")

dagEN <- dagitty("dag {
  SS [unobserved]
  PL [unobserved]
  M [exposure]
  V [outcome]
  SS <- R -> V
  SS -> W
  SS -> M -> V
  W -> Ph -> V
  W -> F -> V
  SP -> V
  SS -> PR
  PL -> PR
  PR -> V
  SS -> nW -> nPh -> V
  nW -> nF -> V
}")

#   M -> V
coordinates(dagEN) <- list( x=c(M=2,SS=1,W=1,F=3,Ph=3,nW=2,nPh=3,nF=3,V=5,PR=3,PL=4,R=4,SP=4), 
                            y=c(M=3,SS=1,W=4,F=5,Ph=4,nW=2,nPh=2,nF=3,V=4,PR=1,PL=1,R=0,SP=5) ) 
drawdag( dagEN )

adjustmentSets( dagEN , effect = "direct")
adjustmentSets( dagEN , effect = "total")

dagEN <- dagitty("dag {
  SS [unobserved]
  PL [unobserved]
  M [exposure]
  V [outcome]
  SS <- R -> V
  SS -> W <- M
  SS -> M -> V
  W -> Ph -> V
  W -> F -> V
  SP -> V
  SS -> PR
  PL -> PR
  PR -> V
  SS -> nW -> nPh -> V
  nW -> nF -> V
}")

#   M -> V
coordinates(dagEN) <- list( x=c(M=2,SS=1,W=1,F=3,Ph=3,nW=2,nPh=3,nF=3,V=5,PR=3,PL=4,R=4,SP=4), 
                            y=c(M=3,SS=1,W=4,F=5,Ph=4,nW=2,nPh=2,nF=3,V=4,PR=1,PL=1,R=0,SP=5) ) 
drawdag( dagEN )

adjustmentSets( dagEN , effect = "direct")
adjustmentSets( dagEN , effect = "total")
# total effect of M cannot be determined because we cannot control SS
# in other words we can't distinguish between the effect of W and M

dagEN <- dagitty("dag {
  SS [unobserved]
  PL [unobserved]
  M [exposure]
  V [outcome]
  SS <- R -> V
  W <- M
  SS -> M -> V
  W -> Ph -> V
  W -> F -> V
  SP -> V
  SS -> PR
  PL -> PR
  PR -> V
  SS -> nW -> nPh -> V
  nW -> nF -> V
}")

#   M -> V
coordinates(dagEN) <- list( x=c(M=2,SS=1,W=1,F=3,Ph=3,nW=2,nPh=3,nF=3,V=5,PR=3,PL=4,R=4,SP=4), 
                            y=c(M=3,SS=1,W=4,F=5,Ph=4,nW=2,nPh=2,nF=3,V=4,PR=1,PL=1,R=0,SP=5) ) 
drawdag( dagEN )

adjustmentSets( dagEN , effect = "direct")
adjustmentSets( dagEN , effect = "total")

par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1))


testDAG <- dagitty("dag {
  A [exposure]
  Z [outcome]
  A -> Z
  A -> B -> Z
  A -> C -> Z
  A -> D -> E -> Z
}")

coordinates(testDAG) <- list( x=c(A=0,B=1,C=1,D=1,E=2,Z=3), 
                            y=c(A=0,B=1,C=2,D=3,E=3,Z=0) ) 

drawdag( testDAG )
drawopenpaths(testDAG)



#####
TZ_e_indices <- as.integer(row.names(best[best$suffix_variant == "@",]))
TZ_en_indices <- as.integer(row.names(best[best$suffix_variant == "@n",]))
TZ_n_indices <- as.integer(row.names(best[best$suffix_variant == "n",]))
TZ_0_indices <- as.integer(row.names(best[best$suffix_variant == "0",]))
TZ_e_nas_indices <- as.integer(row.names(best[best$suffix_variant == "~",]))

vali_e_indices <- sample(x = TZ_e_indices, size = length(TZ_e_indices) %/% 2, replace = F)
test_e_indices <- TZ_e_indices[!(TZ_e_indices %in% vali_e_indices)]

vali_en_indices <- sample(x = TZ_en_indices, size = length(TZ_en_indices) %/% 2, replace = F)
test_en_indices <- TZ_en_indices[!(TZ_en_indices %in% vali_en_indices)]

vali_n_indices <- sample(x = TZ_n_indices, size = length(TZ_n_indices) %/% 2, replace = F)
test_n_indices <- TZ_n_indices[!(TZ_n_indices %in% vali_n_indices)]

test_0_indices <- sample(x = TZ_0_indices, size = length(TZ_0_indices) %/% 2, replace = F)
vali_0_indices <- TZ_0_indices[!(TZ_0_indices %in% test_0_indices)]

vali_e_nas_indices <- sample(x = TZ_e_nas_indices, size = length(TZ_e_nas_indices) %/% 2, replace = F)
test_e_nas_indices <- TZ_e_nas_indices[!(TZ_e_nas_indices %in% vali_e_nas_indices)]

vali_indices <- c(vali_e_indices, vali_en_indices, vali_n_indices, vali_0_indices, vali_e_nas_indices)
test_indices <- c(test_e_indices, test_en_indices, test_n_indices, test_0_indices, test_e_nas_indices)

