
if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/grid_search/"
} else {
  f_path = "/vol/tensusers/timzee/grid_search/"
}


gs = read.csv(paste(f_path, "gs03_aligned_dist.csv", sep = ""))
mean_distance = sum(na.omit(gs$tran_dist)) / NROW(na.omit(gs$tran_dist))

gs_sil = gs[gs$cgn_tran == "SIL" | gs$kal_tran == "SIL",]
gs_sil$cgn_tran = as.character(gs_sil$cgn_tran)
gs_sil$cgn_tran[is.na(gs_sil$cgn_tran)] = "NA"
gs_sil$kal_tran = as.character(gs_sil$kal_tran)
gs_sil$kal_tran[is.na(gs_sil$kal_tran)] = "NA"
sil_tab = table(gs_sil$cgn_tran, gs_sil$kal_tran)
sil_recall = sil_tab["SIL","SIL"] / sum(sil_tab["SIL",])
sil_precision = sil_tab["SIL","SIL"] / sum(sil_tab[,"SIL"])
sil_F1 = 2 * ((sil_precision * sil_recall) / (sil_precision + sil_recall))

# to measure durational differences between SIL and non-existing SIL
# take start and end time from previous word, unless it is a leading silence,
# in which case both start and end should be set to 0.00
gs$id = 1:nrow(gs)

for (x in gs$id) {
  if (is.na(gs$cgn_tran[x])) {
    if (gs[as.integer(x),]$kal_start == 0.00) {
      gs[as.integer(x), "cgn_start"] = 0.00
      gs[as.integer(x), "cgn_end"] = 0.00
    } else {
      gs$cgn_start[as.integer(x)] = gs$cgn_start[as.integer(x) - 1]
      gs$cgn_end[as.integer(x)] = gs$cgn_end[as.integer(x) - 1]
    }
  } else if (is.na(gs$kal_tran[x])) {
    if (gs[as.integer(x),]$cgn_start == 0.00) {
      gs[as.integer(x), "kal_start"] = 0.00
      gs[as.integer(x), "kal_end"] = 0.00
    } else {
      gs$kal_start[as.integer(x)] = gs$kal_start[as.integer(x) - 1]
      gs$kal_end[as.integer(x)] = gs$kal_end[as.integer(x) - 1]
    }
  }
}

gs_sil = gs[gs$cgn_tran == "SIL" | gs$kal_tran == "SIL",]
gs_sil$sil_start_diff = gs_sil$cgn_start - gs_sil$kal_start
mean_sil_start_diff = mean(abs(gs_sil$sil_start_diff))
gs_sil$sil_end_diff = gs_sil$cgn_end - gs_sil$kal_end
mean_sil_end_diff = mean(abs(gs_sil$sil_end_diff))

gs_wrd = gs[gs$word != "silence",]
gs_wrd$start_diff = gs_wrd$cgn_start - gs_wrd$kal_start
mean_wrd_start_diff = mean(abs(gs_wrd$start_diff))
gs_wrd$end_diff = gs_wrd$cgn_end - gs_wrd$kal_end
mean_wrd_end_diff = mean(abs(gs_wrd$end_diff))

gs_n = gs[gs$ends_in_en == TRUE,]
gs_n$cgn_tran = as.character(gs_n$cgn_tran)
gs_n$kal_tran = as.character(gs_n$kal_tran)
gs_n$cgn_final = substr(gs_n$cgn_tran, nchar(gs_n$cgn_tran), nchar(gs_n$cgn_tran))
gs_n$kal_final = substr(gs_n$kal_tran, nchar(gs_n$kal_tran), nchar(gs_n$kal_tran))
gs_n2 = gs_n[gs_n$cgn_final == "n" | gs_n$kal_final == "n",]

n_tab = table(gs_n2$cgn_final, gs_n2$kal_final)
n_recall = n_tab["n","n"] / sum(n_tab["n",])
n_precision = n_tab["n","n"] / sum(n_tab[,"n"])
n_F1 = 2 * ((n_precision * n_recall) / (n_precision + n_recall))

cat("\n\nMEASURES:\n")
cat("---------------------------------------\n")
cat("- Mean Distance: ", mean_distance, "\n\n")
cat("- SIL recall: ", sil_recall, "\n")
cat("- SIL precision: ", sil_precision, "\n")
cat("- SIL F-measure: ", sil_F1, "\n\n")
cat("- SIL start diff: ", mean_sil_start_diff, "\n")
cat("- SIL end diff: ", mean_sil_end_diff, "\n\n")
cat("- Word start diff: ", mean_wrd_start_diff, "\n")
cat("- Word end diff: ", mean_wrd_end_diff, "\n\n")
cat("- N recall: ", n_recall, "\n")
cat("- N precision: ", n_precision, "\n")
cat("- N F-measure: ", n_F1, "\n")
cat("---------------------------------------\n")
