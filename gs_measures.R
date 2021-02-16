
if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/grid_search/"
} else {
  f_path = "/vol/tensusers/timzee/grid_search/"
}

gs_num = "v2_gs40"

gs = read.csv(paste(f_path, gs_num, "_aligned_dist.csv", sep = ""))
gs$corpus = as.factor(substr(gs$wav, 1, 1))
mean_distance = sum(na.omit(gs$tran_dist)) / NROW(na.omit(gs$tran_dist))

mean_dist_a = sum(na.omit(gs[gs$corpus == "a",]$tran_dist)) / NROW(na.omit(gs[gs$corpus == "a",]$tran_dist))
mean_dist_k = sum(na.omit(gs[gs$corpus == "k",]$tran_dist)) / NROW(na.omit(gs[gs$corpus == "k",]$tran_dist))
mean_dist_o = sum(na.omit(gs[gs$corpus == "o",]$tran_dist)) / NROW(na.omit(gs[gs$corpus == "o",]$tran_dist))


gs_sil = gs[gs$cgn_tran == "SIL" | gs$kal_tran == "SIL",]
gs_sil$cgn_tran = as.character(gs_sil$cgn_tran)
gs_sil$cgn_tran[is.na(gs_sil$cgn_tran)] = "NA"
gs_sil$kal_tran = as.character(gs_sil$kal_tran)
gs_sil$kal_tran[is.na(gs_sil$kal_tran)] = "NA"
sil_tab = table(gs_sil$cgn_tran, gs_sil$kal_tran, dnn = c("CGN", "KALDI"))
sil_recall = sil_tab["SIL","SIL"] / sum(sil_tab["SIL",])
sil_precision = sil_tab["SIL","SIL"] / sum(sil_tab[,"SIL"])
sil_F1 = 2 * ((sil_precision * sil_recall) / (sil_precision + sil_recall))

sil_tab_a = table(gs_sil[gs_sil$corpus == "a",]$cgn_tran, gs_sil[gs_sil$corpus == "a",]$kal_tran, dnn = c("CGN", "KALDI"))
sil_recall_a = sil_tab_a["SIL","SIL"] / sum(sil_tab_a["SIL",])
sil_precision_a = sil_tab_a["SIL","SIL"] / sum(sil_tab_a[,"SIL"])
sil_F1_a = 2 * ((sil_precision_a * sil_recall_a) / (sil_precision_a + sil_recall_a))

sil_tab_k = table(gs_sil[gs_sil$corpus == "k",]$cgn_tran, gs_sil[gs_sil$corpus == "k",]$kal_tran, dnn = c("CGN", "KALDI"))
sil_recall_k = sil_tab_k["SIL","SIL"] / sum(sil_tab_k["SIL",])
sil_precision_k = sil_tab_k["SIL","SIL"] / sum(sil_tab_k[,"SIL"])
sil_F1_k = 2 * ((sil_precision_k * sil_recall_k) / (sil_precision_k + sil_recall_k))

sil_tab_o = table(gs_sil[gs_sil$corpus == "o",]$cgn_tran, gs_sil[gs_sil$corpus == "o",]$kal_tran, dnn = c("CGN", "KALDI"))
sil_recall_o = sil_tab_o["SIL","SIL"] / sum(sil_tab_o["SIL",])
sil_precision_o = sil_tab_o["SIL","SIL"] / sum(sil_tab_o[,"SIL"])
sil_F1_o = 2 * ((sil_precision_o * sil_recall_o) / (sil_precision_o + sil_recall_o))


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

mean_sil_start_diff_a = mean(abs(gs_sil[gs_sil$corpus == "a",]$sil_start_diff))
mean_sil_end_diff_a = mean(abs(gs_sil[gs_sil$corpus == "a",]$sil_end_diff))
mean_sil_start_diff_k = mean(abs(gs_sil[gs_sil$corpus == "k",]$sil_start_diff))
mean_sil_end_diff_k = mean(abs(gs_sil[gs_sil$corpus == "k",]$sil_end_diff))
mean_sil_start_diff_o = mean(abs(gs_sil[gs_sil$corpus == "o",]$sil_start_diff))
mean_sil_end_diff_o = mean(abs(gs_sil[gs_sil$corpus == "o",]$sil_end_diff))

gs_wrd = gs[gs$word != "silence",]
gs_wrd$start_diff = gs_wrd$cgn_start - gs_wrd$kal_start
mean_wrd_start_diff = mean(abs(gs_wrd$start_diff))
gs_wrd$end_diff = gs_wrd$cgn_end - gs_wrd$kal_end
mean_wrd_end_diff = mean(abs(gs_wrd$end_diff))

mean_wrd_start_diff_a = mean(abs(gs_wrd[gs_wrd$corpus == "a",]$start_diff))
mean_wrd_end_diff_a = mean(abs(gs_wrd[gs_wrd$corpus == "a",]$end_diff))
mean_wrd_start_diff_k = mean(abs(gs_wrd[gs_wrd$corpus == "k",]$start_diff))
mean_wrd_end_diff_k = mean(abs(gs_wrd[gs_wrd$corpus == "k",]$end_diff))
mean_wrd_start_diff_o = mean(abs(gs_wrd[gs_wrd$corpus == "o",]$start_diff))
mean_wrd_end_diff_o = mean(abs(gs_wrd[gs_wrd$corpus == "o",]$end_diff))

gs$start_diff = gs$cgn_start - gs$kal_start
mean_start_diff = mean(abs(gs$start_diff))
gs$end_diff = gs$cgn_end - gs$kal_end
mean_end_diff = mean(abs(gs$end_diff))

mean_start_diff_a = mean(abs(gs[gs$corpus == "a",]$start_diff))
mean_end_diff_a = mean(abs(gs[gs$corpus == "a",]$end_diff))
mean_start_diff_k = mean(abs(gs[gs$corpus == "k",]$start_diff))
mean_end_diff_k = mean(abs(gs[gs$corpus == "k",]$end_diff))
mean_start_diff_o = mean(abs(gs[gs$corpus == "o",]$start_diff))
mean_end_diff_o = mean(abs(gs[gs$corpus == "o",]$end_diff))


gs_n = gs[gs$ends_in_en == TRUE,]
gs_n$cgn_tran = as.character(gs_n$cgn_tran)
gs_n$kal_tran = as.character(gs_n$kal_tran)
gs_n$cgn_final = substr(gs_n$cgn_tran, nchar(gs_n$cgn_tran), nchar(gs_n$cgn_tran))
gs_n$kal_final = substr(gs_n$kal_tran, nchar(gs_n$kal_tran), nchar(gs_n$kal_tran))
#gs_n2 = gs_n[gs_n$cgn_final == "n" | gs_n$kal_final == "n",]

n_tab = table(gs_n$cgn_final, gs_n$kal_final, dnn = c("CGN", "KALDI"))
n_recall = n_tab["n","n"] / sum(n_tab["n",])
n_precision = n_tab["n","n"] / sum(n_tab[,"n"])
n_F1 = 2 * ((n_precision * n_recall) / (n_precision + n_recall))
if ("@" %in% colnames(n_tab)) {
  schwa_recall = n_tab["@","@"] / sum(n_tab["@",])
  schwa_precision = n_tab["@","@"] / sum(n_tab[,"@"])
  schwa_F1 = 2 * ((schwa_precision * schwa_recall) / (schwa_precision + schwa_recall))
} else {
  schwa_recall = 0
  schwa_precision = 0
  schwa_F1 = 0
}

n_tab_a = table(gs_n[gs_n$corpus == "a",]$cgn_final, gs_n[gs_n$corpus == "a",]$kal_final, dnn = c("CGN", "KALDI"))
n_recall_a = n_tab_a["n","n"] / sum(n_tab_a["n",])
n_precision_a = n_tab_a["n","n"] / sum(n_tab_a[,"n"])
n_F1_a = 2 * ((n_precision_a * n_recall_a) / (n_precision_a + n_recall_a))
if ("@" %in% colnames(n_tab_a)) {
  schwa_recall_a = n_tab_a["@","@"] / sum(n_tab_a["@",])
  schwa_precision_a = n_tab_a["@","@"] / sum(n_tab_a[,"@"])
  schwa_F1_a = 2 * ((schwa_precision_a * schwa_recall_a) / (schwa_precision_a + schwa_recall_a))
} else {
  schwa_recall_a = 0
  schwa_precision_a = 0
  schwa_F1_a = 0
}

n_tab_k = table(gs_n[gs_n$corpus == "k",]$cgn_final, gs_n[gs_n$corpus == "k",]$kal_final, dnn = c("CGN", "KALDI"))
n_recall_k = n_tab_k["n","n"] / sum(n_tab_k["n",])
n_precision_k = n_tab_k["n","n"] / sum(n_tab_k[,"n"])
n_F1_k = 2 * ((n_precision_k * n_recall_k) / (n_precision_k + n_recall_k))
if ("@" %in% colnames(n_tab_a)) {
  schwa_recall_k = n_tab_k["@","@"] / sum(n_tab_k["@",])
  schwa_precision_k = n_tab_k["@","@"] / sum(n_tab_k[,"@"])
  schwa_F1_k = 2 * ((schwa_precision_k * schwa_recall_k) / (schwa_precision_k + schwa_recall_k))
} else {
  schwa_recall_k = 0
  schwa_precision_k = 0
  schwa_F1_k = 0
}

n_tab_o = table(gs_n[gs_n$corpus == "o",]$cgn_final, gs_n[gs_n$corpus == "o",]$kal_final, dnn = c("CGN", "KALDI"))
n_recall_o = n_tab_o["n","n"] / sum(n_tab_o["n",])
n_precision_o = n_tab_o["n","n"] / sum(n_tab_o[,"n"])
n_F1_o = 2 * ((n_precision_o * n_recall_o) / (n_precision_o + n_recall_o))
if ("@" %in% colnames(n_tab_a)) {
  schwa_recall_o = n_tab_o["@","@"] / sum(n_tab_o["@",])
  schwa_precision_o = n_tab_o["@","@"] / sum(n_tab_o[,"@"])
  schwa_F1_o = 2 * ((schwa_precision_o * schwa_recall_o) / (schwa_precision_o + schwa_recall_o))
} else {
  schwa_recall_o = 0
  schwa_precision_o = 0
  schwa_F1_o = 0
}

# get SPN matches
gs_spn = gs[grepl("\\[\\]", gs$cgn_tran) | gs$kal_tran == "[SPN]",]
# if we don't want to count SPNs that are indicated by the orthography
gs_spn = gs_spn[!(grepl("\\[SPN\\]", gs_spn$cgn_tran) | grepl("xx", gs_spn$word)),]

gs_spn$cgn_tran[grepl("\\[\\]", gs_spn$cgn_tran)] = "[SPN]"
gs_spn = gs_spn[rowSums(is.na(gs_spn))<length(gs_spn),]
gs_spn$cgn_tran = as.character(gs_spn$cgn_tran)
gs_spn$cgn_tran[grepl("\\[SPN\\]", gs_spn$cgn_tran) == FALSE] = "word"
gs_spn$cgn_tran = as.factor(gs_spn$cgn_tran)
gs_spn$kal_tran = as.character(gs_spn$kal_tran)
gs_spn$kal_tran[gs_spn$kal_tran != "[SPN]"] = "word"
gs_spn$kal_tran = as.factor(gs_spn$kal_tran)

spn_tab = table(gs_spn$cgn_tran, gs_spn$kal_tran, dnn = c("CGN", "KALDI"))
spn_recall = spn_tab["[SPN]","[SPN]"] / sum(spn_tab["[SPN]",])
spn_precision = spn_tab["[SPN]","[SPN]"] / sum(spn_tab[,"[SPN]"])
spn_F1 = 2 * ((spn_precision * spn_recall) / (spn_precision + spn_recall))

spn_tab_a = table(gs_spn[gs_spn$corpus == "a",]$cgn_tran, gs_spn[gs_spn$corpus == "a",]$kal_tran, dnn = c("CGN", "KALDI"))
spn_recall_a = spn_tab_a["[SPN]","[SPN]"] / sum(spn_tab_a["[SPN]",])
spn_precision_a = spn_tab_a["[SPN]","[SPN]"] / sum(spn_tab_a[,"[SPN]"])
spn_F1_a = 2 * ((spn_precision_a * spn_recall_a) / (spn_precision_a + spn_recall_a))

spn_tab_k = table(gs_spn[gs_spn$corpus == "k",]$cgn_tran, gs_spn[gs_spn$corpus == "k",]$kal_tran, dnn = c("CGN", "KALDI"))
spn_recall_k = spn_tab_k["[SPN]","[SPN]"] / sum(spn_tab_k["[SPN]",])
spn_precision_k = spn_tab_k["[SPN]","[SPN]"] / sum(spn_tab_k[,"[SPN]"])
spn_F1_k = 2 * ((spn_precision_k * spn_recall_k) / (spn_precision_k + spn_recall_k))

spn_tab_o = table(gs_spn[gs_spn$corpus == "o",]$cgn_tran, gs_spn[gs_spn$corpus == "o",]$kal_tran, dnn = c("CGN", "KALDI"))
spn_recall_o = spn_tab_o["[SPN]","[SPN]"] / sum(spn_tab_o["[SPN]",])
spn_precision_o = spn_tab_o["[SPN]","[SPN]"] / sum(spn_tab_o[,"[SPN]"])
spn_F1_o = 2 * ((spn_precision_o * spn_recall_o) / (spn_precision_o + spn_recall_o))


measures = matrix(c(mean_dist_a, mean_dist_k, mean_dist_o, mean_distance,
                    sil_recall_a, sil_recall_k, sil_recall_o, sil_recall,
                    sil_precision_a, sil_precision_k, sil_precision_o, sil_precision,
                    sil_F1_a, sil_F1_k, sil_F1_o, sil_F1,
                    mean_sil_start_diff_a, mean_sil_start_diff_k, mean_sil_start_diff_o, mean_sil_start_diff,
                    mean_sil_end_diff_a, mean_sil_end_diff_k, mean_sil_end_diff_o, mean_sil_end_diff,
                    mean_wrd_start_diff_a, mean_wrd_end_diff_k, mean_wrd_end_diff_o, mean_wrd_start_diff,
                    mean_wrd_end_diff_a, mean_wrd_end_diff_k, mean_wrd_end_diff_o, mean_wrd_end_diff,
                    mean_start_diff_a, mean_start_diff_k, mean_start_diff_o, mean_start_diff,
                    mean_end_diff_a, mean_end_diff_k, mean_end_diff_o, mean_end_diff,
                    n_recall_a, n_recall_k, n_recall_o, n_recall,
                    n_precision_a, n_precision_k, n_precision_o, n_precision,
                    n_F1_a, n_F1_k, n_F1_o, n_F1,
                    schwa_recall_a, schwa_recall_k, schwa_recall_o, schwa_recall,
                    schwa_precision_a, schwa_precision_k, schwa_precision_o, schwa_precision,
                    schwa_F1_a, schwa_F1_k, schwa_F1_o, schwa_F1,
                    spn_recall_a, spn_recall_k, spn_recall_o, spn_recall,
                    spn_precision_a, spn_precision_k, spn_precision_o, spn_precision,
                    spn_F1_a, spn_F1_k, spn_F1_o, spn_F1),
                  nrow = 19, byrow = TRUE,
                  dimnames = list(c("Mean Distance", "SIL recall", "SIL precision",
                                    "SIL F-measure", "SIL start diff", "SIL end diff",
                                    "Word start diff", "Word end diff", "All start diff",
                                    "All end diff", "N recall",
                                    "N precision", "N F-measure", "Schwa recall",
                                    "Schwa precision", "Schwa F-measure",
                                    "SPN recall", "SPN precision", "SPN F-measure"),
                                  c("comp-a", "comp-k", "comp-o", "combined")))

write.table(measures, file = paste(f_path, gs_num, "_measures.csv", sep = ""),
            row.names=TRUE, col.names=TRUE, sep=",", quote = FALSE)

cat("\nMEASURES:\n")
cat("---------------------------------------\n")
#cat("                \tcomp-a\t\tcomp-k\t\tcomp-o\t\tcombined\n\n")
#cat("- Mean Distance:\t", mean_dist_a, "\t", mean_dist_k, "\t", mean_dist_o, "\t", mean_distance, "\n\n")
#cat("- SIL recall:\t\t", sil_recall_a, "\t", sil_recall_k, "\t", sil_recall_o, "\t", sil_recall, "\n")
#cat("- SIL precision:\t", sil_precision_a, "\t", sil_precision_k, "\t", sil_precision_o, "\t", sil_precision, "\n")
#cat("- SIL F-measure:\t", sil_F1_a, "\t", sil_F1_k, "\t", sil_F1_o, "\t", sil_F1, "\n\n")
#cat("- SIL start diff:\t", mean_sil_start_diff_a, "\t", mean_sil_start_diff_k, "\t", mean_sil_start_diff_o, "\t", mean_sil_start_diff, "\n")
#cat("- SIL end diff:\t\t", mean_sil_end_diff_a, "\t", mean_sil_end_diff_k, "\t", mean_sil_end_diff_o, "\t", mean_sil_end_diff, "\n\n")
#cat("- Word start diff:\t", mean_wrd_start_diff_a, "\t", mean_wrd_end_diff_k, "\t", mean_wrd_end_diff_o, "\t", mean_wrd_start_diff, "\n")
#cat("- Word end diff:\t", mean_wrd_end_diff_a, "\t", mean_wrd_end_diff_k, "\t", mean_wrd_end_diff_o, "\t", mean_wrd_end_diff, "\n\n")
#cat("- N recall:\t\t", n_recall_a, "\t", n_recall_k, "\t", n_recall_o, "\t", n_recall, "\n")
#cat("- N precision:\t\t", n_precision_a, "\t", n_precision_k, "\t", n_precision_o, "\t", n_precision, "\n")
#cat("- N F-measure:\t\t", n_F1_a, "\t", n_F1_k, "\t", n_F1_o, "\t", n_F1, "\n")
#cat("---------------------------------------\n")

measures
