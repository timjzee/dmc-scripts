if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/IFAcorpus/"
} else {
  f_path = "/vol/tensusers/timzee/IFAcorpus/"
}

agr2 = read.csv(paste(f_path, "validation_data_time_from_ifa_pos.csv", sep = ""))
threshold = 0.02
agr2$ifa_kaldi_agr = agr2$ifa_kaldi_diff < threshold
agr2$ifa_tim_agr = agr2$ifa_tim_diff < threshold
agr2$tim_kaldi_agr = agr2$tim_kaldi_diff < threshold

i_k_ins = agr2[(is.na(agr2$ifa_labels) == TRUE & is.na(agr2$kaldi_labels) == FALSE), ]
prop_k_ins = nrow(i_k_ins) / nrow(agr2[is.na(agr2$kaldi_labels) == FALSE,])
prop_k_ins
i_t_ins = agr2[(is.na(agr2$ifa_labels) == TRUE & is.na(agr2$tim_labels) == FALSE), ]
prop_t_ins = nrow(i_t_ins) / nrow(agr2[is.na(agr2$tim_labels) == FALSE,])
prop_t_ins

par(mfrow = c(1,3))
agr_tbl = table(agr2[,c(9)])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(agr2[is.na(agr2$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N = ", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(agr2[,c(10)])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(agr2[is.na(agr2$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N = ", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(agr2[,c(11)])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(agr2[is.na(agr2$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N = ", as.character(num_t_k), sep = ""), ylim = c(0, 1))

s_agr2 = agr2[agr2$kaldi_labels == "s" & agr2$kaldi_pos == "E",]
s_agr2 = s_agr2[rowSums(is.na(s_agr2)) != ncol(s_agr2),]

par(mfrow = c(1,3))
agr_tbl = table(s_agr2[,c(9)])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(s_agr2[is.na(s_agr2$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N=", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(s_agr2[,c(10)])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(s_agr2[is.na(s_agr2$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N=", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(s_agr2[,c(11)])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(s_agr2[is.na(s_agr2$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N=", as.character(num_t_k), sep = ""), ylim = c(0, 1))

sub_agr = agr2[!is.na(agr2$ifa_labels) & !is.na(agr2$kaldi_labels) & !is.na(agr2$tim_labels),]

par(mfrow = c(1,3))
agr_tbl = table(sub_agr[,c(9)])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(sub_agr[is.na(sub_agr$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N=", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(sub_agr[,c(10)])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(sub_agr[is.na(sub_agr$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N=", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(sub_agr[,c(11)])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(sub_agr[is.na(sub_agr$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N=", as.character(num_t_k), sep = ""), ylim = c(0, 1))
