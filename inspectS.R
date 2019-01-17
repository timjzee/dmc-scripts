if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/cgn/"
} else {
  f_path = "/vol/tensusers/timzee/cgn/"
}

s_df = read.csv(paste(f_path, "s_words_class.csv", sep = ""))
s_df$next_sound = substr(s_df$next_phon, 1, 1)
s_df$chunk_start = sprintf("%.3f", s_df$chunk_start)
s_df$chunk_end = sprintf("%.3f", s_df$chunk_end)
# select data that is in core CGN, which does not contain OOVs in chunk,
# which is not followed by /s/, and which is Netherlandic Dutch
s_sub_df = s_df[s_df$language == "nl" & s_df$oov_in_chunk == 0 
                & (s_df$next_sound != "s" | is.na(s_df$next_sound)) 
                & s_df$in_core == 1 & (s_df$word_class == "N" 
                                       | s_df$word_class == "ADJ" 
                                       | s_df$word_class == "WW"
                                       | s_df$word_class == "TW") 
                & s_df$type_of_s != "OTHER" 
                & grepl("\\*", s_df$word_ort) == FALSE 
                & grepl("'", s_df$word_ort) == FALSE 
                & grepl("is", s_df$word_ort) == FALSE 
                & grepl("-", s_df$word_ort) == FALSE 
                & (grepl("[A-Z]", s_df$word_ort) == FALSE | s_df$type_of_s == "GEN")
                & grepl("was", s_df$word_ort) == FALSE, ]

s_sub_df_u = s_sub_df[!duplicated(s_sub_df$word_ort),]

s_sub_df_S = s_sub_df_u[s_sub_df_u$type_of_s == "S",]
s_sub_df_S_smp = s_sub_df_S[sample(nrow(s_sub_df_S), 60), ]
s_sub_df_PL = s_sub_df_u[s_sub_df_u$type_of_s == "PL",]
s_sub_df_PL_smp = s_sub_df_PL[sample(nrow(s_sub_df_PL), 60), ]
s_sub_df_GEN = s_sub_df_u[s_sub_df_u$type_of_s == "GEN",]
s_sub_df_GEN_smp = s_sub_df_GEN[sample(nrow(s_sub_df_GEN), 40), ]
s_sub_df_DER = s_sub_df_u[s_sub_df_u$type_of_s == "DER",]
s_sub_df_DER_smp = s_sub_df_DER[sample(nrow(s_sub_df_DER), 40), ]

s_sub_df_smp = rbind(s_sub_df_DER_smp, s_sub_df_GEN_smp, s_sub_df_PL_smp, s_sub_df_S_smp)
s_sub_df_smp_u = s_sub_df_smp[!duplicated(s_sub_df_smp[, c("filename", "chunk_start", "chunk_end")]),]


write.table(s_sub_df_smp_u[ ,c(1,2,3,6,7,8)], file = paste(f_path, "chunks.csv", sep = ""), row.names=FALSE, sep=",")
