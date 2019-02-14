if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/cgn/"
} else {
  f_path = "/vol/tensusers/timzee/cgn/"
}

set.seed(42)

s_df = read.csv(paste(f_path, "all_s_combined.csv", sep = ""))
s_df$language = substr(s_df$wav, 3, 4)
s_df$cgn_comp = substr(s_df$wav, 1, 1)
s_sub_df = s_df[s_df$language == "nl" & s_df$oov_in_chunk == "False" 
                & (s_df$next_phon != "s" | is.na(s_df$next_phon)) 
                & s_df$cgn_comp == "a" 
#                & (s_df$word_class == "N" 
#                   | s_df$word_class == "ADJ" 
#                   | s_df$word_class == "WW" 
#                   | s_df$word_class == "TW") 
                & s_df$type_of_s != "OTHER" 
#                & grepl("\\*", s_df$word_ort) == FALSE 
#                & grepl("'", s_df$word_ort) == FALSE 
#                & grepl("is", s_df$word_ort) == FALSE 
#                & grepl("-", s_df$word_ort) == FALSE 
#                & (grepl("[A-Z]", s_df$word_ort) == FALSE | s_df$type_of_s == "GEN")
#                & grepl("was", s_df$word_ort) == FALSE
                , ]

s_sub_df$type_of_s = as.factor(as.character(s_sub_df$type_of_s))
s_sub_df$word_ort = as.factor(as.character(s_sub_df$word_ort))

word_t = table(s_sub_df$word_ort)
s_sub_df$ort_freq = as.integer(word_t[s_sub_df$word_ort])
s_sub_df = s_sub_df[rowSums(is.na(s_sub_df)) != ncol(s_sub_df),]

s_sub_freq = s_sub_df[s_sub_df$ort_freq > 25,]
s_sub_freq$word_ort = as.factor(as.character(s_sub_freq$word_ort))
s_samp_df1 = s_sub_freq[unlist(tapply(1:nrow(s_sub_freq), s_sub_freq$word_ort, sample, 25)),]

s_samp_df = rbind(s_samp_df1, s_sub_df[s_sub_df$ort_freq < 25,])
# the sampled dataset contains 12871 tokens

par(mfrow = c(1,1))
barplot(table(s_samp_df$type_of_s))

# Load Marein index to get orthography

marein_df = read.csv(paste(f_path, "cgn_ort_index_210119.txt", sep = ""))
s_samp_ort = merge(s_samp_df, marein_df, by.x = c("wav", "chan", "chunk_start", "chunk_end"), by.y = c("wav", "chan", "from", "to"))
s_samp_ort$ort_old = paste('"', s_samp_ort$ort, '"', sep = "")
s_samp_ort$chan_old = s_samp_ort$chan - 1

write.table(s_samp_ort[ ,c(1,20,3,4,19)], file = paste(f_path, "cgn_louis_1.txt", sep = ""), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE)



