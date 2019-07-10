if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/cgn/"
} else {
  f_path = "/vol/tensusers/timzee/cgn/"
}

set.seed(42)

s_df = read.csv(paste(f_path, "all_s_comb_o.csv", sep = ""))
# if non cgn corpus
#s_df$language = "nl"
#s_df$language = as.factor(s_df$language)
# if cgn component
s_df$language = as.factor(substr(s_df$wav, 3, 4))
s_df$cgn_comp = as.factor(substr(s_df$wav, 1, 1))
s_sub_df = s_df[s_df$language == "nl" 
                & s_df$overlap == FALSE 
                & (s_df$next_phon_pron != "s" | is.na(s_df$next_phon_pron)) 
                & (s_df$next_phon_pron != "z" | is.na(s_df$next_phon_pron)) 
                & (s_df$next_phon_pron != "S" | is.na(s_df$next_phon_pron)) 
                & (s_df$next_phon_pron != "Z" | is.na(s_df$next_phon_pron)) 
                & (s_df$next_phon_pron != "[SPN]" | is.na(s_df$next_phon_pron)) 
                & (s_df$prev_phon_pron != "s" | is.na(s_df$prev_phon_pron)) 
                & (s_df$prev_phon_pron != "z" | is.na(s_df$prev_phon_pron)) 
                & (s_df$prev_phon_pron != "S" | is.na(s_df$prev_phon_pron)) 
                & (s_df$prev_phon_pron != "Z" | is.na(s_df$prev_phon_pron)) 
                & (s_df$prev_phon_pron != "[SPN]" | is.na(s_df$prev_phon_pron)) 
                & (s_df$prev_phon_pron != "SIL" | is.na(s_df$prev_phon_pron)) 
#                & s_df$cgn_comp == "a" 
#                & (s_df$word_class == "N" 
#                   | s_df$word_class == "ADJ" 
#                   | s_df$word_class == "WW" 
#                   | s_df$word_class == "TW") 
                & s_df$type_of_s != "OTHER" 
                & !grepl("accented", s_df$oov_meta, fixed=TRUE) 
                & !grepl("dialect_word", s_df$oov_meta, fixed=TRUE) 
                & !grepl("foreign_word", s_df$oov_meta, fixed=TRUE) 
                & !grepl("incomplete", s_df$oov_meta, fixed=TRUE) 
                & !grepl("interjection", s_df$oov_meta, fixed=TRUE)
                & !grepl("mispronunciation", s_df$oov_meta, fixed=TRUE) 
                & !grepl("numeral", s_df$oov_meta, fixed=TRUE) 
                & !grepl("unclear", s_df$oov_meta, fixed=TRUE) 
                & s_df$phon_pron == "s" 
#                & grepl("\\*", s_df$word_ort) == FALSE 
#                & grepl("'", s_df$word_ort) == FALSE 
#                & grepl("is", s_df$word_ort) == FALSE 
#                & grepl("-", s_df$word_ort) == FALSE 
#                & (grepl("[A-Z]", s_df$word_ort) == FALSE | s_df$type_of_s == "GEN")
#                & grepl("was", s_df$word_ort) == FALSE
                , ]

s_sub_df$type_of_s = as.factor(as.character(s_sub_df$type_of_s))
s_sub_df$word_ort = as.factor(as.character(s_sub_df$word_ort))
s_sub_df = s_sub_df[rowSums(is.na(s_sub_df)) != ncol(s_sub_df),]

# if we want to exclude negotiations from ecsd
#neg_starts = read.csv(paste(f_path, "negotiations.csv", sep = ""))
#abc = as.list(neg_starts$part)
#names(abc) = neg_starts$pair
#s_sub_df$pair = as.factor(substr(s_sub_df$wav, 3, nchar(as.character(s_sub_df$wav)) - 8))
#s_sub_df$part = as.integer(substr(s_sub_df$wav, nchar(as.character(s_sub_df$wav)) - 1, nchar(as.character(s_sub_df$wav))))
#s_sub_df$neg_start = as.integer(abc[as.character(s_sub_df$pair)])
#s_sub_df = s_sub_df[s_sub_df$part < s_sub_df$neg_start,]

write.table(s_sub_df[ ,c("wav", "chan", "chunk_start", "chunk_end", "tier", "word_chunk_i", 
                           "sent_i", "word_sent_i", "word_ort", "prev_phon", "prev_phon_pron",
                           "next_phon", "next_phon_pron", "word_pos", "word_class", "type_of_s",
                           "speaker", "per_mil_wf", "log_wf", "lex_neb", "lex_neb_freq", "ptan",
                           "ptaf", "bigram_f", "num_syl", "word_stress")], 
            file = paste(f_path, "comp-o_s.csv", sep = ""), row.names=FALSE, col.names=TRUE, sep=",", quote = FALSE)


#word_t = table(s_sub_df$word_ort)
#s_sub_df$ort_freq = as.integer(word_t[s_sub_df$word_ort])
#s_sub_df = s_sub_df[rowSums(is.na(s_sub_df)) != ncol(s_sub_df),]

#s_sub_freq = s_sub_df[s_sub_df$ort_freq > 25,]
#s_sub_freq$word_ort = as.factor(as.character(s_sub_freq$word_ort))
#s_samp_df1 = s_sub_freq[unlist(tapply(1:nrow(s_sub_freq), s_sub_freq$word_ort, sample, 25)),]

#s_samp_df = rbind(s_samp_df1, s_sub_df[s_sub_df$ort_freq < 25,])
# the sampled dataset contains 12871 tokens

#par(mfrow = c(1,1))
#barplot(table(s_samp_df$type_of_s))

# Load Marein index to get orthography

#marein_df = read.csv(paste(f_path, "cgn_ort_index_210119.txt", sep = ""))
#s_samp_ort = merge(s_samp_df, marein_df, by.x = c("wav", "chan", "chunk_start", "chunk_end"), by.y = c("wav", "chan", "from", "to"))
#s_samp_ort$ort_old = paste('"', s_samp_ort$ort, '"', sep = "")
#s_samp_ort$chan_old = s_samp_ort$chan - 1

#write.table(s_samp_ort[ ,c(1,20,3,4,19)], file = paste(f_path, "cgn_louis_1.txt", sep = ""), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE)

#write.table(s_samp_ort[ ,1:14], file = paste(f_path, "cgn_praat_1.csv", sep = ""), row.names=FALSE, col.names=TRUE, sep=",", quote = FALSE)

