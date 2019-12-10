if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/cgn/"
} else {
  f_path = "/vol/tensusers/timzee/cgn/"
}

set.seed(42)

en_df = read.csv(paste(f_path, "all_en_comb_c_ndl.csv", sep = ""))
# if non cgn corpus
en_df$language = "nl"
en_df$language = as.factor(en_df$language)
# if cgn component
#s_df$language = as.factor(substr(s_df$wav, 3, 4))
#s_df$cgn_comp = as.factor(substr(s_df$wav, 1, 1))
en_sub_df = en_df[en_df$language == "nl" 
                & en_df$overlap == FALSE # always include this if cgn-a, cgn-c or cgn-d
                & (en_df$next_phon_pron != "n" | is.na(en_df$next_phon_pron)) 
                & (en_df$next_phon_pron != "[SPN]" | is.na(en_df$next_phon_pron)) 
                & (en_df$prev_phon_pron != "[SPN]" | is.na(en_df$prev_phon_pron)) 
                & (en_df$prev_phon_pron != "SIL" | is.na(en_df$prev_phon_pron)) 
                & en_df$type_of_en != "OTHER" 
                & !grepl("accented", en_df$oov_meta, fixed=TRUE) 
                & !grepl("dialect_word", en_df$oov_meta, fixed=TRUE) 
                & !grepl("foreign_word", en_df$oov_meta, fixed=TRUE) 
                & !grepl("incomplete", en_df$oov_meta, fixed=TRUE) 
                & !grepl("interjection", en_df$oov_meta, fixed=TRUE)
                & !grepl("mispronunciation", en_df$oov_meta, fixed=TRUE) 
                & !grepl("numeral", en_df$oov_meta, fixed=TRUE) 
                & !grepl("unclear", en_df$oov_meta, fixed=TRUE) 
                & en_df$phon_pron %in% c("@", "n") 
#                & grepl("\\*", s_df$word_ort) == FALSE 
#                & grepl("'", s_df$word_ort) == FALSE 
#                & grepl("is", s_df$word_ort) == FALSE 
#                & grepl("-", s_df$word_ort) == FALSE 
#                & (grepl("[A-Z]", s_df$word_ort) == FALSE | s_df$type_of_s == "GEN")
#                & grepl("was", s_df$word_ort) == FALSE
                , ]

en_sub_df$type_of_en = as.factor(as.character(en_sub_df$type_of_en))
en_sub_df$word_ort = as.factor(as.character(en_sub_df$word_ort))
en_sub_df = en_sub_df[rowSums(is.na(en_sub_df)) != ncol(en_sub_df),]

# if we want to exclude negotiations from ecsd
#neg_starts = read.csv(paste(f_path, "negotiations.csv", sep = ""))
#abc = as.list(neg_starts$part)
#names(abc) = neg_starts$pair
#en_sub_df$pair = as.factor(substr(en_sub_df$wav, 3, nchar(as.character(en_sub_df$wav)) - 8))
#en_sub_df$part = as.integer(substr(en_sub_df$wav, nchar(as.character(en_sub_df$wav)) - 1, nchar(as.character(en_sub_df$wav))))
#en_sub_df$neg_start = as.integer(abc[as.character(en_sub_df$pair)])
#en_sub_df = en_sub_df[en_sub_df$part < en_sub_df$neg_start,]

write.table(en_sub_df[ ,c("wav", "chan", "chunk_start", "chunk_end", "tier", "word_chunk_i", 
                           "sent_i", "word_sent_i", "word_ort", "word_phon", "num_phon", "phon_pron", "prev_phon", "prev_phon_pron",
                           "next_phon", "next_phon_pron", "word_pos", "word_class", "type_of_en",
                           "speaker", "per_mil_wf", "log_wf", "lex_neb", "lex_neb_freq", "ptan",
                           "ptaf", "cow_wf", "next_word", "next_wf", "bigram_f", "prev_word", "prev_wf", "prev_bigram_f", "num_syl", "word_stress", "ndl_boundary_diph", "other_ndl_cues")], 
            file = paste(f_path, "comp-c_en_ndl.csv", sep = ""), row.names=FALSE, col.names=TRUE, sep=",", quote = FALSE)


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

