if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/s_dur_models/"
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
  ifadv_path = "/Volumes/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/Volumes/tensusers/timzee/ECSD/"
} else {
  f_path = "/vol/tensusers/timzee/s_dur_models/"
  cgn_path = "/vol/tensusers/timzee/cgn/"
  ifadv_path = "/vol/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/vol/tensusers/timzee/ECSD/"
}

s_dur_a = read.csv(paste(cgn_path, "comp-a_s_static_final.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
s_dur_c = read.csv(paste(cgn_path, "comp-c_s_static_final.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
s_dur_d = read.csv(paste(cgn_path, "comp-d_s_static_final.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
s_dur_ifadv = read.csv(paste(ifadv_path, "ifadv_s_static_final.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$mean_hnr = as.factor(s_dur_ifadv$mean_hnr)
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
s_dur_ecsd = read.csv(paste(ecsd_path, "ecsd_s_static_final.csv", sep = ""))
s_dur_ecsd$corpus = as.factor("ecsd")

s_dur = rbind(s_dur_a, s_dur_c, s_dur_d, s_dur_ifadv, s_dur_ecsd)
s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)

s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)

is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron

drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced", "proportion_voiced2", "s_cog_full", "s_cog_window", "per_mil_wf", 
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "chan")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")
s_dur$log_bigf = log10(s_dur$bigram_f + 1)

vowels = c("@", "A", "AU", "E", "E2", "EI", "EU", "I", "O", "U", "UI", "a", "e", "i", "o", "u", "y")
liquids = c("r", "l")
approximants = c("j", "w")
nasals = c("n", "m", "N")
fricatives = c("G", "S", "Z", "f", "h", "s", "v", "x", "z")
plosives = c("b", "d", "g", "k", "p", "t")

get_phon_class = function(x) {
  if (x %in% vowels){
    return("V")
  } else if (x %in% liquids){
    return("L")
  } else if (x %in% approximants){
    return("APP")
  } else if (x %in% nasals){
    return("N")
  } else if (x %in% fricatives){
    return("F")
  } else if (x %in% plosives){
    return("P")
  } else if (!is.na(x) & x == "SIL"){
    return("SIL")
  } else {
    return(NA)
  }
}

s_dur$next_phon_class = as.factor(sapply(s_dur$next_phon_pron, get_phon_class))
s_dur$prev_phon_class = as.factor(sapply(s_dur$prev_phon_pron, get_phon_class))

s_dur$prev_phon_class = relevel(s_dur$prev_phon_class, ref="V")
s_dur$next_phon_class = relevel(s_dur$next_phon_class, ref="V")

# remove lines for which measurements failed
s_dur = s_dur[!is.na(s_dur$s_dur), ]
# remove NA lines
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

# remove unrepresentative outliers (Baayen, 2008, p. 243)
s_dur = s_dur[s_dur$s_dur < 0.4,]

# make new predictors
n_cow = 5052213
n_subtlex = 437504
s_dur$wf = 10^s_dur$log_wf
s_dur[is.na(s_dur$wf),]$wf = 1
s_dur$log_wf = log10(s_dur$wf)
s_dur$p_next_w = (s_dur$bigram_f / n_cow) / (s_dur$wf / n_subtlex)

s_dur[is.na(s_dur$lex_neb_freq),]$lex_neb_freq = 0
s_dur$prop_lex_neb_freq = s_dur$lex_neb_freq / s_dur$wf

s_dur$mean_syl_dur = s_dur$base_dur / s_dur$num_syl_pron

# transform dependent variable
s_dur$log_s_dur = log10(s_dur$s_dur)

drop = c("lex_neb", "prev_phon_pron", "next_phon_pron", "lex_neb_freq", "bigram_f", "num_syl", 
         "word_stress", "base_dur", "num_syl_pron", "stress_dist", "log_bigf", "prev_phon_class",
         "wf")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

############
# omit lines that are ignored by the model
table(s_dur$type_of_s, s_dur$corpus)

abc = s_dur[s_dur$type_of_s == "GEN-POSS" & is.na(s_dur$stressed),]

s_dur = na.omit(s_dur)
table(s_dur$type_of_s, s_dur$corpus)




