if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/other/"
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
  ifadv_path = "/Volumes/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/Volumes/tensusers/timzee/ECSD/"
} else {
  f_path = "/vol/tensusers/timzee/other/"
  cgn_path = "/vol/tensusers/timzee/cgn/"
  ifadv_path = "/vol/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/vol/tensusers/timzee/ECSD/"
}

library(ggplot2)
library(rcompanion)

short_vowels = c("A", "E", "I", "O", "U")
long_vowels = c("AU", "E2", "EI", "EU", "UI", "a", "e", "i", "o", "u", "y")
vowels = c("@", short_vowels, long_vowels)
liquids = c("r", "l")
approximants = c("j", "w")
nasals = c("n", "m", "N")
fricatives = c("G", "S", "Z", "f", "h", "s", "v", "x", "z")
plosives = c("b", "d", "g", "k", "p", "t")

obstruents = c(fricatives, plosives)
sonorants = c(liquids, approximants, nasals)

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

s_dur_a = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-a_timbl2.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
s_dur_a$register = as.factor("conversation")
s_dur_a = s_dur_a[s_dur_a$overlap == FALSE,]
s_dur_a = s_dur_a[ , !(names(s_dur_a) %in% c("overlap", "mean_hnr", "nn_end_score"))]
s_dur_c = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-c_timbl2.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$register = as.factor("conversation")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
s_dur_c = s_dur_c[s_dur_c$overlap == FALSE,]
s_dur_c = s_dur_c[ , !(names(s_dur_c) %in% c("overlap", "mean_hnr", "nn_end_score"))]
s_dur_d = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-d_timbl2.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
s_dur_d$register = as.factor("conversation")
s_dur_d = s_dur_d[s_dur_d$overlap == FALSE,]
s_dur_d = s_dur_d[ , !(names(s_dur_d) %in% c("overlap", "mean_hnr", "nn_end_score"))]
s_dur_ifadv = read.csv(paste(ifadv_path, "synvoirelPL_s_comb_ifadv_timbl2.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$register = as.factor("conversation")
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
s_dur_ifadv = s_dur_ifadv[ , !(names(s_dur_ifadv) %in% c("mean_hnr", "nn_end_score"))]
s_dur_ecsd = read.csv(paste(ecsd_path, "synvoirelPL_s_comb_ecsd_timbl2.csv", sep = ""))
s_dur_ecsd$corpus = as.factor("ecsd")
s_dur_ecsd$register = as.factor("conversation")
s_dur_ecsd = s_dur_ecsd[ , !(names(s_dur_ecsd) %in% c("mean_hnr", "nn_end_score"))]
s_dur_k = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-k_timbl2.csv", sep = ""))
s_dur_k$corpus = as.factor("cgn-k")
s_dur_k$register = as.factor("news")
s_dur_k = s_dur_k[ , !(names(s_dur_k) %in% c("mean_hnr", "nn_end_score"))]
s_dur_o = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-o_timbl2.csv", sep = ""))
s_dur_o$corpus = as.factor("cgn-o")
s_dur_o$register = as.factor("stories")
s_dur_o = s_dur_o[ , !(names(s_dur_o) %in% c("mean_hnr", "nn_end_score"))]
s_dur = rbind(s_dur_a, s_dur_c, s_dur_d, s_dur_ifadv, s_dur_ecsd, s_dur_o, s_dur_k)
s_dur = s_dur[s_dur$type_of_s == "PL",]
s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]
s_dur$s_dur_kal = s_dur$kal_end - s_dur$kal_start
s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)
names(s_dur)[names(s_dur) == "pl_prop"] = "prop_s"


var = read.csv(paste(f_path, "p_f_type_O_merge_2syl_k5_ID_invar.csv", sep = ""))
var = var[var$f_s != 0,]
var$f_nons = var$f_en + var$f_other
var$f_pl = var$f_s + var$f_en + var$f_other
names(var)[names(var) == "f_ev"] = "f_sg"
var$log_freq_pl = log(var$f_pl)
var$prop_s = var$f_s / var$f_pl
var$log_ratio_pl = log((var$f_pl + 1) / (var$f_sg + 1))

get_f_s = function(lem) {
  if (lem == "hersens"){
    lem = "hersen"
  }
  if (lem %in% levels(var$word)){
    return(var[var$word == lem,]$f_s)
  } else {
    return(NA)
  }
}

get_f_en = function(lem) {
  if (lem == "hersens"){
    lem = "hersen"
  }
  if (lem %in% levels(var$word)){
    return(var[var$word == lem,]$f_en)
  } else {
    return(NA)
  }
}

get_f_oth = function(lem) {
  if (lem == "hersens"){
    lem = "hersen"
  }
  if (lem %in% levels(var$word)){
    return(var[var$word == lem,]$f_other)
  } else {
    return(NA)
  }
}

get_f_lem = function(lem) {
  if (lem == "hersens"){
    lem = "hersen"
  }
  if (lem %in% levels(var$word)){
    return(var[var$word == lem,]$f_s + var[var$word == lem,]$f_en + var[var$word == lem,]$f_other + var[var$word == lem,]$f_sg)
  } else {
    print("bla")
    return(NA)
  }
}

s_dur = s_dur[!is.na(s_dur$lemma),]
s_dur$f_s = as.numeric(sapply(as.character(s_dur$lemma), get_f_s))
s_dur = s_dur[!is.na(s_dur$f_s) & length(s_dur$f_s)!=0,]

s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron
s_dur = s_dur[!(s_dur$prev_phon_pron %in% c("s", "z", "S", "Z", "t", "d", "j") | s_dur$next_phon_pron %in% c("s", "z", "S", "Z", "t", "d", "j")),]
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]
s_dur$log_base_dur = log(s_dur$base_dur)
s_dur$next_phon_class = as.factor(sapply(s_dur$next_phon_pron, get_phon_class))
s_dur$prev_phon_class = as.factor(sapply(s_dur$prev_phon_pron, get_phon_class))
s_dur$prev_phon_class = relevel(s_dur$prev_phon_class, ref="V")
s_dur$next_phon_class = relevel(s_dur$next_phon_class, ref="V")
s_dur[is.na(s_dur$lex_neb),]$lex_neb = 0
s_dur[is.na(s_dur$lex_neb_freq),]$lex_neb_freq = 0
s_dur$log_s_dur_kal = log(s_dur$s_dur_kal)
s_dur$syntax_f2 = as.numeric(s_dur$syntax_f2)
s_dur$syntax_f3 = as.numeric(s_dur$syntax_f3)
s_dur$syntax_f4 = as.numeric(s_dur$syntax_f4)
s_dur$syntax_f5 = as.numeric(s_dur$syntax_f5)
s_dur$syntax_f6 = as.numeric(s_dur$syntax_f6)
s_dur$syntax_f7 = as.numeric(s_dur$syntax_f7)
s_dur$syntax_f8 = as.numeric(s_dur$syntax_f8)
s_dur = s_dur[!(is.na(s_dur$type_of_s) | is.na(s_dur$next_phon_class) 
                | is.na(s_dur$prev_mention) | is.na(s_dur$register)), ]
# remove data without prev_word or next_word to avoid artefacts when calculating conditional probabilities
s_dur = s_dur[!(s_dur$next_wf == 0 & s_dur$bigram_f == 0),]
s_dur = s_dur[!(s_dur$prev_wf == 0 & s_dur$prev_bigram_f == 0),]
drop = c("ptan", "ptaf", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced2", "per_mil_wf", "prev_word", "next_word",
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "nn_start", "nn_end", "nn_start_b", "nn_end_b", "nn_start_e", "nn_end_e", "s_dur", "s_dur_nn", "s_dur_kal", "nn_start_score", "nn_end_score", "chan", "timbl_s_prob")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]


s_dur$word_ort = as.character(s_dur$word_ort)
s_dur = s_dur[substr(s_dur$word_ort, nchar(s_dur$word_ort)-2, nchar(s_dur$word_ort)) != "jes",]
s_dur = s_dur[substr(s_dur$word_ort, nchar(s_dur$word_ort)-2, nchar(s_dur$word_ort)) != "kes",]
s_dur$word_ort = as.factor(s_dur$word_ort)
s_dur = na.omit(s_dur)

s_dur$f_en = sapply(as.character(s_dur$lemma), get_f_en)
s_dur$f_other = sapply(as.character(s_dur$lemma), get_f_oth)
s_dur$f_nons = s_dur$f_en + s_dur$f_other
s_dur$f_lem = sapply(as.character(s_dur$lemma), get_f_lem)
s_dur$log_f_s = log(s_dur$f_s)
s_dur$log_f_nons = log(s_dur$f_nons)
s_dur$rel_f_s = s_dur$f_s / s_dur$f_lem
s_dur$log_rel_f_s = log(s_dur$rel_f_s)
s_dur$rel_f_en = s_dur$f_en / s_dur$f_lem
s_dur$rel_f_nons = s_dur$f_nons / s_dur$f_lem
s_dur$rel_f_other = s_dur$f_other / s_dur$f_lem
s_dur$probability_prev = log((s_dur$prev_bigram_f+1) / (s_dur$prev_wf+1))
s_dur$probability_next = log((s_dur$bigram_f+1) / (s_dur$next_wf+1))
s_dur$log_lem_freq = log(s_dur$f_lem)
s_dur$freq_pl = s_dur$f_s + s_dur$f_en + s_dur$f_other
# get rid of variable plurals that only occur twice, because their prop_s is rather meaningless
var = var[var$f_pl > 2,]
s_dur = s_dur[s_dur$freq_pl > 2,]
s_dur$f_ev = s_dur$f_lem - s_dur$freq_pl
s_dur$log_f_sg = log(s_dur$f_ev + 1)
s_dur$rel_f_sg = s_dur$f_ev / s_dur$f_lem
s_dur$log_freq_pl = log(s_dur$freq_pl)
s_dur$log_f_pl = log(s_dur$freq_pl)
s_dur$log_ratio_pl = log((s_dur$freq_pl + 1) / (s_dur$f_ev + 1))
s_dur$log_ratio_s =  log(s_dur$f_s / s_dur$f_nons)
names(s_dur)[names(s_dur) == "word_ort"] = "word"

s_dur$speech_rate_pron_sc = scale(s_dur$speech_rate_pron)
s_dur$log_base_dur_sc = scale(s_dur$log_base_dur)
s_dur$num_syl_pron_sc = scale(s_dur$num_syl_pron)
s_dur$lex_neb_sc = scale(s_dur$lex_neb)
s_dur$probability_next_sc = scale(s_dur$probability_next)
s_dur$probability_prev_sc = scale(s_dur$probability_prev)

col_pred = s_dur[, c("syntax_f2", "syntax_f3", "syntax_f4", "syntax_f5", "syntax_f6", "syntax_f7", "syntax_f8")]
col_pred_pca = prcomp(col_pred, center = T, scale. = T)
# keep pc1 - pc5, threshold of 0.9 cumulative proportion reached
s_dur$PC1 = col_pred_pca$x[,1]
s_dur$PC2 = col_pred_pca$x[,2]
s_dur$PC3 = col_pred_pca$x[,3]
s_dur$PC4 = col_pred_pca$x[,4]
s_dur$PC5 = col_pred_pca$x[,5]

s_dur$PC1_sc = scale(s_dur$PC1)
s_dur$PC2_sc = scale(s_dur$PC2)
s_dur$PC3_sc = scale(s_dur$PC3)
s_dur$PC4_sc = scale(s_dur$PC4)
s_dur$PC5_sc = scale(s_dur$PC5)

s_dur$speaker = as.factor(as.character(s_dur$speaker))
s_dur$word = as.factor(as.character(s_dur$word))
s_dur$prev_phon_class = as.factor(as.character(s_dur$prev_phon_class))
s_dur$prev_phon_class = relevel(s_dur$prev_phon_class, ref="V")

pred_ass = matrix(c(cramerV(table(s_dur[,c("next_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "prev_mention")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ next_phon_class, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("prev_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_phon_class", "prev_mention")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ prev_phon_class, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("stressed", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "prev_mention")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ stressed, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("register", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "prev_mention")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ register, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("prev_mention", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "prev_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "prev_mention")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$speech_rate_pron_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC1_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC2_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC3_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC4_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC5_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$lex_neb_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$probability_prev_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$probability_next_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_ratio_s),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_ratio_pl),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_f_s),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(num_syl_pron_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$num_syl_pron_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$PC1_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$PC2_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$PC3_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$PC4_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$PC5_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$lex_neb_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$probability_prev_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$probability_next_sc),
                   cor(s_dur$num_syl_pron_sc, s_dur$log_ratio_s),
                   cor(s_dur$num_syl_pron_sc, s_dur$log_ratio_pl),
                   cor(s_dur$num_syl_pron_sc, s_dur$log_f_s),
                   cor(s_dur$num_syl_pron_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(log_base_dur_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_base_dur_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$log_base_dur_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$PC1_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$PC2_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$PC3_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$PC4_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$PC5_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$lex_neb_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$probability_prev_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$probability_next_sc),
                   cor(s_dur$log_base_dur_sc, s_dur$log_ratio_s),
                   cor(s_dur$log_base_dur_sc, s_dur$log_ratio_pl),
                   cor(s_dur$log_base_dur_sc, s_dur$log_f_s),
                   cor(s_dur$log_base_dur_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(PC1_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$PC1_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC1_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$PC1_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$PC1_sc, s_dur$PC1_sc),
                   cor(s_dur$PC1_sc, s_dur$PC2_sc),
                   cor(s_dur$PC1_sc, s_dur$PC3_sc),
                   cor(s_dur$PC1_sc, s_dur$PC4_sc),
                   cor(s_dur$PC1_sc, s_dur$PC5_sc),
                   cor(s_dur$PC1_sc, s_dur$lex_neb_sc),
                   cor(s_dur$PC1_sc, s_dur$probability_prev_sc),
                   cor(s_dur$PC1_sc, s_dur$probability_next_sc),
                   cor(s_dur$PC1_sc, s_dur$log_ratio_s),
                   cor(s_dur$PC1_sc, s_dur$log_ratio_pl),
                   cor(s_dur$PC1_sc, s_dur$log_f_s),
                   cor(s_dur$PC1_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(PC2_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$PC2_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC2_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$PC2_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$PC2_sc, s_dur$PC1_sc),
                   cor(s_dur$PC2_sc, s_dur$PC2_sc),
                   cor(s_dur$PC2_sc, s_dur$PC3_sc),
                   cor(s_dur$PC2_sc, s_dur$PC4_sc),
                   cor(s_dur$PC2_sc, s_dur$PC5_sc),
                   cor(s_dur$PC2_sc, s_dur$lex_neb_sc),
                   cor(s_dur$PC2_sc, s_dur$probability_prev_sc),
                   cor(s_dur$PC2_sc, s_dur$probability_next_sc),
                   cor(s_dur$PC2_sc, s_dur$log_ratio_s),
                   cor(s_dur$PC2_sc, s_dur$log_ratio_pl),
                   cor(s_dur$PC2_sc, s_dur$log_f_s),
                   cor(s_dur$PC2_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(PC3_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$PC3_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC3_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$PC3_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$PC3_sc, s_dur$PC1_sc),
                   cor(s_dur$PC3_sc, s_dur$PC2_sc),
                   cor(s_dur$PC3_sc, s_dur$PC3_sc),
                   cor(s_dur$PC3_sc, s_dur$PC4_sc),
                   cor(s_dur$PC3_sc, s_dur$PC5_sc),
                   cor(s_dur$PC3_sc, s_dur$lex_neb_sc),
                   cor(s_dur$PC3_sc, s_dur$probability_prev_sc),
                   cor(s_dur$PC3_sc, s_dur$probability_next_sc),
                   cor(s_dur$PC3_sc, s_dur$log_ratio_s),
                   cor(s_dur$PC3_sc, s_dur$log_ratio_pl),
                   cor(s_dur$PC3_sc, s_dur$log_f_s),
                   cor(s_dur$PC3_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(PC4_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC4_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$PC4_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC4_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$PC4_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$PC4_sc, s_dur$PC1_sc),
                   cor(s_dur$PC4_sc, s_dur$PC2_sc),
                   cor(s_dur$PC4_sc, s_dur$PC3_sc),
                   cor(s_dur$PC4_sc, s_dur$PC4_sc),
                   cor(s_dur$PC4_sc, s_dur$PC5_sc),
                   cor(s_dur$PC4_sc, s_dur$lex_neb_sc),
                   cor(s_dur$PC4_sc, s_dur$probability_prev_sc),
                   cor(s_dur$PC4_sc, s_dur$probability_next_sc),
                   cor(s_dur$PC4_sc, s_dur$log_ratio_s),
                   cor(s_dur$PC4_sc, s_dur$log_ratio_pl),
                   cor(s_dur$PC4_sc, s_dur$log_f_s),
                   cor(s_dur$PC4_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(PC5_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC5_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$PC5_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC5_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$PC5_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$PC5_sc, s_dur$PC1_sc),
                   cor(s_dur$PC5_sc, s_dur$PC2_sc),
                   cor(s_dur$PC5_sc, s_dur$PC3_sc),
                   cor(s_dur$PC5_sc, s_dur$PC4_sc),
                   cor(s_dur$PC5_sc, s_dur$PC5_sc),
                   cor(s_dur$PC5_sc, s_dur$lex_neb_sc),
                   cor(s_dur$PC5_sc, s_dur$probability_prev_sc),
                   cor(s_dur$PC5_sc, s_dur$probability_next_sc),
                   cor(s_dur$PC5_sc, s_dur$log_ratio_s),
                   cor(s_dur$PC5_sc, s_dur$log_ratio_pl),
                   cor(s_dur$PC5_sc, s_dur$log_f_s),
                   cor(s_dur$PC5_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(lex_neb_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$lex_neb_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$lex_neb_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$lex_neb_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$lex_neb_sc, s_dur$PC1_sc),
                   cor(s_dur$lex_neb_sc, s_dur$PC2_sc),
                   cor(s_dur$lex_neb_sc, s_dur$PC3_sc),
                   cor(s_dur$lex_neb_sc, s_dur$PC4_sc),
                   cor(s_dur$lex_neb_sc, s_dur$PC5_sc),
                   cor(s_dur$lex_neb_sc, s_dur$lex_neb_sc),
                   cor(s_dur$lex_neb_sc, s_dur$probability_prev_sc),
                   cor(s_dur$lex_neb_sc, s_dur$probability_next_sc),
                   cor(s_dur$lex_neb_sc, s_dur$log_ratio_s),
                   cor(s_dur$lex_neb_sc, s_dur$log_ratio_pl),
                   cor(s_dur$lex_neb_sc, s_dur$log_f_s),
                   cor(s_dur$lex_neb_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(probability_prev_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_prev_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$probability_prev_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$probability_prev_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$probability_prev_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$probability_prev_sc, s_dur$PC1_sc),
                   cor(s_dur$probability_prev_sc, s_dur$PC2_sc),
                   cor(s_dur$probability_prev_sc, s_dur$PC3_sc),
                   cor(s_dur$probability_prev_sc, s_dur$PC4_sc),
                   cor(s_dur$probability_prev_sc, s_dur$PC5_sc),
                   cor(s_dur$probability_prev_sc, s_dur$lex_neb_sc),
                   cor(s_dur$probability_prev_sc, s_dur$probability_prev_sc),
                   cor(s_dur$probability_prev_sc, s_dur$probability_next_sc),
                   cor(s_dur$probability_prev_sc, s_dur$log_ratio_s),
                   cor(s_dur$probability_prev_sc, s_dur$log_ratio_pl),
                   cor(s_dur$probability_prev_sc, s_dur$log_f_s),
                   cor(s_dur$probability_prev_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(probability_next_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(probability_next_sc ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$probability_next_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$probability_next_sc, s_dur$num_syl_pron_sc),
                   cor(s_dur$probability_next_sc, s_dur$log_base_dur_sc),
                   cor(s_dur$probability_next_sc, s_dur$PC1_sc),
                   cor(s_dur$probability_next_sc, s_dur$PC2_sc),
                   cor(s_dur$probability_next_sc, s_dur$PC3_sc),
                   cor(s_dur$probability_next_sc, s_dur$PC4_sc),
                   cor(s_dur$probability_next_sc, s_dur$PC5_sc),
                   cor(s_dur$probability_next_sc, s_dur$lex_neb_sc),
                   cor(s_dur$probability_next_sc, s_dur$probability_prev_sc),
                   cor(s_dur$probability_next_sc, s_dur$probability_next_sc),
                   cor(s_dur$probability_next_sc, s_dur$log_ratio_s),
                   cor(s_dur$probability_next_sc, s_dur$log_ratio_pl),
                   cor(s_dur$probability_next_sc, s_dur$log_f_s),
                   cor(s_dur$probability_next_sc, s_dur$log_rel_f_s),
                   sqrt(summary(lm(log_ratio_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_s ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$log_ratio_s, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_ratio_s, s_dur$num_syl_pron_sc),
                   cor(s_dur$log_ratio_s, s_dur$log_base_dur_sc),
                   cor(s_dur$log_ratio_s, s_dur$PC1_sc),
                   cor(s_dur$log_ratio_s, s_dur$PC2_sc),
                   cor(s_dur$log_ratio_s, s_dur$PC3_sc),
                   cor(s_dur$log_ratio_s, s_dur$PC4_sc),
                   cor(s_dur$log_ratio_s, s_dur$PC5_sc),
                   cor(s_dur$log_ratio_s, s_dur$lex_neb_sc),
                   cor(s_dur$log_ratio_s, s_dur$probability_prev_sc),
                   cor(s_dur$log_ratio_s, s_dur$probability_next_sc),
                   cor(s_dur$log_ratio_s, s_dur$log_ratio_s),
                   cor(s_dur$log_ratio_s, s_dur$log_ratio_pl),
                   cor(s_dur$log_ratio_s, s_dur$log_f_s),
                   cor(s_dur$log_ratio_s, s_dur$log_rel_f_s),
                   sqrt(summary(lm(log_ratio_pl ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_ratio_pl ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$log_ratio_pl, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_ratio_pl, s_dur$num_syl_pron_sc),
                   cor(s_dur$log_ratio_pl, s_dur$log_base_dur_sc),
                   cor(s_dur$log_ratio_pl, s_dur$PC1_sc),
                   cor(s_dur$log_ratio_pl, s_dur$PC2_sc),
                   cor(s_dur$log_ratio_pl, s_dur$PC3_sc),
                   cor(s_dur$log_ratio_pl, s_dur$PC4_sc),
                   cor(s_dur$log_ratio_pl, s_dur$PC5_sc),
                   cor(s_dur$log_ratio_pl, s_dur$lex_neb_sc),
                   cor(s_dur$log_ratio_pl, s_dur$probability_prev_sc),
                   cor(s_dur$log_ratio_pl, s_dur$probability_next_sc),
                   cor(s_dur$log_ratio_pl, s_dur$log_ratio_s),
                   cor(s_dur$log_ratio_pl, s_dur$log_ratio_pl),
                   cor(s_dur$log_ratio_pl, s_dur$log_f_s),
                   cor(s_dur$log_ratio_pl, s_dur$log_rel_f_s),
                   sqrt(summary(lm(log_f_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_f_s ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$log_f_s, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_f_s, s_dur$num_syl_pron_sc),
                   cor(s_dur$log_f_s, s_dur$log_base_dur_sc),
                   cor(s_dur$log_f_s, s_dur$PC1_sc),
                   cor(s_dur$log_f_s, s_dur$PC2_sc),
                   cor(s_dur$log_f_s, s_dur$PC3_sc),
                   cor(s_dur$log_f_s, s_dur$PC4_sc),
                   cor(s_dur$log_f_s, s_dur$PC5_sc),
                   cor(s_dur$log_f_s, s_dur$lex_neb_sc),
                   cor(s_dur$log_f_s, s_dur$probability_prev_sc),
                   cor(s_dur$log_f_s, s_dur$probability_next_sc),
                   cor(s_dur$log_f_s, s_dur$log_ratio_s),
                   cor(s_dur$log_f_s, s_dur$log_ratio_pl),
                   cor(s_dur$log_f_s, s_dur$log_f_s),
                   cor(s_dur$log_f_s, s_dur$log_rel_f_s),
                   sqrt(summary(lm(log_rel_f_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ prev_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_rel_f_s ~ prev_mention, data = s_dur))$r.squared),
                   cor(s_dur$log_rel_f_s, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_rel_f_s, s_dur$num_syl_pron_sc),
                   cor(s_dur$log_rel_f_s, s_dur$log_base_dur_sc),
                   cor(s_dur$log_rel_f_s, s_dur$PC1_sc),
                   cor(s_dur$log_rel_f_s, s_dur$PC2_sc),
                   cor(s_dur$log_rel_f_s, s_dur$PC3_sc),
                   cor(s_dur$log_rel_f_s, s_dur$PC4_sc),
                   cor(s_dur$log_rel_f_s, s_dur$PC5_sc),
                   cor(s_dur$log_rel_f_s, s_dur$lex_neb_sc),
                   cor(s_dur$log_rel_f_s, s_dur$probability_prev_sc),
                   cor(s_dur$log_rel_f_s, s_dur$probability_next_sc),
                   cor(s_dur$log_rel_f_s, s_dur$log_ratio_s),
                   cor(s_dur$log_rel_f_s, s_dur$log_ratio_pl),
                   cor(s_dur$log_rel_f_s, s_dur$log_f_s),
                   cor(s_dur$log_rel_f_s, s_dur$log_rel_f_s)
                   ), 
                  nrow = 20, ncol = 20, byrow = T, dimnames = list(
#                    c("Next segment", "Previous segment", "Word stress", "Register", "Previously mentioned", "Speech rate", "Number of syllables", "log(dur(base))", ":Prosody[PC1]", ":Prosody[PC2]", ":Prosody[PC3]", ":Prosody[PC4]", ":Prosody[PC5]", "Lexical neighbours", ":'log(P(w'['n']*' | w'['n-1']*'))'", ":'log(P(w'['n']*' | w'['n+1']*'))'", "log(ratio(-s))", "log(ratio(PL))", "log(freq(-s))", "log(relfreq(-s))"),
                    c("Next segment", "Previous segment", "Word stress", "Register", "Recently mentioned", "Speech rate", "Number of syllables", "Base duration", ":Prosody[PC1]", ":Prosody[PC2]", ":Prosody[PC3]", ":Prosody[PC4]", ":Prosody[PC5]", "Phonological neighbours", "Probability from prev. word", "Probability from next word", "-s Bias", "Plural Dominance", "-s Frequency", "Relative -s Frequency"),
                    c("Next segment", "Previous segment", "Word stress", "Register", "Recently mentioned", "Speech rate", "Number of syllables", "Base duration", ":Prosody[PC1]", ":Prosody[PC2]", ":Prosody[PC3]", ":Prosody[PC4]", ":Prosody[PC5]", "Phonological neighbours", "Probability from prev. word", "Probability from next word", "-s Bias", "Plural Dominance", "-s Frequency", "Relative -s Frequency")
                    ))

#colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
#                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#pie(rep(1, 8), col = colorBlindBlack8)

plot_aodml_effect = function(var_data, var_mod, predictor_var, moderator_var, constant_vars, dependent_var, predictor_lab=NULL, moderator_lab=NULL, dependent_lab=NULL, moderator_values=NULL, backtransform=NULL){
  if (is.null(backtransform)){
    backtransform = T
  }
  if (!backtransform){
    dependent_var_orig = dependent_var
    dependent_var = paste(dependent_var, "logit", sep = "_")
  }
  if (is.null(predictor_lab)){
    predictor_lab = predictor_var
  }
  if (is.null(moderator_lab)){
    moderator_lab = moderator_var
  }
  if (is.null(dependent_lab)){
    dependent_lab = dependent_var
  }
  if (is.null(moderator_values)){
    moderator_values = "minmedmax"
  }
  
  backtransform_vec = rep(backtransform, nrow(var_data))
  var2 = var_data
  if(!backtransform){
    var_data[[dependent_var]] = log(var_data[[dependent_var_orig]]/(1 - var_data[[dependent_var_orig]]))
  }
  for (v in constant_vars){
    var2[[v]] = median(var_data[[v]])
  }
  if (moderator_values == "minmedmax" | moderator_values == "minmeanmax"){
    var2[[moderator_var]] = max(var_data[[moderator_var]])
    mod_max_lab = "Max"
  } else if (moderator_values == "0-0.5-1"){
    var2[[moderator_var]] = 1
    mod_max_lab = "1"
  } else if (moderator_values == "-4_0_4"){
    var2[[moderator_var]] = 4
    mod_max_lab = "4"
  } else if (moderator_values == "meansd"){
    var2[[moderator_var]] = mean(var_data[[moderator_var]]) + sd(var_data[[moderator_var]])
    mod_max_lab = "+SD"
  }
  betabin_pred = predict(var_mod, se.fit = T, newdata = var2)
  s_prop_pred_max = ifelse(backtransform_vec, plogis(betabin_pred$fit), betabin_pred$fit)
  s_prop_lo_max = ifelse(backtransform_vec, plogis(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit)
  s_prop_hi_max = ifelse(backtransform_vec, plogis(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit)

  if (moderator_values == "minmedmax"){
    var2[[moderator_var]] = median(var_data[[moderator_var]])
    mod_med_lab = "Median"
  } else if (moderator_values == "minmeanmax"){
    var2[[moderator_var]] = mean(var_data[[moderator_var]])
    mod_med_lab = "Mean"
  } else if (moderator_values == "0-0.5-1"){
    var2[[moderator_var]] = 0.5
    mod_med_lab = "0.5"
  } else if (moderator_values == "-4_0_4"){
    var2[[moderator_var]] = 0
    mod_med_lab = "0"
  } else if (moderator_values == "meansd"){
    var2[[moderator_var]] = mean(var_data[[moderator_var]])
    mod_med_lab = "Mean"
  }

  betabin_pred = predict(var_mod, se.fit = T, newdata = var2)
  s_prop_pred_med = ifelse(backtransform_vec, plogis(betabin_pred$fit), betabin_pred$fit)
  s_prop_lo_med = ifelse(backtransform_vec, plogis(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit)
  s_prop_hi_med = ifelse(backtransform_vec, plogis(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit)

  if (moderator_values == "minmedmax" | moderator_values == "minmeanmax"){
    var2[[moderator_var]] = min(var_data[[moderator_var]])
    mod_min_lab = "Min"
  } else if (moderator_values == "0-0.5-1"){
    var2[[moderator_var]] = 0
    mod_min_lab = "0"
  } else if (moderator_values == "-4_0_4"){
    var2[[moderator_var]] = -4
    mod_min_lab = "-4"
  } else if (moderator_values == "meansd"){
    var2[[moderator_var]] = mean(var_data[[moderator_var]]) - sd(var_data[[moderator_var]])
    mod_min_lab = "-SD"
  }

  betabin_pred = predict(var_mod, se.fit = T, newdata = var2)
  s_prop_pred_min = ifelse(backtransform_vec, plogis(betabin_pred$fit), betabin_pred$fit)
  s_prop_lo_min = ifelse(backtransform_vec, plogis(betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit - qnorm(.975) * betabin_pred$se.fit)
  s_prop_hi_min = ifelse(backtransform_vec, plogis(betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit), betabin_pred$fit + qnorm(.975) * betabin_pred$se.fit)

  # ggplot(var_data, aes(x = get(predictor_var), y = get(dependent_var))) +
  #        geom_point(color = "grey", alpha = 1) +
  #        geom_line(aes(y=s_prop_pred_max, linetype = mod_max_lab, color=mod_max_lab), size=1) +
  #        geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max, linetype = mod_max_lab, color=mod_max_lab), alpha = 0) +
  #        geom_line(aes(y=s_prop_pred_med, linetype = mod_med_lab, color=mod_med_lab), size=1) +
  #        geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med, linetype = mod_med_lab, color=mod_med_lab), alpha = 0) +
  #        geom_line(aes(y=s_prop_pred_min, linetype = mod_min_lab, color=mod_min_lab), size=1) +
  #        geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min, linetype = mod_min_lab, color=mod_min_lab), alpha = 0) +
  #        labs(x=predictor_lab, y=dependent_lab) +
  #        scale_colour_manual(name=moderator_lab, values=c("#000000", "#0072B2", "#D55E00")) + 
  #        scale_linetype_manual(name=moderator_lab, values=c("solid", "dotted", "dashed"))
  ggplot(var_data, aes(x = get(predictor_var), y = get(dependent_var))) +
    geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max, color=mod_max_lab, fill=mod_max_lab), alpha = 1) +
    geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med, color=mod_med_lab, fill=mod_med_lab), alpha = 1) +
    geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min, color=mod_min_lab, fill=mod_min_lab), alpha = 1) +
#    geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max, color=mod_max_lab, linetype = mod_max_lab), alpha = 0) +
#    geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med, color=mod_med_lab, linetype = mod_med_lab), alpha = 0) +
#    geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min, color=mod_min_lab, linetype = mod_min_lab), alpha = 0) +
    geom_ribbon( aes(ymin = s_prop_lo_min, ymax = s_prop_hi_min, color=mod_min_lab), alpha = 0) +
    geom_ribbon( aes(ymin = s_prop_lo_med, ymax = s_prop_hi_med, color=mod_med_lab), alpha = 0) +
    geom_ribbon( aes(ymin = s_prop_lo_max, ymax = s_prop_hi_max, color=mod_max_lab), alpha = 0) +
    geom_point(color = "grey", alpha = 1, size=0.5) +
    geom_line(aes(y=s_prop_pred_max, linetype = mod_max_lab)) +
    geom_line(aes(y=s_prop_pred_med, linetype = mod_med_lab)) +
    geom_line(aes(y=s_prop_pred_min, linetype = mod_min_lab)) +
    labs(x=predictor_lab, y=dependent_lab) +
    scale_colour_manual(name=moderator_lab, values=c("#009E73", "#0072B2", "#D55E00")) + 
    scale_linetype_manual(name=moderator_lab, values=c("solid", "dotted", "dashed")) +
    scale_fill_manual(name=moderator_lab, values=c("#009E73", "#0072B2", "#D55E00")) +
    theme(legend.position="top")
}

library(aods3)
library(hnp)

distribution_model = aodml(cbind(f_s, f_nons) ~ p_s*log_freq_pl + p_s*log_ratio_pl, var)

summary(distribution_model)
wald.test(b = coef(distribution_model), varb = vcov(distribution_model), Terms = 6)


plot_aodml_effect(var, distribution_model, predictor_var = "p_s", moderator_var = "log_ratio_pl", 
                  constant_vars = c("log_freq_pl"), dependent_var = "prop_s", 
                  predictor_lab = "-s Prediction", 
                  moderator_lab = "Plural Dominance", 
                  dependent_lab = "-s Bias", moderator_values = "-4_0_4",
                  backtransform = F)
ggsave(file="~/OneDrive/PhD_Nijmegen/dist_eff.eps", width = 8.5, height = 10, units = "cm")


plot_aodml_effect(var, distribution_model, predictor_var = "p_s", moderator_var = "log_freq_pl", 
                  constant_vars = c("log_ratio_pl"), dependent_var = "prop_s", 
                  predictor_lab = "-s Prediction", 
                  moderator_lab = "Plural Frequency", 
                  dependent_lab = "-s Bias", moderator_values = "minmeanmax",
                  backtransform = F)
ggsave(file="~/OneDrive/PhD_Nijmegen/dist_eff2.eps", width = 8.5, height = 10, units = "cm")

distribution_model_bin = glm(cbind(f_s, f_nons) ~ p_s*log_freq_pl + p_s*log_ratio_pl, family = "binomial", data = var)
summary(distribution_model_bin)

lrt_X = -2 * (as.numeric(logLik(distribution_model_bin))-as.numeric(logLik(distribution_model)))
lrt_p = pchisq(lrt_X, df = 1, lower.tail = FALSE)
print(paste("X(1)=", as.character(lrt_X), ", p=", as.character(lrt_p), sep = ""))

distribution_model_glmer = glmer(cbind(f_s, f_nons) ~ p_s*log_freq_pl + p_s*log_ratio_pl + (1|word), family = "binomial", data = var, control = glmerControl(optimizer = "bobyqa"))
summary(distribution_model_glmer)

lrt_X = -2 * (as.numeric(logLik(distribution_model))-as.numeric(logLik(distribution_model_glmer)))
lrt_p = pchisq(lrt_X, df = 1, lower.tail = FALSE)
print(paste("X(1)=", as.character(lrt_X), ", p=", as.character(lrt_p), sep = ""))

setEPS()
postscript("~/OneDrive/PhD_Nijmegen/dist_hnp.eps", width = 7.08661, height = 4)
par(mfrow=c(1,2))
hnp(distribution_model_bin, how.many.out = T, main="Binomial")
hnp(distribution_model, how.many.out = T, main="Beta-binomial")
par(mfrow=c(1,1))
dev.off()

library(lmerTest)
library(corrplot)

duration_model_cov = lmer(log_s_dur_kal ~ 
                            speech_rate_pron_sc + 
                            log_base_dur_sc +
                            PC1_sc + PC2_sc + PC3_sc + PC4_sc + PC5_sc +
                            next_phon_class +
                            prev_phon_class + stressed + num_syl_pron_sc + lex_neb_sc +
                            probability_prev_sc + probability_next_sc +
                            prev_mention + 
                            register +
                            (1 | word) +
                            (1 | speaker), 
                          REML = T,
                          data = s_dur)

duration_model_abs = update(duration_model_cov, . ~ . + log_f_s)
duration_model_rel = update(duration_model_cov, . ~ . + log_rel_f_s)
duration_model_ratio = update(duration_model_cov, . ~ . + log_ratio_s*log_ratio_pl)

s_dur$dur_resid = resid(duration_model_abs)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]
duration_model_abs_trim = lmer(duration_model_abs@call$formula, REML = T, data = s_dur_trim)                   
summary(duration_model_abs_trim)
s_dur$dur_resid = resid(duration_model_rel)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]
duration_model_rel_trim = lmer(duration_model_rel@call$formula, REML = T, data = s_dur_trim)
summary(duration_model_rel_trim)
s_dur$dur_resid = resid(duration_model_ratio)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]
duration_model_ratio_trim = lmer(duration_model_ratio@call$formula, REML = T, data = s_dur_trim)
summary(duration_model_ratio_trim)

AICs = c(AIC(refitML(duration_model_abs)), 
         AIC(refitML(duration_model_rel)),
         AIC(refitML(duration_model_ratio))
         )
mod_labs = c("log_f_s", "log_rel_f_s", "log_ratio_s * log_ratio_pl")

plot(1:length(AICs), AICs, xlab = "", ylab = "", xaxt='n', type = "h")
text(1:length(AICs), par("usr")[3], labels = mod_labs, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.65)
abline(h = AIC(refitML(duration_model_cov)), lty = "dashed")

AIC(refitML(duration_model_ratio))
AIC(refitML(duration_model_abs))
exp((AIC(refitML(duration_model_ratio)) - AIC(refitML(duration_model_abs)))/2)

setEPS()
postscript("~/OneDrive/PhD_Nijmegen/dur_cor.eps", width = 7.08661, height = 7.5)
corrplot(pred_ass, method = "number", type = "lower", tl.cex = 0.8, number.cex = 0.5, mar=c(0, 0, 0.8, 0))
dev.off()
# remove due to correlation with log_ratio_s: stressed, num_syl_pron

duration_model_full = lmer(log_s_dur_kal ~ 
                             speech_rate_pron_sc + 
                             log_base_dur_sc + 
                             PC1_sc + PC2_sc + PC3_sc + PC4_sc + PC5_sc + 
                             prev_phon_class + next_phon_class + lex_neb_sc +
                             probability_prev_sc + probability_next_sc +
                             prev_mention + register + 
                            log_ratio_s*log_ratio_pl +
                             (1 | word) + 
                             (1 | speaker), 
                           REML = T,
                           data = s_dur)

# see https://www.rdocumentation.org/packages/lmerTest/versions/2.0-36/topics/step for description
backward_elim = step(duration_model_full#, reduce.random=F
                     )
backward_elim

duration_model_ratio_small = lmer(log_s_dur_kal ~ 
                            speech_rate_pron_sc + 
                            PC2_sc + 
                            next_phon_class + 
                            register +
                            log_ratio_s*log_ratio_pl +
#                            (1 | word) +
                            (1 | speaker), 
                          REML = T,
                          data = s_dur)

s_dur$dur_resid = resid(duration_model_ratio_small)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]

duration_model_ratio_small_trim = lmer(duration_model_ratio_small@call$formula, 
                                 REML = T,
                                 data = s_dur_trim)

summary(duration_model_ratio_small_trim)

qqnorm(resid(duration_model_ratio_small_trim))
qqline(resid(duration_model_ratio_small_trim), col="red")

library(sjPlot)

set_theme(legend.title.face = "plain", legend.pos="top", legend.size = 0.8, axis.title.size = 1, axis.title.color = "black", axis.textsize = 0.9)
pm = plot_model(duration_model_ratio_small_trim, type = "eff", terms = c("log_ratio_s", "log_ratio_pl[-4, 0, 4]"), color="bw", legend.title = "Plural Dominance", show.legend = T, title = "", axis.title = c("-s Bias", "-s Duration"))
pm$guides$colour = "legend"
pm$guides$fill = "legend"
pm$labels$title = NULL
pm + geom_ribbon(aes(ymin=pm$data$conf.low, ymax=pm$data$conf.high), alpha = 1, linetype=0, show.legend = T) + 
  geom_ribbon( aes(ymin=pm$data$conf.low, ymax=pm$data$conf.high), alpha = 0, linetype="solid", show.legend = T) + 
  geom_point(data = s_dur_trim, mapping = aes(x = log_ratio_s, y = log_s_dur_kal), inherit.aes = FALSE, size=0.5, color="grey") +
  geom_line(aes(y=pm$data$predicted, x=pm$data$x), color="black", show.legend = T) +
  scale_colour_manual(name="Plural Dominance", values=c("#009E73", "#0072B2", "#D55E00")) + 
  scale_fill_manual(name="Plural Dominance", values=c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual(name="Plural Dominance", values=c("solid", "dotted", "dashed"))
ggsave(file="~/OneDrive/PhD_Nijmegen/dur_eff.eps", width = 8.5, height = 10, units = "cm")


library(interactions)
jn = johnson_neyman(duration_model_ratio_small_trim, pred = log_ratio_s, modx = log_ratio_pl, plot = T, control.fdr = T)
jn$bounds
jn$plot$theme$legend.position = "top"
jn$plot$layers[[4]]$aes_params$alpha = 1
jn$plot$layers[[5]]$aes_params$alpha = 1
jn$plot$layers[[6]]$aes_params$alpha = 1
jn$plot$layers[[9]]$aes_params$colour = "black"
jn$plot$layers[[10]]$aes_params$colour = "black"
jn$plot + xlab("Plural Dominance") + ylab("Slope of -s Bias") + theme(text = element_text(size = 10), legend.text = element_text(size = 10), legend.key.width = unit(0.5, 'cm')) +
  geom_line(aes(y=jn$plot$layers[[1]]$data$`Slope of log_ratio_s`, x=jn$plot$layers[[1]]$data$log_ratio_pl), color="black") +
  geom_line(aes(y=jn$plot$layers[[2]]$data$`Slope of log_ratio_s`, x=jn$plot$layers[[2]]$data$log_ratio_pl), color="black") +
  geom_line(aes(y=jn$plot$layers[[3]]$data$`Slope of log_ratio_s`, x=jn$plot$layers[[3]]$data$log_ratio_pl), color="black")
ggsave(file="~/OneDrive/PhD_Nijmegen/dur_jn.eps", width = 8.5, height = 10, units = "cm")

# why reduction effect of log_ratio_s at low log_ratio_pl
# perhaps because I didn't include correlated covariates?
# no because same interaction in full model

# show that a full model still works

summary(duration_model_ratio_trim)

plot_model(duration_model_ratio_trim, type = "eff", terms = c("log_ratio_s", "log_ratio_pl[-4, 0, 4]"), colors = "bw", legend.title = "log(ratio(PL))", title = "", axis.title = c("log(ratio(-s))", "log(duration(-s))"))

# alternative explanation
get_p_s_var = function(lem) {
  if (lem == "hersens"){
    lem = "hersen"
  }
  if (lem %in% levels(var$word)){
    return(var[var$word == lem,]$p_s)
  } else {
    return(NA)
  }
}

s_dur$p_s = as.numeric(sapply(as.character(s_dur$lemma), get_p_s_var))

s_dur$pred_prop_s = plogis(predict(distribution_model, newdata = s_dur))
s_dur$pred_log_ratio_s = log(s_dur$pred_prop_s / (1 - s_dur$pred_prop_s))
s_dur$resid_log_ratio_s = s_dur$pred_log_ratio_s - s_dur$log_ratio_s


duration_model_residREML = lmer(log_s_dur_kal ~ 
                                  speech_rate_pron_sc +
                                  PC2_sc + 
                                  next_phon_class +
                                  register +
                                  p_s + resid_log_ratio_s*log_ratio_pl +
                                  (1 | speaker), 
                                REML = T,
                                data = s_dur)

s_dur$dur_resid = resid(duration_model_residREML)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]

duration_model_resid_trim = lmer(log_s_dur_kal ~ 
                                   speech_rate_pron_sc +
                                   PC2_sc + 
                                   next_phon_class +
                                   register +
                                   p_s + resid_log_ratio_s*log_ratio_pl +
                                   (1 | speaker), 
                                 REML = T,
                                 data = s_dur_trim)

plot_model(duration_model_resid_trim, type = "eff", terms = c("p_s"), colors = "bw", title = "", axis.title = c(expression("Probability"["TiMBL"]*"(-s)", "log(duration(-s))")))

plot_model(duration_model_resid_trim, type = "eff", terms = c("resid_log_ratio_s", "log_ratio_pl[-4, 0, 4]"), colors = "bw", legend.title = "log(ratio(PL))", title = "", axis.title = c("resid(log(ratio(-s))", "log(duration(-s))"))

plot(predict(duration_model_resid_trim), resid(duration_model_resid_trim))

# random forest
# library(party)
# 
# s_dur.forest = cforest(log_s_dur_kal ~ 
#                          speech_rate_pron_sc + num_syl_pron_sc + log_base_dur_sc +
#                          PC1_sc + PC2_sc + PC3_sc + PC4_sc + PC5_sc + stressed + next_phon_class +
#                          prev_phon_class + lex_neb_sc + probability_prev_sc +
#                          probability_next_sc + prev_mention + register + 
#                          log_f_s + entropy + relative_entropy + log_ratio_s*log_ratio_pl
#                        , data = s_dur, controls = cforest_unbiased(ntree = 500, mtry = 3))
# variable_importance = varimp(s_dur.forest)
# 
# par(mar=c(4, 8, 2, 2))
# bp = barplot(sort(variable_importance), horiz = TRUE, las = 2)
# abline(v = abs(min(variable_importance)), lty = 2, col = "red")
# par(mar=c(2, 2, 2, 2))

# why is ratio better?
# more normal residuals?
par(mfrow = c(1,2))
qqnorm(resid(duration_model_ratio), main = "Ratio")
qqline(resid(duration_model_ratio), col="red")
qqnorm(resid(duration_model_prop), main = "Proportion")
qqline(resid(duration_model_prop), col="red")

par(mfrow = c(1,4))
frequencies_a = c(2, 10, 20, 30, 40, 50, 60, 70, 80, 90, 98)
log_frequencies_a = log(frequencies_a)
log_frequencies_b = log(100 - frequencies_a)
ratios = frequencies_a/(100 - frequencies_a)
log_ratios = log(ratios)
proportions = frequencies_a / 100
log_proportions = log(proportions)
ratio_logs = log_frequencies_a / log_frequencies_b
plot(x=frequencies_a, y=log_ratios, main = "log(ratio(A))", xlab = "frequency variant A", type="b")
plot(x=frequencies_a, y=proportions, main = "Proportion(A)", xlab = "frequency variant A", type="b")
plot(x=frequencies_a, y=log_proportions, main = "log(Proportion(A))", xlab = "frequency variant A", type="b")
#plot(x=frequencies_a, y=ratios, main = "ratio(A)", xlab = "frequency variant A", type="b")
#plot(x=frequencies_a, y=ratio_logs, main = "", xlab = "frequency variant A", type="b")

#plot(x=frequencies_a, y=log_frequencies_a / log_frequencies_b, main = "100 plurals in total", xlab = "frequency variant A", type="both")

s_dur_trim$pred_prop_s = plogis(predict(distribution_model, newdata = s_dur_trim))
s_dur$resid_prop_s = s_dur$pred_prop_s - s_dur$prop_s
s_dur_trim$pred_log_ratio_s = log(s_dur_trim$pred_prop_s / (1 - s_dur_trim$pred_prop_s))
s_dur_trim$resid_log_ratio_s = s_dur_trim$pred_log_ratio_s - s_dur_trim$log_ratio_s

duration_model_resid1 = update(duration_model_cov, . ~ . + resid_prop_s*prop_pl + p_s)
duration_model_resid2 = update(duration_model_cov_trim, . ~ . + resid_log_ratio_s*log_ratio_pl + p_s)



s_dur$dur_resid = resid(duration_model)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]

duration_model_trim = lmer(log_s_dur ~ speech_rate_pron_sc +
                             PC1_sc + PC2_sc + PC3_sc +
                             next_phon_class +
                             register +
                             prop_s*prop_pl +
                             (1 | speaker) + (1 | word),
                           data = s_dur_trim)

library(sjPlot)
plot_model(duration_model_trim, type = "eff", terms = c("prop_s", "prop_pl[0, 0.5, 1]"), colors = "bw", legend.title = "Proportion(PL)", title = "", axis.title = c("Proportion(-s)", "log(duration(-s))"))

```

The plot above shows that we do find *paradigmatic enhancement* but only if *Proportion(PL)* is high. This is in line with our hypothesis. However, the plot also seems to show a *reduction* effect when *Proportion(PL)* is low. 

## Double checking the interaction
Let's find out whether either the apparent paradigmatic enhancement or the reduction is due to collinear predictors or non-linear relations between the variables. 

First, let's see if either the reduction or the enhancement effect is only apparent by checking whether quadratic predictors improve the model:

```{r message=FALSE, warning=FALSE}
duration_model_quad = lmer(log_s_dur_kal ~ speech_rate_pron_sc + 
                             PC1_sc + PC2_sc +
                             next_phon_class +
                             register +
                             poly(log_ratio_s,2)*poly(prop_pl,2) +
                             (1 | speaker),
                           data = s_dur_trim)
kable(as.matrix(summary(duration_model_quad)$coefficients), caption = "Coefficients")
```

As you can see none of the possible combinations of quadratic predictors improve the model. Only `poly(log_ratio_s, 2)1:poly(prop_pl, 2)1`, which represents the interaction in which both predictors are linear, is significant.

Now, let's check the associations between all variables in the model:

```{r message=FALSE, warning=FALSE}
library(rcompanion)
library(corrplot)
pred_ass = matrix(c(cramerV(table(s_dur[,c("next_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "register")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_pl ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_s_dur ~ next_phon_class, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("register", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "register")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_s ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_pl ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_s_dur ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron_sc ~ register, data = s_dur))$r.squared),
                   cor(s_dur$speech_rate_pron_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC1_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC2_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$PC3_sc),
                   cor(s_dur$speech_rate_pron_sc, s_dur$prop_s),
                   cor(s_dur$speech_rate_pron_sc, s_dur$prop_pl),
                   cor(s_dur$speech_rate_pron_sc, s_dur$log_s_dur),
                   sqrt(summary(lm(PC1_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC1_sc ~ register, data = s_dur))$r.squared),
                   cor(s_dur$PC1_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC1_sc, s_dur$PC1_sc),
                   cor(s_dur$PC1_sc, s_dur$PC2_sc),
                   cor(s_dur$PC1_sc, s_dur$PC3_sc),
                   cor(s_dur$PC1_sc, s_dur$prop_s),
                   cor(s_dur$PC1_sc, s_dur$prop_pl),
                   cor(s_dur$PC1_sc, s_dur$log_s_dur),
                   sqrt(summary(lm(PC2_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC2_sc ~ register, data = s_dur))$r.squared),
                   cor(s_dur$PC2_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC2_sc, s_dur$PC1_sc),
                   cor(s_dur$PC2_sc, s_dur$PC2_sc),
                   cor(s_dur$PC2_sc, s_dur$PC3_sc),
                   cor(s_dur$PC2_sc, s_dur$prop_s),
                   cor(s_dur$PC2_sc, s_dur$prop_pl),
                   cor(s_dur$PC2_sc, s_dur$log_s_dur),
                   sqrt(summary(lm(PC3_sc ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(PC3_sc ~ register, data = s_dur))$r.squared),
                   cor(s_dur$PC3_sc, s_dur$speech_rate_pron_sc),
                   cor(s_dur$PC3_sc, s_dur$PC1_sc),
                   cor(s_dur$PC3_sc, s_dur$PC2_sc),
                   cor(s_dur$PC3_sc, s_dur$PC3_sc),
                   cor(s_dur$PC3_sc, s_dur$prop_s),
                   cor(s_dur$PC3_sc, s_dur$prop_pl),
                   cor(s_dur$PC3_sc, s_dur$log_s_dur),
                   sqrt(summary(lm(prop_s ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_s ~ register, data = s_dur))$r.squared),
                   cor(s_dur$prop_s, s_dur$speech_rate_pron_sc),
                   cor(s_dur$prop_s, s_dur$PC1_sc),
                   cor(s_dur$prop_s, s_dur$PC2_sc),
                   cor(s_dur$prop_s, s_dur$PC3_sc),
                   cor(s_dur$prop_s, s_dur$prop_s),
                   cor(s_dur$prop_s, s_dur$prop_pl),
                   cor(s_dur$prop_s, s_dur$log_s_dur),
                   sqrt(summary(lm(prop_pl ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(prop_pl ~ register, data = s_dur))$r.squared),
                   cor(s_dur$prop_pl, s_dur$speech_rate_pron_sc),
                   cor(s_dur$prop_pl, s_dur$PC1_sc),
                   cor(s_dur$prop_pl, s_dur$PC2_sc),
                   cor(s_dur$prop_pl, s_dur$PC3_sc),
                   cor(s_dur$prop_pl, s_dur$prop_s),
                   cor(s_dur$prop_pl, s_dur$prop_pl),
                   cor(s_dur$prop_pl, s_dur$log_s_dur),
                   sqrt(summary(lm(log_s_dur ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_s_dur ~ register, data = s_dur))$r.squared),
                   cor(s_dur$log_s_dur, s_dur$speech_rate_pron_sc),
                   cor(s_dur$log_s_dur, s_dur$PC1_sc),
                   cor(s_dur$log_s_dur, s_dur$PC2_sc),
                   cor(s_dur$log_s_dur, s_dur$PC3_sc),
                   cor(s_dur$log_s_dur, s_dur$prop_s),
                   cor(s_dur$log_s_dur, s_dur$prop_pl),
                   cor(s_dur$log_s_dur, s_dur$log_s_dur)
                   ), 
                  nrow = 9, ncol = 9, byrow = T, dimnames = list(
                    c("Next Phonetic Class", "Register", "Speech Rate", "Prosody 1", 
                      "Prosody 2", "Prosody 3", "Proportion(-s)", "Proportion(PL)", "log(duration(-s))"),
                    c("Next Phonetic Class", "Register", "Speech Rate", "Prosody 1", 
                      "Prosody 2", "Prosody 3", "Proportion(-s)", "Proportion(PL)", "log(duration(-s))")))
corrplot(pred_ass, method = "number")
```

As you can see, the predictors of interest and the covariates aren't very correlated. However, some of the covariates are rather strongly associated with our dependent variable. In order to get a better idea of which data points support the interaction between *Proportion(-s)* and *Proportion(PL)* let's residualize on the covariates first, and then inspect our interaction effect.

```{r message=FALSE, warning=FALSE}
duration_model_cov = lmer(log_s_dur ~ speech_rate_pron_sc +
                            PC1_sc + PC2_sc + PC3_sc +
                            next_phon_class +
                            register +
                            (1 | speaker) + (1 | word),
                          data = s_dur)
s_dur$resid_dur = resid(duration_model_cov) 

s_dur$prop_pl_groups = factor(cut(s_dur$prop_pl, breaks = 3), labels = c("small", "average", "large"))

ggplot(s_dur, aes(x = prop_s, y = resid_dur, color = prop_pl_groups)) +
  geom_point(size = .9, alpha = .3) +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  labs(x = "Proportion(-s)", y = "residual(duration(-s))", color = "Proportion(PL)") +
  ylim(-0.5, 0.5)

```

From the plot above it becomes obvious that the residuals still contain quite a lot of variance that is not explained by our interaction of interest. As a result, it is hard to see whether either the reduction or the enhancement effect is not supported by the data. Using the `interactions` package, we can explore this more formally by finding the Johnson-Neyman interval.

```{r message=FALSE, warning=FALSE}
library(interactions)

jn = johnson_neyman(duration_model_ratio_trim, pred = log_ratio_s, modx = log_ratio_pl, plot = T)
jn$bounds
jn$plot + xlab("log(ratio((PL))") + ylab("Slope of log(ratio(-s))")
```

This tells us that the effect of *Proportion(-s)* is significant if *Proportion(PL)* is either below 0.31 or above 0.87. In other words, the significant interaction reflects both a reduction and an enhancement effect. So what is the explanation for the reduction effect?

## Secondary Analysis

First of all, we should remember from our distributional study that what *Proportion(-s)* represents depends on the value of *Proportion(PL)*. At high *Proportion(PL)*, it might be a measure of paradigmatic competition, but at low *Proportion(PL)*, it might represent the amount of phonological support from similar paradigms. Could phonological support result in phonetic reduction? Previous research on phonological neighbourhood size suggests that this might be the case (Gahl, Yao, Johnson, 2012). Is there some way to investigate whether the durational reduction in our data is due to phonological support? We can't include *Probability(-s)* and *Proportion(-s)* in the same model, as the two measures are strongly correlated. But we can include *Probability(-s)* with the residuals of *Proportion(-s)* from the distributional model. We can make a number of predictions if we assume that the reduction effect is due to increased phonological support.

* *Probability(-s)* should have a negative effect on *Duration(-s)*
* *Resid(Proportion(-s))* should interact with *Proportion(PL)*:
    + At low *Proportion(PL)*, 
        - Negative *Resid(Proportion(-s))* represents underestimated phonological support $\rightarrow$ shorter duration
        - Positive *Resid(Proportion(-s))* represents overestimated phonological support $\rightarrow$ longer duration
    + At high *Proportion(PL)*,
        - Negative *Resid(Proportion(-s))* represents underestimated representational strength $\rightarrow$ longer duration
        - Positive *Resid(Proportion(-s))* represents overestimated representational strength $\rightarrow$ shorter duration
    
Let's see if these predictions are borne out:
```{r message=FALSE, warning=FALSE}
s_dur$pred_prop_s = plogis(predict(distribution_model, newdata = s_dur))
s_dur$resid_prop_s = s_dur$pred_prop_s - s_dur$prop_s

duration_model2 = lmer(log_s_dur ~ speech_rate_pron_sc +
                         PC1_sc + PC2_sc + PC3_sc +
                         next_phon_class +
                         register +
                         p_s +
                         resid_prop_s*prop_pl +
                         (1 | speaker) + (1 | word),
                       data = s_dur)

s_dur$dur_resid = resid(duration_model2)
s_dur_trim = s_dur[abs(scale(s_dur$dur_resid)) < 2.5,]

duration_model2_trim = lmer(log_s_dur ~ speech_rate_pron_sc +
                              PC1_sc + PC2_sc + PC3_sc +
                              next_phon_class +
                              register +
                              p_s +
                              resid_prop_s*prop_pl +
                              (1 | speaker) + (1 | word),
                           data = s_dur_trim)

kable(as.matrix(summary(duration_model2_trim)$coefficients), caption = "Coefficients")
```

The negative coefficient for `p_s` shows us that increased *Probability(-s)* does indeed have a reduction effect. Now, let's explore the interaction between *Resid(Proportion(-s))* and *Proportion(PL)*:

```{r message=FALSE, warning=FALSE}
plot_model(duration_model2_trim, type = "eff", terms = c("resid_prop_s", "prop_pl[0, 0.5, 1]"), colors = "bw", legend.title = "Proportion(PL)", title = "", axis.title = c("Resid(Proportion(-s))", "log(duration(-s))"))
```

The cross-over interaction we see here is consistent with an account in which the residuals of the distributional model represent different aspects of variable plural production, depending on the value of *Proportion(PL)*. At low *Proportion(PL)*, the residuals probably represent the errors in the phonological predictions (represented by the *Probability(-s)* variable). At high *Proportion(PL)*, the residuals represent the unexplained variance in *Proportion(-s)* due to the variation being stored.

## References

* Cohen, C. (2015). Context and paradigms: Two patterns of probabilistic pronunciation variation in Russian agreement suffixes. *The Mental Lexicon, 10*(3), 313-338.
* Gahl, S., Yao, Y., & Johnson, K. (2012). Why reduce? Phonological neighborhood density and phonetic reduction in spontaneous speech. *Journal of memory and language, 66*(4), 789-806.
