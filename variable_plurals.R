library(languageR)
library(corrplot)
library(car)
library(rcompanion)
#library(lme4)
library(lmerTest)
library(effects)
library(gridExtra)
library(sjPlot)
library(ggplot2)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/timbl_files/"
  other_path = "/Volumes/tensusers/timzee/other/"
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
  ifadv_path = "/Volumes/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/Volumes/tensusers/timzee/ECSD/"
} else {
  f_path = "/vol/tensusers/timzee/timbl_files/"
  other_path = "/vol/tensusers/timzee/other/"
  cgn_path = "/vol/tensusers/timzee/cgn/"
  ifadv_path = "/vol/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/vol/tensusers/timzee/ECSD/"
}

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

get_prev_prev_phon = function(x) {
  phon_list = unlist(strsplit(as.character(x), " "))
  if (length(phon_list) > 2) {
    return(phon_list[length(phon_list) - 2])
  } else {
    return("")
  }
}

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

#s_dur_a = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-a2.csv", sep = ""))
s_dur_a = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-a_timbl2.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
s_dur_a$register = as.factor("conversation")
s_dur_a$mean_hnr = as.factor(s_dur_a$mean_hnr)
s_dur_a$nn_end_score = as.factor(s_dur_a$nn_end_score)
s_dur_a = s_dur_a[s_dur_a$overlap == FALSE,]
s_dur_a = s_dur_a[ , !(names(s_dur_a) %in% c("overlap"))]
s_dur_a$language = as.factor("nl")
#s_dur_a_vl = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-a_vl.csv", sep = ""))
#s_dur_a_vl$corpus = as.factor("cgn-a")
#s_dur_a_vl$register = as.factor("conversation")
#s_dur_a_vl$mean_hnr = as.factor(s_dur_a_vl$mean_hnr)
#s_dur_a_vl$nn_end_score = as.factor(s_dur_a_vl$nn_end_score)
#s_dur_a_vl = s_dur_a_vl[s_dur_a_vl$overlap == FALSE,]
#s_dur_a_vl = s_dur_a_vl[ , !(names(s_dur_a_vl) %in% c("overlap"))]
#s_dur_a_vl$language = as.factor("vl")
#s_dur_c = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-c2.csv", sep = ""))
s_dur_c = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-c_timbl2.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$register = as.factor("conversation")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
s_dur_c$mean_hnr = as.factor(s_dur_c$mean_hnr)
s_dur_c$nn_end_score = as.factor(s_dur_c$nn_end_score)
s_dur_c = s_dur_c[s_dur_c$overlap == FALSE,]
s_dur_c = s_dur_c[ , !(names(s_dur_c) %in% c("overlap"))]
s_dur_c$language = as.factor("nl")
#s_dur_c_vl = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-c_vl.csv", sep = ""))
#s_dur_c_vl$corpus = as.factor("cgn-c")
#s_dur_c_vl$register = as.factor("conversation")
#s_dur_c_vl$mean_hnr = as.factor(s_dur_c_vl$mean_hnr)
#s_dur_c_vl$nn_end_score = as.factor(s_dur_c_vl$nn_end_score)
#s_dur_c_vl = s_dur_c_vl[s_dur_c_vl$overlap == FALSE,]
#s_dur_c_vl = s_dur_c_vl[ , !(names(s_dur_c_vl) %in% c("overlap"))]
#s_dur_c_vl$language = as.factor("vl")
#s_dur_d = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-d2.csv", sep = ""))
s_dur_d = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-d_timbl2.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
s_dur_d$register = as.factor("conversation")
s_dur_d$mean_hnr = as.factor(s_dur_d$mean_hnr)
s_dur_d$nn_end_score = as.factor(s_dur_d$nn_end_score)
s_dur_d = s_dur_d[s_dur_d$overlap == FALSE,]
s_dur_d = s_dur_d[ , !(names(s_dur_d) %in% c("overlap"))]
s_dur_d$language = as.factor("nl")
#s_dur_d_vl = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-d_vl.csv", sep = ""))
#s_dur_d_vl$corpus = as.factor("cgn-d")
#s_dur_d_vl$register = as.factor("conversation")
#s_dur_d_vl$mean_hnr = as.factor(s_dur_d_vl$mean_hnr)
#s_dur_d_vl$nn_end_score = as.factor(s_dur_d_vl$nn_end_score)
#s_dur_d_vl = s_dur_d_vl[s_dur_d_vl$overlap == FALSE,]
#s_dur_d_vl = s_dur_d_vl[ , !(names(s_dur_d_vl) %in% c("overlap"))]
#s_dur_d_vl$language = as.factor("vl")
#s_dur_ifadv = read.csv(paste(ifadv_path, "synvoirelPL_s_comb_ifadv.csv", sep = ""))
s_dur_ifadv = read.csv(paste(ifadv_path, "synvoirelPL_s_comb_ifadv_timbl2.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$register = as.factor("conversation")
s_dur_ifadv$mean_hnr = as.factor(s_dur_ifadv$mean_hnr)
s_dur_ifadv$nn_end_score = as.factor(s_dur_ifadv$nn_end_score)
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
s_dur_ifadv$language = as.factor("nl")
#s_dur_ecsd = read.csv(paste(ecsd_path, "synvoirelPL_s_comb_ecsd.csv", sep = ""))
s_dur_ecsd = read.csv(paste(ecsd_path, "synvoirelPL_s_comb_ecsd_timbl2.csv", sep = ""))
s_dur_ecsd$corpus = as.factor("ecsd")
s_dur_ecsd$register = as.factor("conversation")
s_dur_ecsd$mean_hnr = as.factor(s_dur_ecsd$mean_hnr)
s_dur_ecsd$nn_end_score = as.factor(s_dur_ecsd$nn_end_score)
s_dur_ecsd$language = as.factor("nl")

#s_dur_k = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-k.csv", sep = ""))
s_dur_k = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-k_timbl2.csv", sep = ""))
s_dur_k$corpus = as.factor("cgn-k")
s_dur_k$register = as.factor("news")
s_dur_k$mean_hnr = as.factor(s_dur_k$mean_hnr)
s_dur_k$nn_end_score = as.factor(s_dur_k$nn_end_score)
s_dur_k$language = as.factor("nl")
#s_dur_k_vl = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-k_vl.csv", sep = ""))
#s_dur_k_vl$corpus = as.factor("cgn-k")
#s_dur_k_vl$register = as.factor("news")
#s_dur_k_vl$mean_hnr = as.factor(s_dur_k_vl$mean_hnr)
#s_dur_k_vl$nn_end_score = as.factor(s_dur_k_vl$nn_end_score)
#s_dur_k_vl$language = as.factor("vl")
#s_dur_k_vl = s_dur_k_vl[ , !(names(s_dur_k_vl) %in% c("overlap"))]
#s_dur_o = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-o.csv", sep = ""))
s_dur_o = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-o_timbl2.csv", sep = ""))
s_dur_o$corpus = as.factor("cgn-o")
s_dur_o$register = as.factor("stories")
s_dur_o$mean_hnr = as.factor(s_dur_o$mean_hnr)
s_dur_o$nn_end_score = as.factor(s_dur_o$nn_end_score)
s_dur_o$language = as.factor("nl")
#s_dur_o_vl = read.csv(paste(cgn_path, "synvoirelPL_s_comb_comp-o_vl.csv", sep = ""))
#s_dur_o_vl$corpus = as.factor("cgn-o")
#s_dur_o_vl$register = as.factor("stories")
#s_dur_o_vl$mean_hnr = as.factor(s_dur_o_vl$mean_hnr)
#s_dur_o_vl$nn_end_score = as.factor(s_dur_o_vl$nn_end_score)
#s_dur_o_vl$language = as.factor("vl")
#s_dur_o_vl = s_dur_o_vl[ , !(names(s_dur_o_vl) %in% c("overlap"))]

s_dur = rbind(s_dur_a, s_dur_c, s_dur_d, s_dur_ifadv, s_dur_ecsd, s_dur_o, s_dur_k)
#s_dur = rbind(s_dur_a, s_dur_a_vl, s_dur_c, s_dur_c_vl, s_dur_d, s_dur_d_vl, s_dur_ifadv, s_dur_ecsd, s_dur_k, s_dur_k_vl, s_dur_o, s_dur_o_vl)
s_dur = s_dur[s_dur$type_of_s == "PL",]
s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)
#s_dur = s_dur[!(s_dur$mean_hnr == "--undefined--"),]
#s_dur$mean_hnr = as.numeric(s_dur$mean_hnr)
#s_dur = s_dur[!(s_dur$nn_end_score == "--undefined--"),]
#s_dur$nn_end_score = ifelse(s_dur$language == "nl", as.numeric(s_dur$nn_end_score) / 1000, NA)
s_dur$nn_end_score = as.numeric(as.character(s_dur$nn_end_score))
#s_dur$nn_end_score = as.numeric(s_dur$nn_end_score) / 1000
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

s_dur$s_dur_kal = s_dur$kal_end - s_dur$kal_start
s_dur$log_s_dur_kal = log(s_dur$s_dur_kal)
#s_dur$s_dur_nn = ifelse(s_dur$language == "nl", s_dur$nn_end - s_dur$kal_start + 0.01, NA)
s_dur$s_dur_nn = s_dur$nn_end - s_dur$kal_start + 0.01
plot(density(s_dur$s_dur_kal), xlim = c(0, 0.5), lty = "dashed")
lines(density(s_dur$s_dur_nn, na.rm = T))

s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)
s_dur$stress_dist = s_dur$num_syl - s_dur$word_stress

# exclude manually identified mistakes (based on comp-a, comp-a_vl, comp-d, comp-c, comp-c_vl, comp-k, comp-k_vl, comp-o, comp-o_vl, ifadv, ecsd)
possible_verbs = c("vingers", "tests", "films", "regels", "nagels", "boetes", "tips", "trips", 
                   "clubs", "types", "sprints", "flirts", "tekens", "ketens", "tafels",
                   "lades", "speeches")
uncertain_words = c(
                    "duivels", "vleugels", "heuvels", "eikels", 
                    "wapens", "hoorns", 
                    "adders",
                    "motors",  # difference between 'engine' and 'motorcycle'?
                    "kentekens",
                    "nevels",
                    "shillings"
                    )
forbidden_words = c("bands",      # die muziek maken
                    "stuks",      # 4 stuks
#                    "standaards", # voor kaarsen
                    "jaars",      # 2e jaars
                    "klasses",    # van de klasses Deventer der
                    "stands",     # leenwoord 'stents'
#                    "hordes",     # hordes mensen vs. horden lopen
                    "chatbox",    # nog niet goed verwerkt
#                    "experts",
#                    "computerexperts",
                    "gangs",      # leenwoord 'gengs'
                    "zones",      # zonen is meervoud van zoon
                    "pools",      # leenwoord 'poel'
#                    "novices",    # leenwoord
#                    "speeches",   # leenwoord
#                    "sterns",     # ?
#                    "pence",      # leenwoord
#                    "palms",      # palmbomen vs. handpalmen
                    "strips",     # boekjes vs. strippenkaart (en ww)
                    "kinders",    # andere 'speelse' betekenis, bovendien is het meervoud hier ers vs. eren
                    "echtgenotes" # ambigue enkelvoud
                    )
s_dur = s_dur[!(s_dur$word_ort %in% forbidden_words),]
s_dur = s_dur[!(s_dur$word_ort %in% possible_verbs),]
#s_dur = s_dur[!(s_dur$word_ort %in% uncertain_words),]

s_dur[s_dur$word_ort == "hersens",]$rel_freq1 = s_dur[s_dur$word_ort == "hersens",]$pl_prop
s_dur[s_dur$word_ort == "hersens",]$rel_freq2 = s_dur[s_dur$word_ort == "hersens",]$pl_prop

#s_dur$rem_prop = s_dur$en_freq / (s_dur$lem_freq - s_dur$s_freq)

# either look exclusively at ambiguous plural
#s_dur = s_dur[s_dur$pl_prop > 0 & s_dur$pl_prop < 1,]

#s_dur$ambig_type = ifelse(s_dur$prev_phon == "@", "schwa", "other")                # ziektes / ziekten
#s_dur[s_dur$stressed == T & s_dur$ambig_type == "other",]$ambig_type = "stress"    # balkons / balkonnen
# detectors / detectoren

#s_dur$ambig_type = as.factor(s_dur$ambig_type)

# or compare them to unambiguous plurals
s_dur$pl_ambig = s_dur$pl_prop < 1
s_dur$pl_ambig = as.factor(s_dur$pl_ambig)

s_dur = s_dur[!is.na(s_dur$pl_ambig),]

s_dur$prev_prev_phon = sapply(s_dur$word_phon, get_prev_prev_phon)
s_dur$prev_prev_phon = as.factor(s_dur$prev_prev_phon)
s_dur$ambig_type = ifelse(s_dur$pl_ambig == "FALSE", "unamb", "other")
s_dur[s_dur$prev_phon == "@" & s_dur$ambig_type == "other",]$ambig_type = "schwa"
s_dur[s_dur$ambig_type == "other" & s_dur$prev_phon %in% sonorants & s_dur$prev_prev_phon == "@",]$ambig_type = "schwa_son"
s_dur[s_dur$ambig_type == "other" & s_dur$prev_phon %in% sonorants & s_dur$prev_prev_phon %in% short_vowels & s_dur$stressed == T,]$ambig_type = "stress_short_son"
s_dur[s_dur$ambig_type == "other" & s_dur$prev_phon %in% sonorants & s_dur$prev_prev_phon %in% short_vowels & s_dur$stressed == F,]$ambig_type = "nostress_short_son"
s_dur[s_dur$ambig_type == "other" & s_dur$prev_phon %in% sonorants & s_dur$prev_prev_phon %in% long_vowels,]$ambig_type = "long_son"
s_dur[s_dur$ambig_type == "other" & s_dur$prev_phon %in% obstruents,]$ambig_type = "obstruent"
# mistake in word_phon, but prev_phon_pron shows @
s_dur[s_dur$word_ort == "bijdrages",]$ambig_type = "schwa"

s_dur$ambig_type = as.factor(s_dur$ambig_type)

s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

boxplot(pl_prop ~ ambig_type, data = s_dur)

# remove lines for which measurements cannot be trusted
s_dur = s_dur[(!is.na(s_dur$s_dur_nn) | s_dur$language == "vl"), ]
s_dur = s_dur[(!s_dur$s_dur_nn < 0.005 | s_dur$language == "vl"),]
is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron
# either limit based op preceding and following sounds
s_dur = s_dur[!(s_dur$prev_phon_pron %in% c("t", "d") | s_dur$next_phon_pron %in% c("j", "t", "d")),]
# or based on nn scores
plot(density(s_dur$nn_end_score))
#s_dur = s_dur[
#  s_dur$nn_start_score > 1.2 & 
#    s_dur$nn_end_score > 1.1
#,]
# remove underlyingly voiced final /s/ to check if it causes the effect
#s_dur = s_dur[s_dur$underlying_voice == "voiceless",]
s_dur = s_dur[s_dur$underlying_voice %in% c("voiced", "voiceless"),]
s_dur$underlying_voice = as.factor(as.character(s_dur$underlying_voice))
# remove NA lines
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

# remove unrepresentative outliers (Baayen, 2008, p. 243)
# s_dur = s_dur[s_dur$s_dur_nn < 0.4,]

# make new predictors and get rid of unnecessary NAs

s_dur$log_base_dur = log(s_dur$base_dur)
s_dur$log_bigf = log(s_dur$bigram_f + 1)
s_dur$log_bigf_prev = log(s_dur$prev_bigram_f + 1)

s_dur$next_phon_class = as.factor(sapply(s_dur$next_phon_pron, get_phon_class))
s_dur$prev_phon_class = as.factor(sapply(s_dur$prev_phon_pron, get_phon_class))
s_dur$prev_phon_class = relevel(s_dur$prev_phon_class, ref="V")
s_dur$next_phon_class = relevel(s_dur$next_phon_class, ref="V")

s_dur[s_dur$cow_wf == 0,]$cow_wf = 1
s_dur$log_wf = log(s_dur$cow_wf)
s_dur[is.na(s_dur$num_syl_pron),]$num_syl_pron = 0
s_dur[is.na(s_dur$lex_neb),]$lex_neb = 0
s_dur[is.na(s_dur$lex_neb_freq),]$lex_neb_freq = 0

# transform dependent variable
s_dur$log_s_dur = log(s_dur$s_dur_nn)
#plot(density(s_dur$log_s_dur))

# remove lines for which continuous predictors are NA
nrow(s_dur)
s_dur = s_dur[!(is.na(s_dur$speech_rate_pron) | is.na(s_dur$base_dur) 
#                | is.na(s_dur$rel_freq1) 
#                | is.na(s_dur$rel_freq2)
                | is.na(s_dur$num_syl_pron) | is.na(s_dur$num_cons_pron)
                | is.na(s_dur$log_wf) | is.na(s_dur$lex_neb) | is.na(s_dur$log_bigf)
                | is.na(s_dur$log_bigf_prev)
                | is.na(s_dur$stress_dist) | is.na(s_dur$syntax_f2)
                | is.na(s_dur$syntax_f3) | is.na(s_dur$syntax_f4)
                | is.na(s_dur$syntax_f5) | is.na(s_dur$syntax_f6)
                | is.na(s_dur$syntax_f7) | is.na(s_dur$syntax_f8)), ]
nrow(s_dur)

# convert syntax features to numeric
s_dur$syntax_f2 = as.numeric(s_dur$syntax_f2)
s_dur$syntax_f3 = as.numeric(s_dur$syntax_f3)
s_dur$syntax_f4 = as.numeric(s_dur$syntax_f4)
s_dur$syntax_f5 = as.numeric(s_dur$syntax_f5)
s_dur$syntax_f6 = as.numeric(s_dur$syntax_f6)
s_dur$syntax_f7 = as.numeric(s_dur$syntax_f7)
s_dur$syntax_f8 = as.numeric(s_dur$syntax_f8)

# principle components
col_pred = s_dur[, c("syntax_f2", "syntax_f3", "syntax_f4", "syntax_f5", "syntax_f6", "syntax_f7", "syntax_f8")]
col_pred_pca = prcomp(col_pred, center = T, scale. = T)
summary(col_pred_pca)

rotation_df = as.data.frame(col_pred_pca$rotation)
rotation_df_ordered = rotation_df[order(-abs(rotation_df$PC1)),]
structure(rotation_df_ordered[, "PC1"],
          names=rownames(rotation_df_ordered))

# keep pc1 - pc5, threshold of 0.9 cumulative proportion reached

s_dur$PC1 = col_pred_pca$x[,1]
s_dur$PC2 = col_pred_pca$x[,2]
s_dur$PC3 = col_pred_pca$x[,3]
s_dur$PC4 = col_pred_pca$x[,4]
s_dur$PC5 = col_pred_pca$x[,5]

s_dur$rel_base_dur = s_dur$base_dur / s_dur$num_syl_pron


# Get centred and scaled predictors
s_dur$rel_base_dur_sc = scale(s_dur$rel_base_dur)
s_dur$speech_rate_pron_sc = scale(s_dur$speech_rate_pron)
s_dur$base_dur_sc = scale(s_dur$base_dur)
s_dur$log_base_dur_sc = scale(s_dur$log_base_dur)
s_dur$num_syl_pron_sc = scale(s_dur$num_syl_pron)
s_dur$num_cons_pron_sc = scale(s_dur$num_cons_pron)
s_dur$log_wf_sc = scale(s_dur$log_wf)
s_dur$lex_neb_sc = scale(s_dur$lex_neb)
s_dur$log_bigf_sc = scale(s_dur$log_bigf)
s_dur$log_bigf_prev_sc = scale(s_dur$log_bigf_prev)
s_dur$rel_freq1_sc = scale(s_dur$rel_freq1)
s_dur$rel_freq2_sc = scale(s_dur$rel_freq2)
s_dur$stress_dist_sc = scale(s_dur$stress_dist)
s_dur$syntax_f2_sc = scale(s_dur$syntax_f2)
s_dur$syntax_f3_sc = scale(s_dur$syntax_f3)
s_dur$syntax_f4_sc = scale(s_dur$syntax_f4)
s_dur$syntax_f5_sc = scale(s_dur$syntax_f5)
s_dur$syntax_f6_sc = scale(s_dur$syntax_f6)
s_dur$syntax_f7_sc = scale(s_dur$syntax_f7)
s_dur$syntax_f8_sc = scale(s_dur$syntax_f8)

s_dur$syntax_f5_cat = as.factor(s_dur$syntax_f5)
s_dur$syntax_f6_cat = as.factor(s_dur$syntax_f6)
s_dur$syntax_f7_cat = as.factor(s_dur$syntax_f7)
s_dur$syntax_f8_cat = as.factor(s_dur$syntax_f8)




# remove lines for which categorical predictors are NA
nrow(s_dur)
s_dur = s_dur[!(is.na(s_dur$type_of_s) | is.na(s_dur$next_phon_class) 
                | is.na(s_dur$prev_mention) | is.na(s_dur$register)), ]
nrow(s_dur)

### prepare dataset
# get rid of unnecessary columns
drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced2", "per_mil_wf", "prev_word",
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "nn_start", "nn_end", "nn_start_score", "nn_end_score",
         "chan")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

#s_dur = na.omit(s_dur)

s_dur_ambig = s_dur[s_dur$pl_ambig == T,]
s_dur_ambig = s_dur_ambig[ , !(names(s_dur_ambig) %in% c("timbl_s_prob"))]
s_dur_ambig = na.omit(s_dur_ambig)
s_dur_ambig$ambig_type = as.factor(as.character(s_dur_ambig$ambig_type))

# get correct frequencies/proportions

#var = read.csv(paste(f_path, "p_f_type_O_merge_2syl_k4_ID_invar.csv", sep = ""))
var = read.csv(paste(other_path, "p_f_type_O_merge_2syl_k4_ID_invar.csv", sep = ""))
var[var$word == "client",]$f_ev = 115
var[var$word == "big",]$f_ev = 801
var[var$word == "orgie",]$f_s = 16
var[var$word == "grieve",]$f_ev = 5
var[var$word == "grieve",]$f_s = 40
var = var[!(var$word %in% c("l", "pop", "portier", "net", "water", "god", "cent", "karaat", "punt", "post", "kajak", "grieve")),]
var = var[var$f_s != 0,]

var$f_nons = var$f_en + var$f_other
var$log_freq_pl = log(var$f_s + var$f_en + var$f_other)
var$rel_freq_pl = (var$f_s + var$f_en + var$f_other) / (var$f_s + var$f_en + var$f_other + var$f_ev)

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
    return(var[var$word == lem,]$f_s + var[var$word == lem,]$f_en + var[var$word == lem,]$f_other + var[var$word == lem,]$f_ev)
  } else {
    print("bla")
    return(NA)
  }
}

get_seg_inf = function(lem) {
  if (lem == "hersens"){
    lem = "hersen"
  }
  if (lem %in% levels(var$word)){
    return(var[var$word == lem,]$seg_info)
  } else {
    return(NA)
  }
}

get_seg_inf_tok = function(lem) {
  if (lem == "hersens"){
    lem = "hersen"
  }
  if (lem %in% levels(var$word)){
    return(var[var$word == lem,]$seg_info_tok)
  } else {
    return(NA)
  }
}

get_phon_s_freq = function(lem) {
  if (lem == "hersens"){
    lem = "hersen"
  }
  if (lem %in% levels(var$word)){
    return(var[var$word == lem,]$phon_s_freq)
  } else {
    return(NA)
  }
}

get_phon_s_num = function(lem) {
  if (lem == "hersens"){
    lem = "hersen"
  }
  if (lem %in% levels(var$word)){
    return(var[var$word == lem,]$phon_s_num)
  } else {
    return(NA)
  }
}

s_dur_ambig$p_s = sapply(as.character(s_dur_ambig$lemma), get_p_s_var)
s_dur_ambig$f_s = sapply(as.character(s_dur_ambig$lemma), get_f_s)
s_dur_ambig$log_f_s = log(s_dur_ambig$f_s)
s_dur_ambig$f_en = sapply(as.character(s_dur_ambig$lemma), get_f_en)
s_dur_ambig$f_other = sapply(as.character(s_dur_ambig$lemma), get_f_oth)
s_dur_ambig$f_nons = s_dur_ambig$f_en + s_dur_ambig$f_other
s_dur_ambig$log_f_nons = log(s_dur_ambig$f_nons)
s_dur_ambig$f_lem = sapply(as.character(s_dur_ambig$lemma), get_f_lem)
s_dur_ambig$rel_f_s = s_dur_ambig$f_s / s_dur_ambig$f_lem
s_dur_ambig$rel_f_en = s_dur_ambig$f_en / s_dur_ambig$f_lem
s_dur_ambig$rel_f_nons = s_dur_ambig$f_nons / s_dur_ambig$f_lem
s_dur_ambig$rel_f_other = s_dur_ambig$f_other / s_dur_ambig$f_lem
s_dur_ambig$seg_info = sapply(as.character(s_dur_ambig$lemma), get_seg_inf)
s_dur_ambig$seg_info_token = sapply(as.character(s_dur_ambig$lemma), get_seg_inf_tok)
s_dur_ambig$phon_s_freq = sapply(as.character(s_dur_ambig$lemma), get_phon_s_freq)
s_dur_ambig$phon_s_num = sapply(as.character(s_dur_ambig$lemma), get_phon_s_num)
s_dur_ambig$plural_prop_s = log(s_dur_ambig$f_s / s_dur_ambig$phon_s_freq)

s_dur_ambig$pl_prop = s_dur_ambig$f_s / (s_dur_ambig$f_s + s_dur_ambig$f_en + s_dur_ambig$f_other)
s_dur_ambig$log_lem_freq = log(s_dur_ambig$f_lem)
s_dur_ambig$rel_freq_pl = (s_dur_ambig$f_s + s_dur_ambig$f_en + s_dur_ambig$f_other) / s_dur_ambig$f_lem

#s_dur_ambig$log_lem_freq = log(s_dur_ambig$lem_freq)
s_dur_ambig$freq_pl = s_dur_ambig$f_s + s_dur_ambig$f_en + s_dur_ambig$f_other
s_dur_ambig$f_ev = s_dur_ambig$f_lem - s_dur_ambig$freq_pl
s_dur_ambig$rel_f_ev = s_dur_ambig$f_ev / s_dur_ambig$f_lem
s_dur_ambig$log_f_ev = log(s_dur_ambig$f_ev + 1)
s_dur_ambig$log_freq_pl = log(s_dur_ambig$f_s + s_dur_ambig$f_en + s_dur_ambig$f_other)
s_dur_ambig$log_freq_pl_sc = scale(s_dur_ambig$log_freq_pl)
s_dur_ambig$dominance = as.factor(ifelse(s_dur_ambig$rel_freq_pl > .5, "plural", "singular"))

#s_dur_ambig$rel_freq_s = s_dur_ambig$s_freq / s_dur_ambig$lem_freq
#s_dur_ambig$rel_freq_en = s_dur_ambig$en_freq / s_dur_ambig$lem_freq
#s_dur_ambig$rel_freq_diff = s_dur_ambig$rel_freq_s - s_dur_ambig$rel_freq_en
#s_dur_ambig$log_freq_s = log(s_dur_ambig$s_freq)

s_dur_ambig$entropy = -1*(ifelse(s_dur_ambig$f_ev > 0, s_dur_ambig$rel_f_ev*log2(s_dur_ambig$rel_f_ev), 0)
                          + ifelse(s_dur_ambig$f_s > 0, s_dur_ambig$rel_f_s*log2(s_dur_ambig$rel_f_s), 0)
                          + ifelse(s_dur_ambig$f_en > 0, s_dur_ambig$rel_f_en*log2(s_dur_ambig$rel_f_en), 0)
                          + ifelse(s_dur_ambig$f_other > 0, s_dur_ambig$rel_f_other*log2(s_dur_ambig$rel_f_other), 0)
                          )

# put lower boundary on frequency of pl, to exclude most unreliable pl_prop measurements
s_dur_ambig_full = s_dur_ambig
lower_boundary = 10
s_dur_ambig = s_dur_ambig_full[s_dur_ambig_full$freq_pl >= lower_boundary,]

s_dur_ambig$speech_rate_pron_sc = scale(s_dur_ambig$speech_rate_pron)
s_dur_ambig$base_dur_sc = scale(s_dur_ambig$base_dur)
s_dur_ambig$num_syl_pron_sc = scale(s_dur_ambig$num_syl_pron)
s_dur_ambig$lex_neb_sc = scale(s_dur_ambig$lex_neb)
s_dur_ambig$log_bigf_sc = scale(s_dur_ambig$log_bigf)
s_dur_ambig$log_bigf_prev_sc = scale(s_dur_ambig$log_bigf_prev)
s_dur_ambig$log_f_s_sc = scale(s_dur_ambig$log_f_s)
s_dur_ambig$log_f_nons_sc = scale(s_dur_ambig$log_f_nons)
s_dur_ambig$p_dev = s_dur_ambig$p_s - s_dur_ambig$pl_prop

library(aods3)
library(VGAM)
s_dur_ambig$p_pred = logitlink(predict(aodml(cbind(f_s, f_nons) ~ p_s*log_freq_pl + p_s*rel_freq_pl, var), newdata = s_dur_ambig), inverse = T)
s_dur_ambig$p_resid = s_dur_ambig$p_pred - s_dur_ambig$pl_prop
s_dur_ambig$p_resid2 = predict(aodml(cbind(f_s, f_nons) ~ p_s*log_freq_pl + p_s*rel_freq_pl, var), newdata = s_dur_ambig) - logitlink(s_dur_ambig$pl_prop)

# principle components
col_pred = s_dur_ambig[, c("syntax_f2", "syntax_f3", "syntax_f4", "syntax_f5", "syntax_f6", "syntax_f7", "syntax_f8")]
col_pred_pca = prcomp(col_pred, center = T, scale. = T)
summary(col_pred_pca)

# keep pc1 - pc5, threshold of 0.9 cumulative proportion reached
s_dur_ambig$PC1 = col_pred_pca$x[,1]
s_dur_ambig$PC2 = col_pred_pca$x[,2]
s_dur_ambig$PC3 = col_pred_pca$x[,3]
s_dur_ambig$PC4 = col_pred_pca$x[,4]
s_dur_ambig$PC5 = col_pred_pca$x[,5]

s_dur_ambig$PC1_sc = scale(s_dur_ambig$PC1)
s_dur_ambig$PC2_sc = scale(s_dur_ambig$PC2)
s_dur_ambig$PC3_sc = scale(s_dur_ambig$PC3)
s_dur_ambig$PC4_sc = scale(s_dur_ambig$PC4)
s_dur_ambig$PC5_sc = scale(s_dur_ambig$PC5)

s_dur_ambig$speaker = as.factor(as.character(s_dur_ambig$speaker))
s_dur_ambig$word_ort = as.factor(as.character(s_dur_ambig$word_ort))

# hersnes/hersenen zit er hier niet meer in!

s_dur_unambig = s_dur[s_dur$pl_ambig == F,]
s_dur_unambig = s_dur_unambig[,!(names(s_dur_unambig) %in% c("timbl_s_prob"))]
s_dur_unambig = na.omit(s_dur_unambig)
s_dur_unambig$log_lem_freq = log(s_dur_unambig$lem_freq)

# get correct invar probabilities

invar = read.csv(paste(other_path, "invar_probs.csv", sep = ""))

get_p_s = function(wrd) {
  lem = sub("[']*s$", "", wrd)
  if (lem %in% levels(invar$word)){
    return(invar[invar$word == lem,]$p_s)
  } else {
    return(NA)
  }
}

# s_dur_unambig$p_s = sapply(as.character(s_dur_unambig$word_ort), get_p_s)
# exclude diminuatives?
# s_dur_unambig = s_dur_unambig[substr(as.character(s_dur_unambig$word_ort), nchar(as.character(s_dur_unambig$word_ort)) - 2, nchar(as.character(s_dur_unambig$word_ort))) != "jes",]
# s_dur_unambig = na.omit(s_dur_unambig)

invar_freqs = read.csv(paste(other_path, "invar_freqs3.csv", sep = ""))

get_f_ev = function(wrd) {
  lem = sub("[']*s$", "", wrd)
  if (lem %in% levels(invar_freqs$word)){
    return(invar_freqs[invar_freqs$word == lem,]$f_ev)
  } else {
    return(NA)
  }
}

get_f_mv = function(wrd) {
  lem = sub("[']*s$", "", wrd)
  if (lem %in% levels(invar_freqs$word)){
    return(invar_freqs[invar_freqs$word == lem,]$f_mv)
  } else {
    return(NA)
  }
}

s_dur_unambig$f_ev = sapply(as.character(s_dur_unambig$word_ort), get_f_ev)
s_dur_unambig$f_mv = sapply(as.character(s_dur_unambig$word_ort), get_f_mv)
s_dur_unambig$rel_freq_pl = s_dur_unambig$f_mv / (s_dur_unambig$f_mv + s_dur_unambig$f_ev)
s_dur_unambig$log_lem_freq = log(s_dur_unambig$f_ev + s_dur_unambig$f_mv)
s_dur_unambig = na.omit(s_dur_unambig)

### Inspect collinearity
continuous = c("speech_rate_pron_sc", "base_dur_sc", "num_syl_pron_sc", 
               "log_freq_pl", "lex_neb_sc", "log_bigf_sc", "log_bigf_prev_sc",
               "PC1", "PC2", "PC3", "pl_prop", "rel_freq_pl")

corrplot(cor(s_dur_ambig[, continuous], use = "complete.obs"), method = "number")

continuous = c("speech_rate_pron", "base_dur", "num_syl_pron", 
               "num_cons_pron", "log_lem_freq", "lex_neb", "log_bigf",
               "syntax_f2", "syntax_f3", "syntax_f4", "pl_prop", "rel_freq_pl")

corrplot(cor(s_dur_ambig[, continuous], use = "complete.obs"), method = "number")
#vif(lm(log_s_dur ~ speech_rate_pron + base_dur + num_syl_pron 
#       + num_cons_pron + log_wf + lex_neb + log_bigf , data=s_dur))
#collin.fnc(na.omit(s_dur[, continuous]))$cnumber

categorical = c("register", 
                "next_phon_class", 
                "prev_mention", "syntax_f5_cat", "syntax_f6_cat", "syntax_f7_cat", "syntax_f8_cat", "stressed")

cat_ass = matrix(c(
                   cramerV(table(s_dur_ambig[,c("register", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("register", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("register", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("register", "syntax_f5_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("register", "syntax_f6_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("register", "syntax_f7_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("register", "syntax_f8_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("register", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("next_phon_class", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("next_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("next_phon_class", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("next_phon_class", "syntax_f5_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("next_phon_class", "syntax_f6_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("next_phon_class", "syntax_f7_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("next_phon_class", "syntax_f8_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("next_phon_class", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("prev_mention", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("prev_mention", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("prev_mention", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("prev_mention", "syntax_f5_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("prev_mention", "syntax_f6_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("prev_mention", "syntax_f7_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("prev_mention", "syntax_f8_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("prev_mention", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f5_cat", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f5_cat", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f5_cat", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f5_cat", "syntax_f5_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f5_cat", "syntax_f6_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f5_cat", "syntax_f7_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f5_cat", "syntax_f8_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f5_cat", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f6_cat", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f6_cat", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f6_cat", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f6_cat", "syntax_f5_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f6_cat", "syntax_f6_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f6_cat", "syntax_f7_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f6_cat", "syntax_f8_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f6_cat", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f7_cat", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f7_cat", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f7_cat", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f7_cat", "syntax_f5_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f7_cat", "syntax_f6_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f7_cat", "syntax_f7_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f7_cat", "syntax_f8_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f7_cat", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f8_cat", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f8_cat", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f8_cat", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f8_cat", "syntax_f5_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f8_cat", "syntax_f6_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f8_cat", "syntax_f7_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f8_cat", "syntax_f8_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("syntax_f8_cat", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("stressed", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("stressed", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("stressed", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("stressed", "syntax_f5_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("stressed", "syntax_f6_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("stressed", "syntax_f7_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("stressed", "syntax_f8_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur_ambig[,c("stressed", "stressed")]), bias.correct = TRUE)),
                 nrow = 8, ncol = 8, byrow = T, dimnames = list(
                   categorical,
                   categorical))

corrplot(cat_ass, method = "number")

cat_con = matrix(c(
                   sqrt(summary(lm(speech_rate_pron ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(base_dur ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_lem_freq ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(lex_neb ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_bigf ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(pl_prop ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(rel_freq_pl ~ register, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(base_dur ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_lem_freq ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(lex_neb ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_bigf ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(pl_prop ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(rel_freq_pl ~ next_phon_class, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(base_dur ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_lem_freq ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(lex_neb ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_bigf ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(pl_prop ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(rel_freq_pl ~ prev_mention, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(base_dur ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_lem_freq ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(lex_neb ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_bigf ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(pl_prop ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(rel_freq_pl ~ syntax_f5_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(base_dur ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_lem_freq ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(lex_neb ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_bigf ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(pl_prop ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(rel_freq_pl ~ syntax_f6_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(base_dur ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_lem_freq ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(lex_neb ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_bigf ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(pl_prop ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(rel_freq_pl ~ syntax_f7_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(base_dur ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_lem_freq ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(lex_neb ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_bigf ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(pl_prop ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(rel_freq_pl ~ syntax_f8_cat, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(base_dur ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_lem_freq ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(lex_neb ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(log_bigf ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(pl_prop ~ stressed, data = s_dur_ambig))$r.squared),
                   sqrt(summary(lm(rel_freq_pl ~ stressed, data = s_dur_ambig))$r.squared)
), 
nrow = 8, ncol = 12, byrow = T, dimnames = list(
  categorical,
  continuous))

corrplot(cat_con, method = "number")

s_dur_ambig = na.omit(s_dur_ambig)

s_dur_ambig_reg = s_dur_ambig
s_dur_ambig_reg$register = as.character(s_dur_ambig_reg$register)
s_dur_ambig_reg = s_dur_ambig_reg[s_dur_ambig_reg$register %in% c("conversation", "stories"),]
s_dur_ambig_reg$register = as.factor(s_dur_ambig_reg$register)

### try single lmer (no correlation between pl_prop, rel_freq1 and covariates)
control2 = lmer(log_s_dur ~ speech_rate_pron_sc 
                + log_base_dur_sc 
                + num_syl_pron_sc 
#                + num_cons_pron_sc 
                + prev_phon_class
#                + log_freq_pl_sc
#                + log_wf_sc
#                + rel_f_s + rel_f_nons
                + lex_neb_sc 
                + log_bigf_sc 
                + log_bigf_prev_sc
#                + syntax_f2_sc + syntax_f3_sc 
#                + syntax_f4_sc 
#                + syntax_f5_cat + syntax_f6_cat 
#                + syntax_f7_cat + syntax_f8_cat 
                + PC1_sc + PC2_sc + PC3_sc + PC4_sc + PC5_sc
                + stressed
                + next_phon_class 
                + prev_mention 
                + register
                + rel_freq_pl*pl_prop
#                + log(phon_s_num)
#                + poly(rel_freq_pl,2)*pl_prop #+ rel_freq_pl:p_pred  #+ dominance*p_pred #rel_freq_pl*p_dev
#                + entropy
                + #rel_f_s #*rel_f_nons #log(f_s/f_lem)
#                + log_f_s + log_f_nons + log_f_ev
#                + log_lem_freq
                + (1 | word_ort)
                + (1 | speaker),
                control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
#                REML = F,
                data=s_dur_ambig)

s_dur_ambig$dur_resid2 = resid(control2)
s_dur_trim2 = s_dur_ambig[abs(scale(s_dur_ambig$dur_resid2)) < 2.5,]

control_trim2 = lmer(log_s_dur ~ speech_rate_pron_sc 
                     + log_base_dur_sc 
                     + num_syl_pron_sc 
#                     + num_cons_pron_sc
                     + prev_phon_class
#                     + log_freq_pl_sc
#                     + log_wf_sc
#                     + log_f_s #+ log_f_nons_sc
                     + lex_neb_sc
                     + log_bigf_sc 
                     + log_bigf_prev_sc
#                     + syntax_f2_sc + syntax_f3_sc 
#                     + syntax_f4_sc 
#                     + syntax_f5_cat + syntax_f6_cat 
#                     + syntax_f7_cat + syntax_f8_cat
                     + PC1_sc + PC2_sc + PC3_sc + PC4_sc + PC5_sc
                     + stressed
                     + next_phon_class 
                     + prev_mention 
                     + register
                     + rel_freq_pl*pl_prop
                     + (1 | word_ort)
                     + (1 | speaker), 
                     control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                     data=s_dur_trim2)

anova(control_trim2)
#par(mar=c(2,2,2,2))
plot(effect("rel_freq_pl:pl_prop", control_trim2, x.var = "rel_freq_pl")
     , multiline=T, rug = F, main = "", ylab = "log(seconds)", ci.style = "bands", 
     xlab = "Proportion plural / number", 
     key.args = list(space="right", columns=1, title = "Proportion -s / plural", cex.title = 0.9)
     )

pm = plot_model(control_trim2, type = "eff", terms = c("pl_prop", "rel_freq_pl [0.0318888, 0.4165103, 1]"), colors = "bw", legend.title = "Proportion(PL)", title = "")
pm + labs(y = "log(seconds)", x="Proportion(-s)") # + geom_point(data = s_dur_trim2, mapping = aes(x = pl_prop, y = log_s_dur), inherit.aes = FALSE)

pm = plot_model(control_trim2, type = "eff", terms = c("rel_freq_pl", "pl_prop [0.00227199, 0.8092689, 0.9985915]"), colors = "bw", legend.title = "Proportion(-s)", title = "")
pm + labs(y = "log(seconds)", x="Proportion(PL)") # + geom_point(data = s_dur_trim2, mapping = aes(x = pl_prop, y = log_s_dur), inherit.aes = FALSE)


# only significant predictors:
control2b = lmer(log_s_dur ~ speech_rate_pron_sc +
#                 + log_base_dur_sc
#                 + lex_neb_sc + stressed
                 + PC1_sc + PC2_sc + PC3_sc
                 + next_phon_class 
                 + register
#                 + entropy
#                 + log_f_s + log_f_nons + log_f_ev
#                 + rel_f_s + pl_prop
#                 + rel_freq_pl*p_dev
                 + rel_freq_pl*pl_prop
                 + (1 | speaker) 
                 + (1 | word_ort),
                 control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
#                 REML = F,
                 data=s_dur_ambig)

s_dur_ambig$dur_resid2 = resid(control2b)
s_dur_trim2 = s_dur_ambig[abs(scale(s_dur_ambig$dur_resid2)) < 2.5,]

control2b_trim = lmer(log_s_dur ~ speech_rate_pron_sc +
#                        + lex_neb_sc + stressed 
                      + PC1_sc + PC2_sc + PC3_sc
                      + next_phon_class 
                      + register
                      + rel_freq_pl*pl_prop
                      + (1 | speaker) 
                      + (1 | word_ort),
                      control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                      data=s_dur_trim2)

anova(control2b_trim)

qqnorm(resid(control2b_trim), main = "QQ-plot")
qqline(resid(control2b_trim))

plot(effect("rel_freq_pl:pl_prop", control2b_trim, x.var = "rel_freq_pl")
     , multiline=T, rug = F, main = "", ylab = "log(seconds)", ci.style = "bands", 
     xlab = "Proportion plural / number", 
     key.args = list(space="right", columns=1, title = "Proportion -s / plural", cex.title = 0.9)
)

ggplot(data = s_dur_trim2) + aes(x = pl_prop, y = log_s_dur) + ylim(-3.2, -2.2) + xlim(0, 1) + labs(y = "log(seconds)", x="Proportion(PL)") + ggtitle("Variable Plurals") # + geom_line(aes(linetype = "0"), alpha=0) + geom_line(aes(linetype = "0.5"), alpha=0) + geom_line(aes(linetype = "1"), alpha=0) + labs(linetype='Proportion(-s)')

pm = plot_model(control2b_trim, type = "pred", terms = c("rel_freq_pl", "pl_prop [0.9985915]"), colors = "bw", legend.title = "Proportion(-s)", title = "", show.legend = F)
pm + ylim(-3.2, -2.2) + xlim(0, 1) + labs(y = "log(seconds)", x="Proportion(PL)") + ggtitle("Variable Plurals") # + geom_point(data = s_dur_trim2, mapping = aes(x = pl_prop, y = log_s_dur), inherit.aes = FALSE)

pm = plot_model(control2b_trim, type = "pred", terms = c("rel_freq_pl", "pl_prop [0.00227199, 0.9985915]"), colors = "bw", legend.title = "Proportion(-s)", title = "", show.legend = F)
pm + ylim(-3.2, -2.2) + xlim(0, 1) + labs(y = "log(seconds)", x="Proportion(PL)") + ggtitle("Variable Plurals") + scale_linetype_manual(values=c("dashed", "solid")) # + geom_point(data = s_dur_trim2, mapping = aes(x = pl_prop, y = log_s_dur), inherit.aes = FALSE)


pm = plot_model(control2b_trim, type = "pred", terms = c("rel_freq_pl", "pl_prop [0.9985915, 0.00227199]"), colors = "bw", legend.title = "Proportion(-s)", title = "")
pm2 = pm + ylim(-3.2, -2.2) + xlim(0, 1) + labs(y = "log(seconds)", x="Proportion(PL)") + scale_linetype_manual(values=c("dashed", "solid"), labels=c("Min", "Max")) + ggtitle("B") # + geom_point(data = s_dur_trim2, mapping = aes(x = pl_prop, y = log_s_dur), inherit.aes = FALSE)

qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int) + labs(y = "Sample Quantiles", x="Theoretical Quantiles") + ggtitle("A")
  
}

qp = qqplot.data(resid(control2b_trim))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(qp, pm2, cols=2)


# only conversation
s_dur_ambig_conv = s_dur_ambig[s_dur_ambig$register == "conversation",]
s_dur_ambig_conv$corpus = as.factor(as.character(s_dur_ambig_conv$corpus))

conv = lmer(log_s_dur ~ speech_rate_pron_sc 
#            + base_dur_sc 
#            + num_syl_pron_sc 
#                + num_cons_pron_sc 
#            + prev_phon_class
#                + log_freq_pl
#                + lex_neb_sc 
#                + log_bigf_sc 
#                + log_bigf_prev_sc
#                + syntax_f2_sc 
#                + syntax_f3_sc 
                + syntax_f4_sc 
#                + syntax_f5_cat 
#             + syntax_f6_cat 
#             + syntax_f7_cat 
#             + syntax_f8_cat 
#              + PC1 + PC2 + PC3
#                + stressed
                + next_phon_class 
#                + prev_mention 
#                + corpus
                + rel_freq_pl*pl_prop#*log_freq_pl
                + (1 | speaker) 
                + (1 | word_ort),
                control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                data=s_dur_ambig_conv)

s_dur_ambig_conv$dur_resid2 = resid(conv)
s_dur_ambig_conv_trim = s_dur_ambig_conv[abs(scale(s_dur_ambig_conv$dur_resid2)) < 2.5,]

conv_trim = lmer(log_s_dur ~ speech_rate_pron_sc 
#                 + base_dur_sc 
#                 + num_syl_pron_sc 
#                     + num_cons_pron_sc 
#                  + prev_phon_class
#                     + log_freq_pl
#                     + lex_neb_sc 
#                     + log_bigf_sc 
#                     + log_bigf_prev_sc
#                     + syntax_f2_sc 
#                     + syntax_f3_sc 
                     + syntax_f4_sc 
#                     + syntax_f5_cat 
#                 + syntax_f6_cat 
#                 + syntax_f7_cat 
#                 + syntax_f8_cat
#                  + PC1 + PC2 + PC3
#                     + stressed
                     + next_phon_class 
#                     + prev_mention 
                     + rel_freq_pl*pl_prop#*log_freq_pl
                     + (1 | speaker) 
                     + (1 | word_ort),
                     control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                     data=s_dur_ambig_conv_trim)

anova(conv_trim)

# a cleaner way of doing this would be to define rel_freq as the relative frequency
# of ANY plural form compared to the lemma frequency
# check how exactlt rel_freq1 is calculated and compare to pl_prop

### try Mirjam's residuals method
### Now try with scaled and centred predictors instead of PCs

control2 = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
               + num_cons_pron_sc 
#               + log_wf_sc 
               + log_lem_freq
               + lex_neb_sc + log_bigf_sc 
               + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
               + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat 
               + stressed
               + next_phon_class + prev_mention 
               + register
               + (1 | speaker) 
               + (1 | word_ort),
               control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
               data=s_dur_ambig)

s_dur_ambig$dur_resid2 = resid(control2)
s_dur_trim2 = s_dur_ambig[abs(scale(s_dur_ambig$dur_resid2)) < 2.5,]

control_trim2 = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
                     + num_cons_pron_sc 
#                     + log_wf_sc 
                     + log_lem_freq
                     + lex_neb_sc + log_bigf_sc 
                     + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
                     + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat
                     + stressed
                     + next_phon_class + prev_mention 
                     + register
                     + (1 | speaker) 
                     + (1 | word_ort),
                     control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                     data=s_dur_trim2)

s_dur_trim2$dur_resid = resid(control_trim2)


interest2 = lm(dur_resid ~ rel_freq_pl*pl_prop,
              data=s_dur_trim2)

summary(interest2)

plot(effect("rel_freq_pl:pl_prop", interest2, x.var = "pl_prop"), 
     multiline = T, 
     rug = F,
#     ci.style = "bars",
     main = "", 
#     xlab = "Morphological status", 
     ylab = "Residuals in log(seconds)")


# do follow-up tests using https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html
library(interactions)

interact_plot(control2b_trim, pred = rel_freq_pl, modx = pl_prop, 
              plot.points = T, colors = c("red", "blue", "green")
              )

ssl = sim_slopes(control2b_trim, pred = rel_freq_pl, modx = pl_prop
           , modx.values = c(0.01, 0.5, 0.99)
           , johnson_neyman = T, control.fdr = TRUE
           )

ssl$slopes

probe_interaction(control2b_trim, pred = rel_freq_pl, modx = pl_prop, cond.int = TRUE,
                  interval = TRUE,  jnplot = TRUE)


# check unambig
continuous = c("speech_rate_pron", "base_dur", "num_syl_pron", 
               "num_cons_pron", "log_lem_freq", "lex_neb", "log_bigf",
               "syntax_f2", "syntax_f3", "syntax_f4", "rel_freq_pl")

corrplot(cor(s_dur_unambig[, continuous], use = "complete.obs"), method = "number")

categorical = c("register", 
                "next_phon_class", 
                "prev_mention", "syntax_f5_cat", "syntax_f6_cat", "syntax_f7_cat", "syntax_f8_cat", "stressed")

cat_con = matrix(c(
  sqrt(summary(lm(speech_rate_pron ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(base_dur ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_syl_pron ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_cons_pron ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_lem_freq ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(lex_neb ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_bigf ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f2 ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f3 ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f4 ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(rel_freq_pl ~ register, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(speech_rate_pron ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(base_dur ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_syl_pron ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_cons_pron ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_lem_freq ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(lex_neb ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_bigf ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f2 ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f3 ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f4 ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(rel_freq_pl ~ next_phon_class, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(speech_rate_pron ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(base_dur ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_syl_pron ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_cons_pron ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_lem_freq ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(lex_neb ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_bigf ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f2 ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f3 ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f4 ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(rel_freq_pl ~ prev_mention, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(speech_rate_pron ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(base_dur ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_syl_pron ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_cons_pron ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_lem_freq ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(lex_neb ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_bigf ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f2 ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f3 ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f4 ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(rel_freq_pl ~ syntax_f5_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(speech_rate_pron ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(base_dur ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_syl_pron ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_cons_pron ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_lem_freq ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(lex_neb ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_bigf ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f2 ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f3 ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f4 ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(rel_freq_pl ~ syntax_f6_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(speech_rate_pron ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(base_dur ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_syl_pron ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_cons_pron ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_lem_freq ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(lex_neb ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_bigf ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f2 ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f3 ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f4 ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(rel_freq_pl ~ syntax_f7_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(speech_rate_pron ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(base_dur ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_syl_pron ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_cons_pron ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_lem_freq ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(lex_neb ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_bigf ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f2 ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f3 ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f4 ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(rel_freq_pl ~ syntax_f8_cat, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(speech_rate_pron ~ stressed, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(base_dur ~ stressed, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_syl_pron ~ stressed, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(num_cons_pron ~ stressed, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_lem_freq ~ stressed, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(lex_neb ~ stressed, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(log_bigf ~ stressed, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f2 ~ stressed, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f3 ~ stressed, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(syntax_f4 ~ stressed, data = s_dur_unambig))$r.squared),
  sqrt(summary(lm(rel_freq_pl ~ stressed, data = s_dur_unambig))$r.squared)
), 
nrow = 8, ncol = 11, byrow = T, dimnames = list(
  categorical,
  continuous))

corrplot(cat_con, method = "number")

# should we exclude diminuatives for a fair comparison?

s_dur_unambig_reg = s_dur_unambig
s_dur_unambig_reg$register = as.character(s_dur_unambig_reg$register)
#s_dur_unambig_reg[s_dur_unambig_reg$register %in% c("news","stories"),]$register = "read"
s_dur_unambig_reg = s_dur_unambig_reg[s_dur_unambig_reg$register %in% c("conversation","stories"),]
s_dur_unambig_reg$register = as.factor(s_dur_unambig_reg$register)

### try single lmer (no correlation between pl_prop, rel_freq1 and covariates)
control3 = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
#                + num_cons_pron_sc
                + prev_phon_class
                + log_lem_freq
#                + log_wf_sc 
#                + lex_neb_sc 
                + log_bigf_sc 
#                + log_bigf_prev_sc
#                + syntax_f2_sc 
#                + syntax_f3_sc 
#                + syntax_f4_sc 
                + syntax_f5_cat 
#                + syntax_f6_cat 
                + syntax_f7_cat + syntax_f8_cat 
#                + stressed
                + next_phon_class + prev_mention 
                + register
                + rel_freq_pl*register#*p_s
                + (1 | speaker) 
                + (1 | word_ort),
                control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                data=s_dur_unambig_reg)

s_dur_unambig_reg$dur_resid2 = resid(control3)
s_dur_trim3 = s_dur_unambig_reg[abs(scale(s_dur_unambig_reg$dur_resid2)) < 2.5,]

control_trim3 = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
#                     + num_cons_pron_sc
                     + prev_phon_class
                     + log_lem_freq
#                     + log_wf_sc 
#                     + lex_neb_sc 
                    + log_bigf_sc 
#                     + log_bigf_prev_sc
#                     + syntax_f2_sc 
#                     + syntax_f3_sc + syntax_f4_sc 
                     + syntax_f5_cat 
#                     + syntax_f6_cat 
                     + syntax_f7_cat + syntax_f8_cat
#                     + stressed
                     + next_phon_class + prev_mention 
                     + register
                     + rel_freq_pl*register#*p_s
                     + (1 | speaker) 
                     + (1 | word_ort),
                     control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                     data=s_dur_trim3)

anova(control_trim3)


s_dur_unambig_conv = s_dur_unambig[s_dur_unambig$register == "conversation",]

control3 = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
#                + num_cons_pron_sc
                + prev_phon_class
                + log_lem_freq
                #                + log_wf_sc 
#                + lex_neb_sc 
                + log_bigf_sc 
#                + log_bigf_prev_sc
#                + syntax_f2_sc 
#                + syntax_f3_sc 
#                + syntax_f4_sc 
#                + syntax_f5_cat 
#                + syntax_f6_cat 
                + syntax_f7_cat 
                + syntax_f8_cat 
#                + stressed
                + next_phon_class + prev_mention 
                + rel_freq_pl#*p_s
                + (1 | speaker) 
                + (1 | word_ort),
                control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                data=s_dur_unambig_conv)

s_dur_unambig_conv$dur_resid2 = resid(control3)
s_dur_trim3 = s_dur_unambig_conv[abs(scale(s_dur_unambig_conv$dur_resid2)) < 2.5,]

control_trim3 = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
#                     + num_cons_pron_sc
                     + prev_phon_class
                     + log_lem_freq
                     #                     + log_wf_sc 
#                     + lex_neb_sc 
                     + log_bigf_sc 
#                     + log_bigf_prev_sc
#                     + syntax_f2_sc 
#                     + syntax_f3_sc 
#                     + syntax_f4_sc 
#                     + syntax_f5_cat 
#                     + syntax_f6_cat 
                     + syntax_f7_cat 
                     + syntax_f8_cat
#                     + stressed
                     + next_phon_class + prev_mention 
                     + rel_freq_pl#*p_s
                     + (1 | speaker) 
                     + (1 | word_ort),
                     control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                     data=s_dur_trim3)

summary(control_trim3)


plot(effect("rel_freq_pl", control_trim3), rug = F, main = "", ylab = "log(seconds)", xlab = "Proportion plural / lemma", colors = c("orange"))

ggplot(data = s_dur_trim3) + aes(x = pl_prop, y = log_s_dur) + ylim(-2.66, -2.525) + xlim(0, 1) + labs(y = "log(seconds)", x="Proportion(PL)") + ggtitle("Invariable Plurals") # + geom_line(aes(linetype = "0"), alpha=0) + geom_line(aes(linetype = "0.5"), alpha=0) + geom_line(aes(linetype = "1"), alpha=0) + labs(linetype='Proportion(-s)')


pm = plot_model(control_trim3, type = "eff", terms = c("rel_freq_pl"), colors = "bw", title = "Invariable Plurals")
pm + ylim(-2.66, -2.525) + xlim(0, 1) + labs(y = "log(seconds)", x="Proportion(PL)") # + geom_point(data = s_dur_trim3, mapping = aes(x = rel_freq_pl, y = log_s_dur), inherit.aes = FALSE)


#grid.arrange(p2, p1, ncol = 2)

par(mfrow=c(2,2))
plot(predict(control_trim), resid(control_trim))
qqnorm(resid(control_trim), main = "PCs")
qqline(resid(control_trim))
plot(predict(control_trim2), resid(control_trim2))
qqnorm(resid(control_trim2), main = "Original")
qqline(resid(control_trim2))
par(mfrow=c(1,1))

# Centre of Gravity

control_cog2 = lmer(s_cog_window ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
                    + num_cons_pron_sc 
#                    + log_wf_sc 
                    + log_lem_freq
                    + lex_neb_sc + log_bigf_sc 
                    + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
                    + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat
                    + stressed
                    + register
                    + next_phon_class + prev_mention 
                    + rel_freq_pl*pl_prop
                   + (1 | speaker) 
                   + (1 | word_ort),
                   control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                   data=s_dur_ambig)

s_dur_ambig$cog_resid2 = resid(control_cog2)
s_cog_trim2 = s_dur_ambig[abs(scale(s_dur_ambig$cog_resid2)) < 2.5,]

control_cog_trim2 = lmer(s_cog_window ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
                         + num_cons_pron_sc 
#                         + log_wf_sc 
                         + log_lem_freq
                         + lex_neb_sc + log_bigf_sc 
                         + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
                         + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat
                         + stressed
                         + register
                         + next_phon_class + prev_mention 
                         + rel_freq_pl*pl_prop
                        + (1 | speaker) 
                        + (1 | word_ort),
                        control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                        data=s_cog_trim2)

summary(control_cog_trim2)
plot(effect("rel_freq_pl:pl_prop", control_cog_trim2, x.var="rel_freq_pl"))

par(mfrow=c(2,2))
plot(predict(control_cog2), resid(control_cog2))
qqnorm(resid(control_cog2), main = "PCs")
qqline(resid(control_cog2))
plot(predict(control_cog_trim2), resid(control_cog_trim2))
qqnorm(resid(control_cog_trim2), main = "Original")
qqline(resid(control_cog_trim2))
par(mfrow=c(1,1))

# non-variable

control_cog3 = lmer(s_cog_window ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
                    + num_cons_pron_sc 
#                    + log_wf_sc 
                    + log_lem_freq
                    + lex_neb_sc + log_bigf_sc 
                    + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
                    + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat
                    + stressed
                    + register
                    + next_phon_class + prev_mention 
                    + rel_freq_pl
                    + (1 | speaker) 
                    + (1 | word_ort),
                    control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                    data=s_dur_unambig)

s_dur_unambig$cog_resid2 = resid(control_cog3)
s_cog_trim3 = s_dur_unambig[abs(scale(s_dur_unambig$cog_resid2)) < 2.5,]

control_cog_trim3 = lmer(s_cog_window ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
                         + num_cons_pron_sc 
#                         + log_wf_sc 
                         + log_lem_freq
                         + lex_neb_sc + log_bigf_sc 
                         + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
                         + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat
                         + stressed
                         + register
                         + next_phon_class + prev_mention 
                         + rel_freq_pl
                         + (1 | speaker) 
                         + (1 | word_ort),
                         control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                         data=s_cog_trim3)

summary(control_cog_trim3)
