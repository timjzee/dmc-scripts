library(languageR)
library(corrplot)
library(car)
library(rcompanion)
#library(lme4)
library(lmerTest)
library(effects)
library(gridExtra)

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

s_dur_a = read.csv(paste(cgn_path, "synvoirel_s_comb_comp-a2.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
s_dur_a$register = as.factor("conversation")
s_dur_a$mean_hnr = as.factor(s_dur_a$mean_hnr)
s_dur_a$nn_end_score = as.factor(s_dur_a$nn_end_score)
s_dur_a = s_dur_a[s_dur_a$overlap == FALSE,]
s_dur_a = s_dur_a[ , !(names(s_dur_a) %in% c("overlap"))]
s_dur_c = read.csv(paste(cgn_path, "synvoirel_s_comb_comp-c2.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$register = as.factor("conversation")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
s_dur_c$mean_hnr = as.factor(s_dur_c$mean_hnr)
s_dur_c$nn_end_score = as.factor(s_dur_c$nn_end_score)
s_dur_c = s_dur_c[s_dur_c$overlap == FALSE,]
s_dur_c = s_dur_c[ , !(names(s_dur_c) %in% c("overlap"))]
s_dur_d = read.csv(paste(cgn_path, "synvoirel_s_comb_comp-d2.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
s_dur_d$register = as.factor("conversation")
s_dur_d$mean_hnr = as.factor(s_dur_d$mean_hnr)
s_dur_d$nn_end_score = as.factor(s_dur_d$nn_end_score)
s_dur_d = s_dur_d[s_dur_d$overlap == FALSE,]
s_dur_d = s_dur_d[ , !(names(s_dur_d) %in% c("overlap"))]
s_dur_ifadv = read.csv(paste(ifadv_path, "synvoirel_s_comb_ifadv.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$register = as.factor("conversation")
s_dur_ifadv$mean_hnr = as.factor(s_dur_ifadv$mean_hnr)
s_dur_ifadv$nn_end_score = as.factor(s_dur_ifadv$nn_end_score)
s_dur_ifadv = s_dur_ifadv[ , !(names(s_dur_ifadv) %in% c("lemma"))]
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
s_dur_ecsd = read.csv(paste(ecsd_path, "synvoirel_s_comb_ecsd.csv", sep = ""))
s_dur_ecsd$corpus = as.factor("ecsd")
s_dur_ecsd$register = as.factor("conversation")
s_dur_ecsd$mean_hnr = as.factor(s_dur_ecsd$mean_hnr)
s_dur_ecsd$nn_end_score = as.factor(s_dur_ecsd$nn_end_score)

s_dur_k = read.csv(paste(cgn_path, "synvoirel_s_comb_comp-k.csv", sep = ""))
s_dur_k$corpus = as.factor("cgn-k")
s_dur_k$register = as.factor("news")
s_dur_k$mean_hnr = as.factor(s_dur_k$mean_hnr)
s_dur_k$nn_end_score = as.factor(s_dur_k$nn_end_score)
s_dur_k = s_dur_k[ , !(names(s_dur_k) %in% c("lemma"))]
s_dur_o = read.csv(paste(cgn_path, "synvoirel_s_comb_comp-o.csv", sep = ""))
s_dur_o$corpus = as.factor("cgn-o")
s_dur_o$register = as.factor("stories")
s_dur_o$mean_hnr = as.factor(s_dur_o$mean_hnr)
s_dur_o$nn_end_score = as.factor(s_dur_o$nn_end_score)

s_dur = rbind(s_dur_a, s_dur_c, s_dur_d, s_dur_ifadv, s_dur_ecsd, s_dur_o, s_dur_k)
s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)
#s_dur = s_dur[!(s_dur$mean_hnr == "--undefined--"),]
#s_dur$mean_hnr = as.numeric(s_dur$mean_hnr)
s_dur = s_dur[!(s_dur$nn_end_score == "--undefined--"),]
#s_dur$nn_end_score = as.numeric(s_dur$nn_end_score) / 1000
s_dur$nn_end_score = as.numeric(as.character(s_dur$nn_end_score))
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

s_dur$s_dur_kal = s_dur$kal_end - s_dur$kal_start
s_dur$s_dur_nn = s_dur$nn_end - s_dur$kal_start + 0.01
plot(density(s_dur$s_dur_kal), xlim = c(0, 0.5), lty = "dashed")
lines(density(s_dur$s_dur_nn))

# remove lines for which measurements cannot be trusted
s_dur = s_dur[!(is.na(s_dur$s_dur_nn) | is.na(s_dur$s_cog_window)), ]
s_dur = s_dur[!(s_dur$s_dur_nn < 0.005),]
is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron
# either limit based op preceding and following sounds
s_dur = s_dur[!(s_dur$prev_phon_pron %in% c("t", "d") | s_dur$next_phon_pron %in% c("j", "t", "d")),]
# or based on nn scores
plot(density(s_dur$nn_end_score))
s_dur = s_dur[
  s_dur$nn_start_score > 1.2 & 
    s_dur$nn_end_score > 1.2
  ,]
# remove words transcribed as only /s/
#s_dur = s_dur[s_dur$word_phon != "s",]
# remove underlyingly voiced final /s/ to check if it causes the effect
#s_dur = s_dur[s_dur$underlying_voice == "voiceless",]
s_dur = s_dur[s_dur$underlying_voice %in% c("voiced", "voiceless"),]
s_dur$underlying_voice = as.factor(as.character(s_dur$underlying_voice))
# remove NA lines
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

# remove unrepresentative outliers (Baayen, 2008, p. 243)
s_dur = s_dur[s_dur$s_dur_nn < 0.4,]

# make new predictors and get rid of unnecessary NAs
s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)
s_dur$stress_dist = s_dur$num_syl - s_dur$word_stress

s_dur$log_bigf = log(s_dur$bigram_f + 1)

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
plot(density(s_dur$log_s_dur))

# remove lines for which continuous predictors are NA
nrow(s_dur)
s_dur = s_dur[!(is.na(s_dur$speech_rate_pron) | is.na(s_dur$base_dur) 
#                | is.na(s_dur$rel_freq1) 
#                | is.na(s_dur$rel_freq2)
                | is.na(s_dur$num_syl_pron) | is.na(s_dur$num_cons_pron)
                | is.na(s_dur$log_wf) | is.na(s_dur$lex_neb) | is.na(s_dur$log_bigf)
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

# Get centred and scaled predictors
s_dur$speech_rate_pron_sc = scale(s_dur$speech_rate_pron)
s_dur$base_dur_sc = scale(s_dur$base_dur)
s_dur$num_syl_pron_sc = scale(s_dur$num_syl_pron)
s_dur$num_cons_pron_sc = scale(s_dur$num_cons_pron)
s_dur$log_wf_sc = scale(s_dur$log_wf)
s_dur$lex_neb_sc = scale(s_dur$lex_neb)
s_dur$log_bigf_sc = scale(s_dur$log_bigf)
#s_dur$rel_freq1_sc = scale(s_dur$rel_freq1)
#s_dur$rel_freq2_sc = scale(s_dur$rel_freq2)
#s_dur$stress_dist_sc = scale(s_dur$stress_dist)
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


s_dur = s_dur[s_dur$type_of_s %in% c("S", "PL"),]
s_dur$type_of_s = as.factor(as.character(s_dur$type_of_s))
s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")


# remove lines for which categorical predictors are NA
nrow(s_dur)
s_dur = s_dur[!(is.na(s_dur$type_of_s) | is.na(s_dur$next_phon_class) 
                | is.na(s_dur$prev_mention) | is.na(s_dur$register) 
                | is.na(s_dur$stressed) | is.na(s_dur$underlying_voice)
), ]
nrow(s_dur)

### prepare dataset
# get rid of unnecessary columns
drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced2", "per_mil_wf", "prev_word",
#         "rel_freq1", "rel_freq2", 
         "stress_dist",
         "ndl_boundary_diph", "other_ndl_cues",
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "chan")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

s_dur = na.omit(s_dur)

### Inspect collinearity
continuous = c("speech_rate_pron", "base_dur", "num_syl_pron", 
               "num_cons_pron", "log_wf", "lex_neb", "log_bigf",
               "syntax_f2", "syntax_f3", "syntax_f4", "syntax_f5",
               "syntax_f6", "syntax_f7", "syntax_f8")

corrplot(cor(s_dur[, continuous], use = "complete.obs"), method = "number")
vif(lm(log_s_dur ~ speech_rate_pron + base_dur + num_syl_pron 
       + num_cons_pron + log_wf + lex_neb + log_bigf , data=s_dur))
collin.fnc(na.omit(s_dur[, continuous]))$cnumber

categorical = c("type_of_s", 
                "register", 
                "next_phon_class", 
                "prev_mention", "phrase_final", "underlying_voice", "stressed")

cat_ass = matrix(c(cramerV(table(s_dur[,c("type_of_s", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("next_phon_class", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("prev_mention", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("phrase_final", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("underlying_voice", "stressed")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "prev_mention")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("stressed", "stressed")]), bias.correct = TRUE)),
                 nrow = 7, ncol = 7, byrow = T, dimnames = list(
                   categorical,
                   categorical))

corrplot(cat_ass, method = "number")

cat_con = matrix(c(sqrt(summary(lm(speech_rate_pron ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(base_dur ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_bigf ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f5 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f6 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f7 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f8 ~ type_of_s, data = s_dur))$r.squared),
                   #                   sqrt(summary(lm(rel_freq1 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(base_dur ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_bigf ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f5 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f6 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f7 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f8 ~ register, data = s_dur))$r.squared),
                   #                   sqrt(summary(lm(rel_freq1 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(base_dur ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_bigf ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f5 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f6 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f7 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f8 ~ next_phon_class, data = s_dur))$r.squared),
                   #                   sqrt(summary(lm(rel_freq1 ~ next_phon_class, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(base_dur ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_bigf ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f5 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f6 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f7 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f8 ~ prev_mention, data = s_dur))$r.squared),
                   #                   sqrt(summary(lm(rel_freq1 ~ prev_mention, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(base_dur ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_bigf ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f5 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f6 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f7 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f8 ~ phrase_final, data = s_dur))$r.squared),
                   #                   sqrt(summary(lm(rel_freq1 ~ phrase_final, data = s_dur))$r.squared),
                   sqrt(summary(lm(speech_rate_pron ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(base_dur ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_bigf ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f5 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f6 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f7 ~ underlying_voice, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f8 ~ underlying_voice, data = s_dur))$r.squared),
                   #                   sqrt(summary(lm(rel_freq1 ~ underlying_voice, data = s_dur))$r.squared)
                   sqrt(summary(lm(speech_rate_pron ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(base_dur ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_bigf ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f5 ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f6 ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f7 ~ stressed, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f8 ~ stressed, data = s_dur))$r.squared)
), 
nrow = 7, ncol = 14, byrow = T, dimnames = list(
  categorical,
  continuous))

corrplot(cat_con, method = "number")

# predictors of interest

int_ass = matrix(c(cramerV(table(s_dur[,c("type_of_s", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "prev_mention")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("type_of_s", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "stressed")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(base_dur ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_bigf ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ type_of_s, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ type_of_s, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("type_of_s", "syntax_f5_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "syntax_f6_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "syntax_f7_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("type_of_s", "syntax_f8_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "type_of_s")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "register")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "next_phon_class")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "prev_mention")]), bias.correct = TRUE),
#                   cramerV(table(s_dur[,c("register", "phrase_final")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "underlying_voice")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "stressed")]), bias.correct = TRUE),
                   sqrt(summary(lm(speech_rate_pron ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(base_dur ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_syl_pron ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(num_cons_pron ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_wf ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(lex_neb ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(log_bigf ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f2 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f3 ~ register, data = s_dur))$r.squared),
                   sqrt(summary(lm(syntax_f4 ~ register, data = s_dur))$r.squared),
                   cramerV(table(s_dur[,c("register", "syntax_f5_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "syntax_f6_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "syntax_f7_cat")]), bias.correct = TRUE),
                   cramerV(table(s_dur[,c("register", "syntax_f8_cat")]), bias.correct = TRUE)
), 
nrow = 2, ncol = 20, byrow = T, dimnames = list(
  c("Type of S", "Register"),
  c("Type of S", "Register", "Phonetic class of next phone", 
    "Previous mention of word", 
#    "phrase_final", 
    "Underlying voice of S", "Word stress on final syllable", 
    "Speech rate", "Duration of word base", "Number of syllables", 
    "Number of consonants in coda", "Word frequency", "Phonological neighbourhood density", "Bigram frequency",
    "Syntax feature 1", "Syntax feature 2", "Syntax feature 3", "Syntax feature 4",
    "Syntax feature 5", "Syntax feature 6", "Syntax feature 7")))

par(oma=c(3,3,3,3))
corrplot(int_ass, method = "number", mar = c(0,0,0,0), tl.srt = 45, cl.lim = c(0,1), cl.pos = "b", cl.ratio = 1)
par(oma=c(0,0,0,0))

### try Mirjam's residuals method
### Now try with scaled and centred predictors instead of PCs

control2 = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
               + num_cons_pron_sc + log_wf_sc + lex_neb_sc + log_bigf_sc 
               + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
               + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat 
               + stressed
               + next_phon_class + prev_mention 
               + underlying_voice
#               + type_of_s*register
               + (1 | speaker) 
               + (1 | word_ort),
               control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
               data=s_dur)

s_dur$dur_resid2 = resid(control2)
s_dur_trim2 = s_dur[abs(scale(s_dur$dur_resid2)) < 2.5,]

control_trim2 = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
                     + num_cons_pron_sc + log_wf_sc + lex_neb_sc + log_bigf_sc 
                     + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
                     + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat
                     + stressed
                     + next_phon_class + prev_mention 
                     + underlying_voice
#                     + type_of_s*register
                     + (1 | speaker) 
                     + (1 | word_ort),
                     control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                     data=s_dur_trim2)

# interest_conv = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
#                      + num_cons_pron_sc + log_wf_sc + lex_neb_sc + log_bigf_sc 
#                      + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc + syntax_f5_sc + syntax_f6_sc 
#                      + syntax_f7_sc + syntax_f8_sc
#                      + stressed
#                      + next_phon_class + prev_mention 
#                      + underlying_voice
#                      + type_of_s
#                      + (1 | speaker) + (1 | word_ort),
#                      control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
#                      data=s_dur_trim2[s_dur_trim2$register == "conversation",])

s_dur_trim2$dur_resid = resid(control_trim2)

#interest2 = lm(dur_resid ~ type_of_s + type_of_s:register,
#               data=s_dur_trim2)

interest2 = lm(dur_resid ~ type_of_s*register,
              data=s_dur_trim2)

anova(interest2)

p1 = plot(effect("type_of_s:register", interest2, x.var = "type_of_s"), multiline = T, 
     ci.style = "bars",
     main = "", 
     xlab = "Morphological status", ylab = "Residuals in log(seconds)")


par(mfrow=c(2,2))
plot(predict(control_trim), resid(control_trim))
qqnorm(resid(control_trim), main = "PCs")
qqline(resid(control_trim))
plot(predict(control_trim2), resid(control_trim2))
qqnorm(resid(control_trim2), main = "Original")
qqline(resid(control_trim2))
par(mfrow=c(1,1))

##### residuals are identical if all PCs are included
interest_conv = lm(dur_resid ~ type_of_s,
                   data=s_dur_trim2[s_dur_trim2$register == "conversation",])
summary(interest_conv)

interest_stor = lm(dur_resid ~ type_of_s,
                   data=s_dur_trim2[s_dur_trim2$register == "stories",])
summary(interest_stor)

interest_news = lm(dur_resid ~ type_of_s,
                   data=s_dur_trim2[s_dur_trim2$register == "news",])
summary(interest_news)

# Back transform
s_dur_trim2$dur_pred_lm = predict(interest2)
s_dur_trim2$dur_pred_lm_back = s_dur_trim2$s_dur_nn - exp(s_dur_trim2$log_s_dur - s_dur_trim2$dur_resid)


mean_conv_s = mean(s_dur_trim2[s_dur_trim2$register == "conversation" & s_dur_trim2$type_of_s == "S",]$dur_pred_lm_back)
mean_conv_pl = mean(s_dur_trim2[s_dur_trim2$register == "conversation" & s_dur_trim2$type_of_s == "PL",]$dur_pred_lm_back)
mean_stor_s = mean(s_dur_trim2[s_dur_trim2$register == "stories" & s_dur_trim2$type_of_s == "S",]$dur_pred_lm_back)
mean_stor_pl = mean(s_dur_trim2[s_dur_trim2$register == "stories" & s_dur_trim2$type_of_s == "PL",]$dur_pred_lm_back)
mean_news_s = mean(s_dur_trim2[s_dur_trim2$register == "news" & s_dur_trim2$type_of_s == "S",]$dur_pred_lm_back)
mean_news_pl = mean(s_dur_trim2[s_dur_trim2$register == "news" & s_dur_trim2$type_of_s == "PL",]$dur_pred_lm_back)

mean_conv_s - mean_conv_pl
mean_stor_s - mean_stor_pl
mean_news_s - mean_news_pl

#
# s_dur_conv = s_dur[s_dur$register == "conversation",]
# control_conv = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
#                 + num_cons_pron_sc + log_wf_sc + lex_neb_sc + log_bigf_sc 
#                 + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
#                 + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat 
#                 + stressed
#                 + next_phon_class + prev_mention 
#                 + underlying_voice
# #                + rel_freq1
#                 + (1 | speaker) 
#                 + (1 | word_ort),
#                 control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
#                 data=s_dur_conv)
# 
# s_dur_conv$dur_resid = resid(control_conv)
# s_dur_conv_trim = s_dur_conv[abs(scale(s_dur_conv$dur_resid)) < 2.5,]
# 
# control_conv_trim = lmer(log_s_dur ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
#                     + num_cons_pron_sc + log_wf_sc + lex_neb_sc + log_bigf_sc 
#                     + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
#                     + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat 
#                     + stressed
#                     + next_phon_class + prev_mention 
#                     + underlying_voice
# #                    + type_of_s
#                     + (1 | speaker) 
#                     + (1 | word_ort),
#                     control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
#                     data=s_dur_conv_trim)
# 
# s_dur_conv_trim$dur_resid2 = resid(control_conv_trim)
# 
# interest_conv = lm(dur_resid2 ~ type_of_s,
#                data=s_dur_conv_trim)

#
# Centre of Gravity

control_cog2 = lmer(s_cog_window ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
                    + num_cons_pron_sc + log_wf_sc + lex_neb_sc + log_bigf_sc 
                    + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
#                    + syntax_f5_sc + syntax_f6_sc + syntax_f7_sc + syntax_f8_sc
                    + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat
                    + stressed
                    + next_phon_class + prev_mention + underlying_voice
#                    + type_of_s*register
                   + (1 | speaker) 
                   + (1 | word_ort),
                   control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                   data=s_dur)

s_dur$cog_resid2 = resid(control_cog2)
s_cog_trim2 = s_dur[abs(scale(s_dur$cog_resid2)) < 2.5,]

control_cog_trim2 = lmer(s_cog_window ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
                         + num_cons_pron_sc + log_wf_sc + lex_neb_sc + log_bigf_sc 
                         + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc 
#                         + syntax_f5_sc + syntax_f6_sc + syntax_f7_sc + syntax_f8_sc
                         + syntax_f5_cat + syntax_f6_cat + syntax_f7_cat + syntax_f8_cat
                         + stressed
                         + next_phon_class + prev_mention + underlying_voice
#                         + type_of_s*register
                        + (1 | speaker) 
                        + (1 | word_ort),
                        control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
                        data=s_cog_trim2)

# interest_cog_conv = lmer(s_cog_window ~ speech_rate_pron_sc + base_dur_sc + num_syl_pron_sc 
#                          + num_cons_pron_sc + log_wf_sc + lex_neb_sc + log_bigf_sc 
#                          + syntax_f2_sc + syntax_f3_sc + syntax_f4_sc + syntax_f5_sc + syntax_f6_sc 
#                          + syntax_f7_sc + syntax_f8_sc
#                          + stressed
#                          + next_phon_class + prev_mention + underlying_voice
#                          + type_of_s
#                          + (1 | speaker) + (1 | word_ort),
#                          control = lmerControl(optCtrl = list(maxfun = 1e6, ftol_abs = 1e-8)),
#                          data=s_cog_trim2[s_cog_trim2$register == "news",])

par(mfrow=c(2,2))
plot(predict(control_cog2), resid(control_cog2))
qqnorm(resid(control_cog2), main = "PCs")
qqline(resid(control_cog2))
plot(predict(control_cog_trim2), resid(control_cog_trim2))
qqnorm(resid(control_cog_trim2), main = "Original")
qqline(resid(control_cog_trim2))
par(mfrow=c(1,1))

s_cog_trim2$cog_resid = resid(control_cog_trim2)

#interest_cog2 = lm(cog_resid ~ type_of_s + type_of_s:register,
#                  data=s_cog_trim2)

interest_cog2 = lm(cog_resid ~ type_of_s*register,
                  data=s_cog_trim2)

anova(interest_cog2)

p2 = plot(effect("type_of_s:register", interest_cog2, x.var = "type_of_s"), multiline = T, 
     ci.style = "bars",
     main = "", 
     xlab = "Morphological status", ylab = "Residuals in Hz")

##

grid.arrange(p1, p2, ncol = 2)

interest_cog_conv = lm(cog_resid ~ type_of_s,
                       data=s_cog_trim2[s_cog_trim2$register == "conversation",])
summary(interest_cog_conv)

interest_cog_stor = lm(cog_resid ~ type_of_s,
                       data=s_cog_trim2[s_cog_trim2$register == "stories",])
summary(interest_cog_stor)

interest_cog_news = lm(cog_resid ~ type_of_s,
                       data=s_cog_trim2[s_cog_trim2$register == "news",])
summary(interest_cog_news)

# again differences are small (~25Hz), but two staged approach, and only 0-4000Hz range
cog_pred = effect("type_of_s:register", interest_cog2, x.var = "type_of_s")$fit

# conversation
cog_pred[1] - cog_pred[2]

# stories
cog_pred[3] - cog_pred[4]

# news
cog_pred[5] - cog_pred[6]

