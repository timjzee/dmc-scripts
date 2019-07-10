library(lmerTest)
library(effects)
library(lsmeans)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/"
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
} else {
  f_path = "/vol/tensusers/timzee/"
  cgn_path = "/vol/tensusers/timzee/cgn/"
}

print("Loading comp-a")
s_dur_a = read.csv(paste(cgn_path, "comp-a_s_static_final.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
print("Loading comp-c")
s_dur_c = read.csv(paste(cgn_path, "comp-c_s_static_final.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)

print("Prepping data")

s_dur = rbind(s_dur_a, s_dur_c)
s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)

s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)
s_dur$stress_dist = s_dur$num_syl - s_dur$word_stress

is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron

# get rid of unnecessary columns
drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced", "proportion_voiced2", "s_cog_full", "s_cog_window", "per_mil_wf", 
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", 
         "chan")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

# Get rid of GEN-TIME
s_dur = s_dur[s_dur$type_of_s != "GEN-TIME",]
s_dur$type_of_s = as.factor(as.character(s_dur$type_of_s))
s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")

# don't create unnnecessary NAs
#is.na(s_dur$bigram_f) = !s_dur$bigram_f
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

# see https://statisticalhorizons.com/multicollinearity
# V has most cases by far for the prev_phon_class
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

# don't create unnecessary NAs
s_dur[is.na(s_dur$lex_neb_freq),]$lex_neb_freq = 0
#s_dur$lex_neb_freq[s_dur$lex_neb_freq == 0] = NA
#n_lexicon = 251563
s_dur$prop_lex_neb_freq = s_dur$lex_neb_freq / s_dur$wf

s_dur$mean_syl_dur = s_dur$base_dur / s_dur$num_syl_pron

# transform dependent variable
s_dur$log_s_dur = log10(s_dur$s_dur)

drop = c("lex_neb", "prev_phon_pron", "next_phon_pron", "lex_neb_freq", "bigram_f", "num_syl", 
         "word_stress", "base_dur", "num_syl_pron", "log_bigf", "prev_phon_class",
         "wf", "stressed")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

# get prepped & cleaned separate data sets
s_dur = na.omit(s_dur)
s_dur_a = s_dur[s_dur$corpus == "cgn-a",]
s_dur_c = s_dur[s_dur$corpus == "cgn-c",]

# get and merge with wrd boundaries
print("Merging with wrd boundary data")
wb_a = read.csv(paste(cgn_path, "comp-a_word_boundaries.csv", sep = ""))
wb_c = read.csv(paste(cgn_path, "comp-c_word_boundaries.csv", sep = ""))
s_wb_a = merge(wb_a, s_dur_a)
s_wb_c = merge(wb_c, s_dur_c)
s_wb = rbind(s_wb_a, s_wb_c)

drop = c("awd_start", "kaldi_start", "wrd_start", "wrd_awd_start", "wrd_kaldi_start", "awd_end", "wrd_awd_end", 
         "awd_tran", "kaldi_tran", "wrd_tran", "raw_word")
s_wb = s_wb[ , !(names(s_wb) %in% drop)]

# take a look at the numbers:
table(s_wb$corpus, s_wb$type_of_s)

s_wb$time_to_end = s_wb$kaldi_end - s_wb$wrd_end

# get rid of outliers
s_wb_end = s_wb[s_wb$wrd_kaldi_end > -0.2 & s_wb$wrd_kaldi_end < 0.2, ]


m2 = lmer(time_to_end ~ speech_rate_pron + mean_syl_dur 
          + num_cons_pron + log_wf + prop_lex_neb_freq + p_next_w + stress_dist 
          + next_phon_class + prev_mention + phrase_final 
          + corpus*type_of_s
          + (1|word_ort) + (1|speaker), data = s_wb_end)

plot(fitted(m2), residuals(m2))
plot(density(residuals(m2)))
qqnorm(residuals(m2))
qqline(residuals(m2))

summary(m2)
plot(effect("corpus:type_of_s", m2), alternating = F, rotx = 45)
anova(m2, type = 2)

# split data

s_wb_a = s_wb_end[s_wb_end$corpus == "cgn-a",]
s_wb_c = s_wb_end[s_wb_end$corpus == "cgn-c",]

# separate models

m_a = lmer(time_to_end ~ speech_rate_pron + mean_syl_dur 
           + num_cons_pron + log_wf + prop_lex_neb_freq + p_next_w + stress_dist 
           + next_phon_class + prev_mention + phrase_final 
           + type_of_s
           + (1|word_ort) + (1|speaker), data = s_wb_a)

anova(m_a, type = 2)
lsmeans(m_a, pairwise~type_of_s)

m_c = lmer(time_to_end ~ speech_rate_pron + mean_syl_dur 
           + num_cons_pron + log_wf + prop_lex_neb_freq + p_next_w + stress_dist 
           + next_phon_class + prev_mention + phrase_final 
           + type_of_s
           + (1|word_ort) + (1|speaker), data = s_wb_c)

anova(m_c, type = 2)
lsmeans(m_c, pairwise~type_of_s)



wb_a = read.csv(paste(cgn_path, "comp-a_word_boundaries.csv", sep = ""))

s_dur_a = read.csv(paste(cgn_path, "comp-a_s_static.csv", sep = ""))

s_wb_a = merge(wb_a, s_dur_a)
s_wb_a$corpus = as.factor("comp-a")
plot(density(s_wb_a$wrd_awd_end, bw = 0.02, na.rm = TRUE), xlim = c(-0.1,0.1))
lines(density(s_wb_a$wrd_kaldi_end, bw = 0.02, na.rm = TRUE), xlim = c(-0.1,0.1), lty = "dashed")

wb_c = read.csv(paste(cgn_path, "comp-c_word_boundaries.csv", sep = ""))
s_dur_c = read.csv(paste(cgn_path, "comp-c_s_static.csv", sep = ""))
s_wb_c = merge(wb_c, s_dur_c)
s_wb_c$corpus = as.factor("comp-c")

s_wb = rbind(s_wb_a, s_wb_c)

s_wb$prev_mention = as.factor(s_wb$prev_mention)
s_wb$phrase_final = as.factor(s_wb$phrase_final)
s_wb$stressed = s_wb$num_syl == s_wb$word_stress
s_wb$stressed = as.factor(s_wb$stressed)
# fix mistake from add_info_script
s_wb$stress_dist = s_wb$num_syl - s_wb$word_stress
#s_wb$stress_dist[s_wb$stress_dist < 0] = NA
#s_wb$stressed[s_wb$stress_dist < 0] = NA
#s_wb$num_syl[s_wb$stress_dist < 0] = NA
is.na(s_wb$num_syl_pron) = !s_wb$num_syl_pron
s_wb$type_of_s = relevel(s_wb$type_of_s, ref="S")

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

s_wb$next_phon_class = as.factor(sapply(s_wb$next_phon_pron, get_phon_class))
s_wb$prev_phon_class = as.factor(sapply(s_wb$prev_phon_pron, get_phon_class))

# get rid of unnecessary columns
drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced", "proportion_voiced2", "s_cog_full", "s_cog_window", "per_mil_wf", 
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "chan", "raw_word")
s_wb = s_wb[ , !(names(s_wb) %in% drop)]

s_wb$time_to_end = s_wb$kaldi_end - wb$wrd_end

# get rid of outliers
s_wb_end = s_wb[s_wb$wrd_kaldi_end > -0.2 & s_wb$wrd_kaldi_end < 0.2, ]

drop = c("awd_start", "kaldi_start", "wrd_start", "wrd_awd_start", "wrd_kaldi_start", "awd_end", "wrd_awd_end", 
         "awd_tran", "kaldi_tran", "wrd_tran", "log_wf", "lex_neb", "lex_neb_freq", "bigram_f", "num_syl",
         "word_stress", "base_dur", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "chan", "raw_word")
s_wb = s_wb[ , !(names(s_wb) %in% drop)]

# take a look at the numbers:
table(s_wb_end$type_of_s, s_wb_end$corpus)

m1 = lmer(time_to_end ~ corpus + (1|word_ort) + (1|speaker), data = s_wb_end)
plot(effect("corpus", m1))

# kaldi set boundary later than manual in comp-a compared to comp-c

m2 = lmer(wrd_kaldi_end ~ corpus*type_of_s + speech_rate_pron + next_phon_class 
          + (1|word_ort) + (1|speaker), data = s_wb_end)
summary(m2)
plot(effect("corpus:type_of_s", m2))
# Kaldi sets the PART and PL boundaries later than the S boundaries in comp-a.
# In comp-c, PART, PL and S boundaries are similar. Unexpected direction. check word starts

s_wb_start = s_wb[s_wb$wrd_kaldi_start > -0.2 & s_wb$wrd_kaldi_start < 0.2, ]
table(s_wb_start$type_of_s, s_wb_start$corpus)
m3 = lmer(wrd_kaldi_start ~ corpus*type_of_s + (1|word_ort) + (1|speaker), data = s_wb_start)
plot(effect("corpus:type_of_s", m3))

# NOPE
