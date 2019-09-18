library(ndl)
library(dplyr)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/IFADVcorpus/"
  ifadv_path = "/Volumes/tensusers/timzee/IFADVcorpus/"
} else {
  f_path = "/vol/tensusers/timzee/IFADVcorpus/"
  ifadv_path = "/vol/tensusers/timzee/IFADVcorpus/"
}

get_NDLOutcome = function(x) {
  if (x == "S") {
    return("NONMORPH")
  } else if (x == "PL") {
    return("PL")
  } else if (x == "GEN-POSS") {
    return("GENPOSS")
  } else {
    return("PART")
  }
}

get_ActFromRemainingCues = function(cues_str, outcome) {
  cues = strsplit(cues_str, "_")
  sum_act = sum(sapply(cues, function(z) ifadv.w[z, outcome]))
  return(sum_act)
}

get_ActDivFromRemainingCues = function(cues_str) {
  cues = strsplit(cues_str, "_")
  sum_act = sum(sapply(cues, function(z) ifadv.w[z,]))
  return(sum_act)
}


# Plag & Balling example
# Word <- c("baptize", "chance", "extreme", "modernize", "optimal", 
#           "optimize", "sand", "size")
# Outcomes <- c("baptize", "chance", "extreme", "modern_make", "optimal", 
#               "optimal_make", "sand", "size")
# Frequency <- c(37, 12303, 558, 6, 15, 5, 1035, 2353)
# ize <- data.frame(Word, Outcomes, Frequency)
# ize$Cues <- orthoCoding(ize$Word, grams=2)
# ize
# ize.w = estimateWeights(ize)
# round(ize.w, 3)
# ize.a = estimateActivations(ize, ize.w)$activationMatrix
# rownames(ize.a) <- ize[,"Word"]

# IFADV data

# ifadv = read.csv(paste(f_path, "ndl_ifadv.csv", sep = ""))
# ifadv = ifadv %>% count(Cues, Outcomes)
# names(ifadv) = c("Cues", "Outcomes", "Frequency")
# ifadv.w = estimateWeights(ifadv)
# 
# saveRDS(ifadv.w, file = paste(f_path, "ifadv_ndl.rds", sep = ""))

ifadv.w = readRDS(paste(f_path, "ifadv_ndl.rds", sep = ""))

# Load /s/ measurments
s_dur = read.csv(paste(ifadv_path, "ifadv_s_static_ndl.csv", sep = ""))
levels(s_dur$speaker_sex) = c("sex2", "sex1")
s_dur = s_dur[s_dur$type_of_s != "GEN-TIME",]
s_dur$type_of_s = as.factor(as.character(s_dur$type_of_s))
s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")

# get NDL measures
s_dur$ndl_outcome = sapply(as.character(s_dur$type_of_s), get_NDLOutcome)
s_dur$priorMorph = sapply(s_dur$ndl_outcome, function(x) sum(ifadv.w[,x]))
s_dur$ActFromBoundaryDiphone = mapply(function(x, y) ifadv.w[x, y], 
                                      as.character(s_dur$ndl_boundary_diph), 
                                      s_dur$ndl_outcome)
s_dur$ActFromRemainingCues = mapply(get_ActFromRemainingCues,
                                    as.character(s_dur$other_ndl_cues),
                                    s_dur$ndl_outcome)
s_dur$ActDivFromBoundaryDiphone = sapply(as.character(s_dur$ndl_boundary_diph),
                                         function(x) sum(ifadv.w[x,]))
s_dur$ActDivFromRemainingCues = sapply(as.character(s_dur$other_ndl_cues),
                                       get_ActDivFromRemainingCues)

#################################################### Make some models
library(lmerTest)
library(effects)

s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)

s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)
s_dur$stress_dist = s_dur$num_syl - s_dur$word_stress

is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron

drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced", "proportion_voiced2", "s_cog_full", "s_cog_window", "per_mil_wf", 
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i", "word_chunk_i", 
         "chan")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]


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

intercepts1 = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron 
                   + next_phon_class 
                   + type_of_s
                   + (1 | speaker) + (1 | word_ort), 
                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
                   data=s_dur)

intercepts2 = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron 
                   + next_phon_class 
                   + priorMorph + ActFromBoundaryDiphone + ActDivFromBoundaryDiphone 
                   + ActFromRemainingCues + ActDivFromRemainingCues
                   + (1 | speaker) + (1 | word_ort), 
                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
                   data=s_dur)

plot(effect("priorMorph", intercepts2))
