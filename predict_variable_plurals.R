library(sjPlot)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/other/"
} else {
  f_path = "/vol/tensusers/timzee/other/"
}

korte_vocalen = c("I", "E", "A", "O", "}")
lange_vocalen = c("i", "y", "e", "|", "a", "o", "u")
voor_vocalen = c("I", "E", "}", "i", "e", "|", "y")
achter_vocalen = c("A", "O", "o", "u", "a")
diftongen = c("K", "L", "M")
schwa = c("@")
leen_vocalen = c(")", "*", "<", "!", "(")
klinkers = c(korte_vocalen, lange_vocalen, diftongen, schwa, leen_vocalen)
occlusieven = c("p", "b", "t", "d", "k", "g")
fricatieven = c("f", "v", "s", "z", "S", "Z", "x", "G", "h")
obstruenten = c(occlusieven, fricatieven)
nasalen = c("m", "n", "N")
liquidae = c("l", "r")
halfvocalen = c("j", "w")
sonoranten = c(nasalen, liquidae, halfvocalen)
sonoranten_cluster = c()
for (s1 in sonoranten) {
  for (s2 in sonoranten) {
    sonoranten_cluster = c(sonoranten_cluster, paste(s1, s2, sep = ""))
  }
}
medeklinkers = c(occlusieven, fricatieven, nasalen, liquidae, halfvocalen)

get_phon_template = function(wrd, ns, n1, o2, n2, c2, fin_phon, s2) {
  if (fin_phon %in% obstruenten) {
    return("obstruent")
  } else if (o2 == "j" & n2 == "@" & c2 == "=" & substr(wrd, nchar(wrd) - 1, nchar(wrd)) == "je" & !(substr(wrd, nchar(wrd) - 2, nchar(wrd) - 2) %in% c("j", "n"))) {
    return("tje")
  } else if ((n2 %in% diftongen & c2 == "=") | (n2 %in% c(lange_vocalen, diftongen) & c2 %in% halfvocalen)) {
    return("diph")
  } else if ((n2 %in% c(lange_vocalen, diftongen) & c2 %in% sonoranten) | (n2 %in% korte_vocalen & c2 %in% sonoranten_cluster)) {
    return("long_vowel_son")
  } else if ((((n2 %in% korte_vocalen & c2 %in% sonoranten) | (n2 %in% voor_vocalen & c2 == "=")) & n1 == "=") | (n2 %in% korte_vocalen & c2 %in% sonoranten & n1 != "=" & s2 == "+" & ns == 1)) {
    return("short_vowel_son")
  } else if (n2 %in% korte_vocalen & c2 == "N" & n2 == "I" & n1 != "=" & s2 == "-") {
    return("ing")
  } else if (n2 %in% voor_vocalen & c2 == "=") {
    return("front_vowel")
  } else if (n2 %in% achter_vocalen & c2 == "=") {
    return("back_vowel")
  } else if (c2 %in% sonoranten & n2 == "@") {
    return("schwa_son")
  } else if (n2 %in% korte_vocalen & c2 %in% sonoranten & n1 != "=" & s2 == "-") {
    return("unstressed_vowel_son")
  } else if (c2 == "=" & n2 == "@") {
    return("schwa")
  } else if (n2 %in% korte_vocalen & c2 %in% sonoranten & n1 != "=" & s2 == "+") {
    return("stressed_vowel_son")
  } else {
    return("other")
  }
}

get_default_suffix = function(temp) {
  if (temp == "obstruent") {
    return("en")
  } else if (temp == "diph") {
    return("en")
  } else if (temp == "long_vowel_son") {
    return("en")
  } else if (temp == "short_vowel_son") {
    return("en")
  } else if (temp == "ing") {
    return("en")
  } else if (temp == "tje") {
    return("s")
  } else if (temp == "front_vowel") {
    return("s")
  } else if (temp == "back_vowel") {
    return("s")
  } else if (temp == "schwa_son") {
    return("s")
  } else if (temp == "unstressed_vowel_son") {
    return("s")
  } else if (temp == "schwa") {
    return("s")
  } else if (temp == "stressed_vowel_son") {
    return("none")
  } else {
    return("none")
  }
}

get_incongruency = function(congr, en_f, other_f, s_f) {
  if (congr == "en") {
    return(((s_f + other_f) / (en_f + other_f + s_f)) * 100)
  } else if (congr == "s") {
    return(((en_f + other_f) / (en_f + other_f + s_f)) * 100)
  } else {
    return(NA)
  }
}

get_variable_percentage = function(var_f, en_f, other_f, s_f) {
  return((var_f / (en_f + other_f + s_f + var_f)) * 100)
}

invar = read.csv(paste(f_path, "pl_type.master", sep = ""), header = F)
names(invar) = c("word", "right_compound_syls", "penult_onset", "penult_nucleus", "penult_coda", "final_onset", "final_nucleus", "final_coda", "penult_stress", "final_stress", "final_letter", "plural")
invar$final_coda = as.character(invar$final_coda)
invar$template = mapply(get_phon_template, as.character(invar$word), invar$right_compound_syls, invar$penult_nucleus, invar$final_onset, invar$final_nucleus, invar$final_coda, substr(invar$final_coda, nchar(invar$final_coda), nchar(invar$final_coda)), invar$final_stress)
invar$template = as.factor(invar$template)
invar$default = sapply(invar$template, get_default_suffix)

abc = table(invar$template, invar$plural)
templates = names(abc[,1])
en = abc[,1]
other = abc[,2]
s = abc[,3]
default = c("s", "en", "s", "en", "en", "en", "none", "s", "s", "en", "none", "s", "s")
table_order = c("obstruent", "diph", "long_vowel_son", "short_vowel_son", "ing", "front_vowel", "back_vowel", "schwa_son", "unstressed_vowel_son", "schwa", "tje", "stressed_vowel_son", "other")
temp_table = data.frame(default, en, other, s)
temp_table$percentage_incongruent = mapply(get_incongruency, temp_table$default, temp_table$en, temp_table$other, temp_table$s)
tab_df(temp_table[table_order,])

var = read.csv(paste(f_path, "compare_p_f_k5.csv", sep = ""))

var$final_coda = as.character(var$final_coda)
var$template = mapply(get_phon_template, as.character(var$word), var$right_compound_syls, var$penult_nucleus, var$final_onset, var$final_nucleus, var$final_coda, substr(var$final_coda, nchar(var$final_coda), nchar(var$final_coda)), var$final_stress)
var$template = as.factor(var$template)
var$default = sapply(var$template, get_default_suffix)

tje = 0
names(tje) = c('tje')
temp_table$variable_types = c(table(var$template)[1:11], tje, table(var$template)[12])
temp_table$percentage_variable = mapply(get_variable_percentage, temp_table$variable_types, temp_table$en, temp_table$other, temp_table$s)
tab_df(temp_table[table_order,])

temp_table2 = temp_table
names(temp_table2) = c("default", "en", "other", "s", "% incongruent", "variable", "% variable")
temp_table2["% incongruent"] = round(temp_table2["% incongruent"], 2)
temp_table2["% variable"] = round(temp_table2["% variable"], 2)
tab_df(temp_table2[table_order,])

plot(temp_table[temp_table$default != "none",]$percentage_incongruent, temp_table[temp_table$default != "none",]$percentage_variable, type = "n", xlab = "Percentage incongruent", ylab = "Percentage variable")
text(temp_table[temp_table$default != "none",]$percentage_incongruent, temp_table[temp_table$default != "none",]$percentage_variable, row.names(temp_table[temp_table$default != "none",]))
cor(temp_table[temp_table$default != "none",]$percentage_incongruent, temp_table[temp_table$default != "none",]$percentage_variable)

var$s_prop = var$f_s/(var$f_s + var$f_en)

plot(var$p_s, var$s_prop, type = "n")
text(var$p_s, var$s_prop, var$word)
cor(var$p_s, var$s_prop)

## check correlation when trained on tokens
var_token = read.csv(paste(f_path, "compare_p_f_tokens.csv", sep = ""))

var_token$final_coda = as.character(var_token$final_coda)
var_token$template = mapply(get_phon_template, var_token$right_compound_syls, var_token$penult_nucleus, var_token$final_nucleus, var_token$final_coda, substr(var_token$final_coda, nchar(var_token$final_coda), nchar(var_token$final_coda)), var_token$final_stress)
var_token$template = as.factor(var_token$template)
var_token$default = sapply(var_token$template, get_default_suffix)

var_token$s_prop = var_token$f_s/(var_token$f_s + var_token$f_en)
cor(var_token$p_s, var_token$s_prop)

# also train on other variable plurals
var_var = read.csv(paste(f_path, "compare_p_f_var.csv", sep = ""))

var_var$final_coda = as.character(var_var$final_coda)
var_var$template = mapply(get_phon_template, var_var$right_compound_syls, var_var$penult_nucleus, var_var$final_nucleus, var_var$final_coda, substr(var_var$final_coda, nchar(var_var$final_coda), nchar(var_var$final_coda)), var_var$final_stress)
var_var$template = as.factor(var_var$template)
var_var$default = sapply(var_var$template, get_default_suffix)

var_var$s_prop = var_var$f_s/(var_var$f_s + var_var$f_en)
cor(var_var$p_s, var_var$s_prop)

# not better, but all numbers might be different at a smaller k
# k2 without other variable plurals
var_k2 = read.csv(paste(f_path, "compare_p_f_k2.csv", sep = ""))

var_k2$final_coda = as.character(var_k2$final_coda)
var_k2$template = mapply(get_phon_template, as.character(var_k2$word), var_k2$right_compound_syls, var_k2$penult_nucleus, var_k2$final_onset, var_k2$final_nucleus, var_k2$final_coda, substr(var_k2$final_coda, nchar(var_k2$final_coda), nchar(var_k2$final_coda)), var_k2$final_stress)
var_k2$template = as.factor(var_k2$template)
var_k2$default = sapply(var_k2$template, get_default_suffix)

var_k2$s_prop = var_k2$f_s/(var_k2$f_s + var_k2$f_en)

plot(var_k2$p_s, var_k2$s_prop, type = "n")
text(var_k2$p_s, var_k2$s_prop, var_k2$word)
cor(var_k2$p_s, var_k2$s_prop)

# k2 with other variable plurals
var_var_k2 = read.csv(paste(f_path, "compare_p_f_var_k2.csv", sep = ""))

var_var_k2$final_coda = as.character(var_var_k2$final_coda)
var_var_k2$template = mapply(get_phon_template, var_var_k2$right_compound_syls, var_var_k2$penult_nucleus, var_var_k2$final_nucleus, var_var_k2$final_coda, substr(var_var_k2$final_coda, nchar(var_var_k2$final_coda), nchar(var_var_k2$final_coda)), var_var_k2$final_stress)
var_var_k2$template = as.factor(var_var_k2$template)
var_var_k2$default = sapply(var_var_k2$template, get_default_suffix)

var_var_k2$s_prop = var_var_k2$f_s/(var_var_k2$f_s + var_var_k2$f_en)

plot(var_var_k2$p_s, var_var_k2$s_prop, type = "n")
text(var_var_k2$p_s, var_var_k2$s_prop, var_var_k2$word)
cor(var_var_k2$p_s, var_var_k2$s_prop)

## Distance weighting
# Inv. Distance Decay
var_k2_ID = read.csv(paste(f_path, "compare_p_f_k2_ID.csv", sep = ""))

var_k2_ID$final_coda = as.character(var_k2_ID$final_coda)
var_k2_ID$template = mapply(get_phon_template, as.character(var_k2_ID$word), var_k2_ID$right_compound_syls, var_k2_ID$penult_nucleus, var_k2_ID$final_onset, var_k2_ID$final_nucleus, var_k2_ID$final_coda, substr(var_k2_ID$final_coda, nchar(var_k2_ID$final_coda), nchar(var_k2_ID$final_coda)), var_k2_ID$final_stress)
var_k2_ID$template = as.factor(var_k2_ID$template)
var_k2_ID$default = sapply(var_k2_ID$template, get_default_suffix)

var_k2_ID$s_prop = var_k2_ID$f_s/(var_k2_ID$f_s + var_k2_ID$f_en)

plot(var_k2_ID$p_s, var_k2_ID$s_prop, type = "n")
text(var_k2_ID$p_s, var_k2_ID$s_prop, var_k2_ID$word)
cor(var_k2_ID$p_s, var_k2_ID$s_prop)

# with ID and variables
var_k2_ID_var = read.csv(paste(f_path, "compare_p_f_k2_ID_var.csv", sep = ""))
var_k2_ID_var$final_coda = as.character(var_k2_ID_var$final_coda)
var_k2_ID_var$template = mapply(get_phon_template, as.character(var_k2_ID_var$word), var_k2_ID_var$right_compound_syls, var_k2_ID_var$penult_nucleus, var_k2_ID_var$final_nucleus, var_k2_ID_var$final_onset, var_k2_ID_var$final_coda, substr(var_k2_ID_var$final_coda, nchar(var_k2_ID_var$final_coda), nchar(var_k2_ID_var$final_coda)), var_k2_ID_var$final_stress)
var_k2_ID_var$template = as.factor(var_k2_ID_var$template)
var_k2_ID_var$default = sapply(var_k2_ID_var$template, get_default_suffix)

var_k2_ID_var$s_prop = var_k2_ID_var$f_s/(var_k2_ID_var$f_s + var_k2_ID_var$f_en)

plot(var_k2_ID_var$p_s, var_k2_ID_var$s_prop, type = "n")
text(var_k2_ID_var$p_s, var_k2_ID_var$s_prop, var_k2_ID_var$word)
cor(var_k2_ID_var$p_s, var_k2_ID_var$s_prop)

# with ID and type merge
var_k2_ID_merge = read.csv(paste(f_path, "compare_p_f_k2_ID_merge.csv", sep = ""))
var_k2_ID_merge$final_coda = as.character(var_k2_ID_merge$final_coda)
var_k2_ID_merge$template = mapply(get_phon_template, as.character(var_k2_ID_merge$word), var_k2_ID_merge$right_compound_syls, var_k2_ID_merge$penult_nucleus, var_k2_ID_merge$final_nucleus, var_k2_ID_merge$final_onset, var_k2_ID_merge$final_coda, substr(var_k2_ID_merge$final_coda, nchar(var_k2_ID_merge$final_coda), nchar(var_k2_ID_merge$final_coda)), var_k2_ID_merge$final_stress)
var_k2_ID_merge$template = as.factor(var_k2_ID_merge$template)
var_k2_ID_merge$default = sapply(var_k2_ID_merge$template, get_default_suffix)

var_k2_ID_merge$s_prop = var_k2_ID_merge$f_s/(var_k2_ID_merge$f_s + var_k2_ID_merge$f_en)

plot(var_k2_ID_merge$p_s, var_k2_ID_merge$s_prop, type = "n")
text(var_k2_ID_merge$p_s, var_k2_ID_merge$s_prop, var_k2_ID_merge$word)
cor(var_k2_ID_merge$p_s, var_k2_ID_merge$s_prop)

# with ID and tokens
var_k2_ID_tokens = read.csv(paste(f_path, "compare_p_f_k2_ID_tokens.csv", sep = ""))

var_k2_ID_tokens$final_coda = as.character(var_k2_ID_tokens$final_coda)
var_k2_ID_tokens$template = mapply(get_phon_template, as.character(var_k2_ID_tokens$word), var_k2_ID_tokens$right_compound_syls, var_k2_ID_tokens$penult_nucleus, var_k2_ID_tokens$final_onset, var_k2_ID_tokens$final_nucleus, var_k2_ID_tokens$final_coda, substr(var_k2_ID_tokens$final_coda, nchar(var_k2_ID_tokens$final_coda), nchar(var_k2_ID_tokens$final_coda)), var_k2_ID_tokens$final_stress)
var_k2_ID_tokens$template = as.factor(var_k2_ID_tokens$template)
var_k2_ID_tokens$default = sapply(var_k2_ID_tokens$template, get_default_suffix)

var_k2_ID_tokens$s_prop = var_k2_ID_tokens$f_s/(var_k2_ID_tokens$f_s + var_k2_ID_tokens$f_en)

plot(var_k2_ID_tokens$p_s, var_k2_ID_tokens$s_prop, type = "n")
text(var_k2_ID_tokens$p_s, var_k2_ID_tokens$s_prop, var_k2_ID_tokens$word)
cor(var_k2_ID_tokens$p_s, var_k2_ID_tokens$s_prop)

# It's not the learning parameters, let's investigate:
var_k2_ID_sub = var_k2_ID[var_k2_ID$s_prop < 0.8 & var_k2_ID$s_prop > 0.2,]
plot(var_k2_ID_sub$p_s, var_k2_ID_sub$s_prop, type = "n")
text(var_k2_ID_sub$p_s, var_k2_ID_sub$s_prop, var_k2_ID_sub$word)
cor(var_k2_ID_sub$p_s, var_k2_ID_sub$s_prop)

var_k2_ID_sub2 = var_k2_ID[var_k2_ID$s_prop >= 0.8 | var_k2_ID$s_prop <= 0.2,]
cor(var_k2_ID_sub2$p_s, var_k2_ID_sub2$s_prop)

library(lattice)
xyplot(s_prop~p_s,
       data=var_k2_ID,
       groups=template, auto.key = list(columns = 6, cex = 1))

# let's check Mirjam's theory
# including suitable verb stems in training data with EN as class
var_k2_ID_verbsEN = read.csv(paste(f_path, "compare_p_f_k2_ID_verbs-EN.csv", sep = ""))
var_k2_ID_verbsEN$final_coda = as.character(var_k2_ID_verbsEN$final_coda)
var_k2_ID_verbsEN$template = mapply(get_phon_template, as.character(var_k2_ID_verbsEN$word), var_k2_ID_verbsEN$right_compound_syls, var_k2_ID_verbsEN$penult_nucleus, var_k2_ID_verbsEN$final_nucleus, var_k2_ID_verbsEN$final_onset, var_k2_ID_verbsEN$final_coda, substr(var_k2_ID_verbsEN$final_coda, nchar(var_k2_ID_verbsEN$final_coda), nchar(var_k2_ID_verbsEN$final_coda)), var_k2_ID_verbsEN$final_stress)
var_k2_ID_verbsEN$template = as.factor(var_k2_ID_verbsEN$template)
var_k2_ID_verbsEN$default = sapply(var_k2_ID_verbsEN$template, get_default_suffix)

var_k2_ID_verbsEN$s_prop = var_k2_ID_verbsEN$f_s/(var_k2_ID_verbsEN$f_s + var_k2_ID_verbsEN$f_en)

plot(var_k2_ID_verbsEN$p_s, var_k2_ID_verbsEN$s_prop, type = "n")
text(var_k2_ID_verbsEN$p_s, var_k2_ID_verbsEN$s_prop, var_k2_ID_verbsEN$word)
cor(var_k2_ID_verbsEN$p_s, var_k2_ID_verbsEN$s_prop)

# including suitable verb stems in training data with VERB as class
var_k2_ID_verbsVERB = read.csv(paste(f_path, "compare_p_f_k2_ID_verbs-VERB.csv", sep = ""))
var_k2_ID_verbsVERB$final_coda = as.character(var_k2_ID_verbsVERB$final_coda)
var_k2_ID_verbsVERB$template = mapply(get_phon_template, as.character(var_k2_ID_verbsVERB$word), var_k2_ID_verbsVERB$right_compound_syls, var_k2_ID_verbsVERB$penult_nucleus, var_k2_ID_verbsVERB$final_nucleus, var_k2_ID_verbsVERB$final_onset, var_k2_ID_verbsVERB$final_coda, substr(var_k2_ID_verbsVERB$final_coda, nchar(var_k2_ID_verbsVERB$final_coda), nchar(var_k2_ID_verbsVERB$final_coda)), var_k2_ID_verbsVERB$final_stress)
var_k2_ID_verbsVERB$template = as.factor(var_k2_ID_verbsVERB$template)
var_k2_ID_verbsVERB$default = sapply(var_k2_ID_verbsVERB$template, get_default_suffix)

var_k2_ID_verbsVERB$s_prop = var_k2_ID_verbsVERB$f_s/(var_k2_ID_verbsVERB$f_s + var_k2_ID_verbsVERB$f_en)

plot(var_k2_ID_verbsVERB$p_s, var_k2_ID_verbsVERB$s_prop, type = "n")
text(var_k2_ID_verbsVERB$p_s, var_k2_ID_verbsVERB$s_prop, var_k2_ID_verbsVERB$word)
cor(var_k2_ID_verbsVERB$p_s, var_k2_ID_verbsVERB$s_prop)

