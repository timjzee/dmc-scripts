if (Sys.info()[1] == "Darwin"){
  f_path <- "/Volumes/tensusers/timzee/classifier_evaluation/en/"
} else {
  f_path <- "/vol/tensusers/timzee/classifier_evaluation/en/"
}

d <- read.csv(paste(f_path, "nn_eval_en_o1_annotated.csv", sep = ""))

d$TS_schwa_start <- ifelse(test = d$TS_._reduction == "D", 0, d$TS_._start_e - d$TS_._start_b)
d$TS_schwa_end <- ifelse(test = d$TS_._reduction == "D", 0, d$TS_._end_e - d$TS_._end_b)
d$TS_schwa_max <- ifelse(test = d$TS_._reduction == "D", 0, d$TS_._end_b - d$TS_._start_e)
d$TS_schwa_start_end <- d$TS_schwa_start + d$TS_schwa_end
d$TS_schwa_start_max <- d$TS_schwa_start + d$TS_schwa_max
d$TS_schwa_max_end <- d$TS_schwa_max + d$TS_schwa_end
d$TS_schwa_start_max_end <- d$TS_schwa_start + d$TS_schwa_max + d$TS_schwa_end
d$TS_schwa_start_prop <- d$TS_schwa_start / (d$TS_schwa_start + d$TS_schwa_max)
d$TS_schwa_end_prop <- d$TS_schwa_end / (d$TS_schwa_end + d$TS_schwa_max)
d$TS_schwa_start_end_prop <- (d$TS_schwa_start + d$TS_schwa_end) / (d$TS_schwa_start + d$TS_schwa_end + d$TS_schwa_max)

library(ordinal)
d_m = d[d$TS_._reduction != "D",]
d_m$TS_._reduction = as.factor(as.character(d_m$TS_._reduction))
m_TS_schwa_start = clm(TS_._reduction ~ TS_schwa_start, data = d_m)
m_TS_schwa_end = clm(TS_._reduction ~ TS_schwa_end, data = d_m)
m_TS_schwa_max = clm(TS_._reduction ~ TS_schwa_max, data = d_m)
m_TS_schwa_start_end = clm(TS_._reduction ~ TS_schwa_start_end, data = d_m)
m_TS_schwa_start_max = clm(TS_._reduction ~ TS_schwa_start_max, data = d_m)
m_TS_schwa_max_end = clm(TS_._reduction ~ TS_schwa_max_end, data = d_m)
m_TS_schwa_start_max_end = clm(TS_._reduction ~ TS_schwa_start_max_end, data = d_m)
m_TS_schwa_start_prop = clm(TS_._reduction ~ TS_schwa_start_prop, data = d_m)
m_TS_schwa_end_prop = clm(TS_._reduction ~ TS_schwa_end_prop, data = d_m)
m_TS_schwa_start_end_prop = clm(TS_._reduction ~ TS_schwa_start_end_prop, data = d_m)

par(mfrow = c(2,5))
plot(d_m$TS_._reduction, d_m$TS_schwa_start)
text(1, max(d_m$TS_schwa_start), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_schwa_start), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_._reduction, d_m$TS_schwa_end)
text(1, max(d_m$TS_schwa_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_schwa_end), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_._reduction, d_m$TS_schwa_max)
text(1, max(d_m$TS_schwa_max), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_schwa_max), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_._reduction, d_m$TS_schwa_start_end)
text(1, max(d_m$TS_schwa_start_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_schwa_start_end), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_._reduction, d_m$TS_schwa_start_max)
text(1, max(d_m$TS_schwa_start_max), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_schwa_start_max), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_._reduction, d_m$TS_schwa_max_end)
text(1, max(d_m$TS_schwa_max_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_schwa_max_end), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_._reduction, d_m$TS_schwa_start_max_end)
text(1, max(d_m$TS_schwa_start_max_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_schwa_start_max_end), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_._reduction, d_m$TS_schwa_start_prop)
text(1, max(d_m$TS_schwa_start_prop), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_schwa_start_prop), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_._reduction, d_m$TS_schwa_end_prop)
text(1, max(d_m$TS_schwa_end_prop), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_schwa_end_prop), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_._reduction, d_m$TS_schwa_start_end_prop)
text(1, max(d_m$TS_schwa_start_end_prop), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_schwa_start_end_prop), digits = 0)), sep = "")), col = "red")


d$TS_n_start <- ifelse(test = d$TS_n_reduction == "D", 0, d$TS_n_start_e - d$TS_n_start_b)
d$TS_n_end <- ifelse(test = d$TS_n_reduction == "D", 0, d$TS_n_end_e - d$TS_n_end_b)
d$TS_n_max <- ifelse(test = d$TS_n_reduction == "D", 0, d$TS_n_end_b - d$TS_n_start_e)
d$TS_n_start_end <- d$TS_n_start + d$TS_n_end
d$TS_n_start_max <- d$TS_n_start + d$TS_n_max
d$TS_n_max_end <- d$TS_n_max + d$TS_n_end
d$TS_n_start_max_end <- d$TS_n_start + d$TS_n_max + d$TS_n_end
d$TS_n_start_prop <- d$TS_n_start / (d$TS_n_start + d$TS_n_max)
d$TS_n_end_prop <- d$TS_n_end / (d$TS_n_end + d$TS_n_max)
d$TS_n_start_end_prop <- (d$TS_n_start + d$TS_n_end) / (d$TS_n_start + d$TS_n_end + d$TS_n_max)

d_m = d[d$TS_n_reduction != "D",]
d_m$TS_n_reduction = as.factor(as.character(d_m$TS_n_reduction))
m_TS_n_start = clm(TS_n_reduction ~ TS_n_start, data = d_m)
m_TS_n_end = clm(TS_n_reduction ~ TS_n_end, data = d_m)
m_TS_n_max = clm(TS_n_reduction ~ TS_n_max, data = d_m)
m_TS_n_start_end = clm(TS_n_reduction ~ TS_n_start_end, data = d_m)
m_TS_n_start_max = clm(TS_n_reduction ~ TS_n_start_max, data = d_m)
m_TS_n_max_end = clm(TS_n_reduction ~ TS_n_max_end, data = d_m)
m_TS_n_start_max_end = clm(TS_n_reduction ~ TS_n_start_max_end, data = d_m)
m_TS_n_start_prop = clm(TS_n_reduction ~ TS_n_start_prop, data = d_m)
m_TS_n_end_prop = clm(TS_n_reduction ~ TS_n_end_prop, data = d_m)
m_TS_n_start_end_prop = clm(TS_n_reduction ~ TS_n_start_end_prop, data = d_m)



par(mfrow = c(2,5))
plot(d_m$TS_n_reduction, d_m$TS_n_start)
text(1, max(d_m$TS_n_start), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_n_start), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_n_reduction, d_m$TS_n_end)
text(1, max(d_m$TS_n_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_n_end), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_n_reduction, d_m$TS_n_max)
text(1, max(d_m$TS_n_max), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_n_max), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_n_reduction, d_m$TS_n_start_end)
text(1, max(d_m$TS_n_start_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_n_start_end), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_n_reduction, d_m$TS_n_start_max)
text(1, max(d_m$TS_n_start_max), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_n_start_max), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_n_reduction, d_m$TS_n_max_end)
text(1, max(d_m$TS_n_max_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_n_max_end), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_n_reduction, d_m$TS_n_start_max_end)
text(1, max(d_m$TS_n_start_max_end), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_n_start_max_end), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_n_reduction, d_m$TS_n_start_prop)
text(1, max(d_m$TS_n_start_prop), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_n_start_prop), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_n_reduction, d_m$TS_n_end_prop)
text(1, max(d_m$TS_n_end_prop), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_n_end_prop), digits = 0)), sep = "")), col = "red")
plot(d_m$TS_n_reduction, d_m$TS_n_start_end_prop)
text(1, max(d_m$TS_n_start_end_prop), labels = c(paste("AIC: ", as.character(round(AIC(m_TS_n_start_end_prop), digits = 0)), sep = "")), col = "red")
par(mfrow = c(1,1))


d$TS_N_dur <- ifelse(test = is.na(d$TS_N_e) | is.na(d$TS_N_b), 0, d$TS_N_e - d$TS_N_b)
d$TS_schwa_n_overlap <- ifelse(test = d$TS_n_reduction == "D" | d$TS_._reduction == "D", NA, d$TS_._end_e - d$TS_n_start_b)
d$TS_schwa_N_overlap <- ifelse(test = d$TS_._reduction == "D" | d$TS_N_dur == 0, 0, 
                               ifelse(test = (d$TS_N_e < d$TS_._end_e) & (d$TS_N_b >= d$TS_._start_b), d$TS_N_dur, 
                                      ifelse(test = (d$TS_N_e < d$TS_._end_e) & (d$TS_N_b < d$TS_._start_b), d$TS_N_e - d$TS_._start_b, 
                                             ifelse(test = (d$TS_N_e >= d$TS_._end_e) & (d$TS_N_b >= d$TS_._start_b), d$TS_._end_e - d$TS_N_b, d$TS_._end_e - d$TS_._start_b))))

plot(as.factor(as.character(d[d$TS_n_reduction != "D",]$TS_n_reduction)), d[d$TS_n_reduction != "D",]$TS_schwa_n_overlap)
plot(as.factor(d$suffix_variant), d$TS_schwa_start_max_end, ylab="Schwa duration", xlab="Variant According to Tim")
plot(as.factor(d$suffix_variant), d$TS_schwa_n_overlap)
plot(d$TS_schwa_start_max_end, d$TS_schwa_n_overlap)
plot(as.factor(d[d$suffix_variant %in% c("@", "@n", "~"),]$suffix_variant), d[d$suffix_variant %in% c("@", "@n", "~"),]$TS_schwa_N_overlap)

d$TS_schwa_N_overlap_prop = d$TS_schwa_N_overlap / d$TS_schwa_start_max_end
plot(as.factor(d[d$suffix_variant %in% c("@", "@n", "~"),]$suffix_variant), d[d$suffix_variant %in% c("@", "@n", "~"),]$TS_schwa_N_overlap_prop, ylab="Proportion of Schwa Nasalized", xlab="Variant According to Tim")


d$suffix_variant <- as.factor(d$suffix_variant)
d$TS_n_reduction <- as.factor(d$TS_n_reduction)
d$TS_._reduction <- as.factor(d$TS_._reduction)

xtabs(~ TS_n_reduction + TS_._reduction, data = d)
xtabs(~ suffix_variant + TS_._reduction, data = d)
xtabs(~ suffix_variant + TS_n_reduction, data = d)

xtabs(TS_schwa_start_max_end ~ suffix_variant, data = d)
xtabs(TS_N_dur ~ suffix_variant, data = d)


d$TS_total <- ifelse(test = d$TS_._reduction == "D" & d$TS_n_reduction == "D", 0, 
                     ifelse(test = d$TS_._reduction != "D" & d$TS_n_reduction != "D", d$TS_n_end_e - d$TS_._start_b, 
                            ifelse(test = d$TS_._reduction != "D", d$TS_._end_e - d$TS_._start_b, d$TS_n_end_e - d$TS_n_start_b)))
d$TS_schwa_prop <- d$TS_schwa_start_max_end / d$TS_total

plot(as.factor(d$suffix_variant), d$TS_total, ylab="Total suffix duration", xlab="Variant According to Tim")

plot(as.factor(as.character(d[d$TS_._reduction != "D",]$TS_._reduction)), d[d$TS_._reduction != "D",]$TS_schwa_prop)

d$TS_n_prop <- d$TS_n_start_max_end / d$TS_total

plot(as.factor(as.character(d[d$TS_n_reduction != "D",]$TS_n_reduction)), d[d$TS_n_reduction != "D",]$TS_n_prop)

