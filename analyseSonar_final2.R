if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/"
} else {
  f_path = "/vol/tensusers/timzee/"
}

library(rethinking)
library(stringr)
library(dagitty)
##
dagPL <- dagitty("dag {
  Plural [outcome]
  Stress [exposure]
  Break
  Sound
  U [unobserved]
  Style
  Singular
  ID [unobserved]
  Break -> Plural <- Stress
  Break <- Stress
  Sound -> Plural <- Style
  Singular -> Plural <- ID
  Stress <- U -> Sound
}")

coordinates(dagPL) <- list( x=c(Break=1,Stress=1,Plural=2,U=1,Sound=2,Style=3,Singular=3,ID=3), 
                            y=c(Break=3,Stress=2,Plural=2,U=1,Sound=1,Style=1,Singular=2,ID=3) ) 
drawdag( dagPL )
par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1))
adjustmentSets(dagPL)
# alternative hypothesis
dagSt <- dagitty("dag {
  Plural [exposure]
  Stress [outcome]
  Break
  Sound
  U [unobserved]
  Style
  Singular
  ID [unobserved]
  Plural -> Stress <- Break
  Sound <- Plural <- Style
  Singular -> Plural <- ID
  Stress <- U -> Sound
}")

coordinates(dagSt) <- list( x=c(Break=1,Stress=1,Plural=2,U=1,Sound=2,Style=3,Singular=3,ID=3), 
                            y=c(Break=3,Stress=2,Plural=2,U=1,Sound=1,Style=1,Singular=2,ID=3) ) 
drawdag( dagSt )
par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1))
adjustmentSets(dagSt)


##
dagPL2a <- dagitty("dag {
  Plural [outcome]
  Stress [exposure]
  Break
  Sound
  U [unobserved]
  Style
  Singular
  ID [unobserved]
  Break -> Plural <- Stress
  Sound <- Break -> Stress
  Sound -> Plural <- Style
  Singular -> Plural <- ID
  Stress <- U -> Sound
}")

dagPL2b <- dagitty("dag {
  Plural [outcome]
  Stress [exposure]
  Break
  Sound
  U [unobserved]
  Style
  Singular
  ID [unobserved]
  Break -> Plural <- Stress
  Sound -> Break <- Stress
  Sound -> Plural <- Style
  Singular -> Plural <- ID
  Stress <- U -> Sound
}")

# difference between:
#   Sound -> Break <- Stress
#   Sound <- Break -> Stress
# reflects which comes first:
#   word/lemma selection
#   syntactic structure
# we can stay agnostic about it by doing:
#   U -> Break, U -> Stress, U -> Sound
# regardless, it doesn't matter for the adjustment sets
# we can't stay agnostic about causality between Stress and Plural though:
#   Stress -> Plural
#   Stress <- Plural
# Represents two different processes in sentence formation
#   a structure of word forms with phonological representations and breaks exists that influences
#     which morphological forms are chosen (right to left influence)
#   Phonological structure of final forms affect which following word is chosen
# but maybe it isn't helpful to do U -> Break
#   U is very abstract in that conceptualisation
#     Stress <- U -> Sound is very clear: stress and sound co-occur in words
#   We may need to consider both directions in Sound ~ Break ~ Stress anyway when
#   we look at Plural -> Stress
coordinates(dagPL2a) <- list( x=c(Break=2,Stress=2,Plural=3,U=1,Sound=2,Style=4,Singular=4,ID=4), 
                            y=c(Break=2,Stress=1,Plural=2,U=2,Sound=3,Style=1,Singular=2,ID=3) ) 
coordinates(dagPL2b) <- list( x=c(Break=2,Stress=2,Plural=3,U=1,Sound=2,Style=4,Singular=4,ID=4), 
                              y=c(Break=2,Stress=1,Plural=2,U=2,Sound=3,Style=1,Singular=2,ID=3) ) 
par(mfrow=c(1,2))
drawdag( dagPL2a )
drawdag( dagPL2b )
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1))
adjustmentSets(dagPL2)
impliedConditionalIndependencies(dagPL2)

library(ggdag)
library(gridExtra)
library(tibble)
# helpful: https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-dags.html
dagPL2a_plot <- ggdag(dagPL2a) + ggtitle("hey")
dagPL2b_plot <- ggdag(dagPL2b) + ggtitle("you")
grid.arrange(dagPL2a_plot, dagPL2b_plot, ncol=2)

# scratch all that
# mechanism 1
# - Break influences Plural through an interaction with stress
# - in this case we have all lexemes in place in the syntactic structure
#   which may or may not contain a break
# - Break does not have a causal relation to Stress or Sound
# mechanism 2 lexical selection
# - Plural influences Stress through an interaction with Break
# - in this case we have the syntactic structure in place, the plural variant
#   in place, but the lexeme following the plural is not yet decided 
#   (i.e. a choice has to be made between different lexemes with similar meaning,
#   e.g. vampieren doden/vermoorden mensen)
# - Plural does not have a causal relation to Break
# mechanism 2b word ordering
# - Plural influences Stress through an interaction with Break
# - in this case only the plural variant and the general message is chosen, 
#   but the structure/ordering and lexemes are not yet decided. Stress represents the
#   ordering/structure (different orderings would have resulted in a different 
#   following word and thus stress)
#   (i.e. a choice has to be made between different phrases with dif. structure and words,
#   e.g. Zonlicht is gevaarlijk voor vampiers. Vampieren kunnen niet tegen zonlicht.
#   e.g. Vampiers, zo gaat het verhaal, kunnen niet tegen zonlicht. Vampieren kunnen niet tegen zonlicht, zo gaat het verhaal.

mech2bsub <- dagitty("dag {
  Plural [exposure]
  Stress [outcome]
  Break
  Stress <- Plural -> Break
  Break -> Stress
}")
drawdag( mech2bsub )
adjustmentSets(mech2bsub, effect = "direct")
# mechanism ???
# - Plural influences break through an interaction with Stress
# - This is hard to imagine, because usually the removal of a ortographic break
#   does not result in a legal structure (.?, often can't just be removed)

# mech 1
mech1_dag <- dagify(Plural ~ Break + Stress + Sound + ID + Singular + Style,
                    Stress ~ Next_Word,
                    Sound ~ Next_Word,
                    latent = "ID",
                    exposure = c("Stress", "Break"),
                    outcome = "Plural",
                    labels = c("Plural" = "Plural",
                               "Break" = "Break",
                               "Stress" = "Stress",
                               "Sound" = "Sound",
                               "ID" = "ID",
                               "Singular" = "Singular",
                               "Style" = "Style",
                               "Next_Word" = "Next Word"),
                    coords = list( x=c(Break=2,Stress=2,Plural=3,Next_Word=1,Sound=2,Style=4,Singular=4,ID=4), 
                                   y=c(Break=2,Stress=3,Plural=2,Next_Word=2,Sound=1,Style=3,Singular=2,ID=1) ))

mech1_dag_tidy <- tidy_dagitty(mech1_dag)

mech1_dag_tidy$data = mech1_dag_tidy$data %>% 
  add_column(ntype=ifelse(mech1_dag_tidy$data$name %in% latents(mech1_dag), "latent",
                          ifelse(mech1_dag_tidy$data$name %in% outcomes(mech1_dag), "outcome",
                                 ifelse(mech1_dag_tidy$data$name %in% exposures(mech1_dag), "exposure", "other"))))

mech1_plot <- mech1_dag_tidy %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = ntype)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_label_repel(aes(label=label), show.legend = F, force = 0, col="black") +
  theme_dag()

adjustmentSets(mech1_dag, effect = "direct")

# mech 2
mech2_dag <- dagify(Plural ~ ID + Singular + Style,
                    Stress ~ Plural + Break,
                    Sound ~ Plural + Break,
                    Next_Word ~ Stress + Sound,
                    latent = "ID",
                    exposure = c("Plural", "Break"),
                    outcome = "Stress",
                    labels = c("Plural" = "Plural",
                               "Break" = "Break",
                               "Stress" = "Stress",
                               "Sound" = "Sound",
                               "ID" = "ID",
                               "Singular" = "Singular",
                               "Style" = "Style",
                               "Next_Word" = "Next Word"),
                    coords = list( x=c(Break=2,Stress=2,Plural=3,Next_Word=1,Sound=2,Style=4,Singular=4,ID=4), 
                                   y=c(Break=2,Stress=3,Plural=2,Next_Word=2,Sound=1,Style=3,Singular=2,ID=1) ))

mech2_dag_tidy <- tidy_dagitty(mech2_dag)

mech2_dag_tidy$data = mech2_dag_tidy$data %>% 
  add_column(ntype=ifelse(mech2_dag_tidy$data$name %in% latents(mech2_dag), "latent",
                          ifelse(mech2_dag_tidy$data$name %in% outcomes(mech2_dag), "outcome",
                                 ifelse(mech2_dag_tidy$data$name %in% exposures(mech2_dag), "exposure", "other"))))

mech2_plot <- mech2_dag_tidy %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = ntype)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_label_repel(aes(label=label), show.legend = F, force = 0, col="black") +
  theme_dag()

adjustmentSets(mech2_dag, effect = "direct")

grid.arrange(mech1_plot, mech2_plot, ncol=2)

## nope got it wrong again: in mech2, plural doesn't influence next_word through
## Stress and Sound, but rather it directly influences Next_word which yields
## Stress and Sound

mech2_dag_alt <- dagify(Plural ~ ID + Singular + Style,
                    Stress ~ Next_Word,
                    Sound ~ Next_Word,
                    Break ~ Plural,
                    Next_Word ~ Break + Plural,
                    latent = "ID",
                    exposure = c("Plural", "Break"),
                    outcome = "Stress",
                    labels = c("Plural" = "Plural",
                               "Break" = "Break",
                               "Stress" = "Stress",
                               "Sound" = "Sound",
                               "ID" = "ID",
                               "Singular" = "Singular",
                               "Style" = "Style",
                               "Next_Word" = "Next Word"),
                    coords = list( x=c(Break=1,Stress=2,Plural=3,Next_Word=1,Sound=2,Style=4,Singular=4,ID=4), 
                                   y=c(Break=3,Stress=2,Plural=2,Next_Word=1,Sound=1,Style=2,Singular=3,ID=1) ))

mech2_dag_alt_tidy <- tidy_dagitty(mech2_dag_alt)

mech2_dag_alt_tidy$data = mech2_dag_alt_tidy$data %>% 
  add_column(ntype=ifelse(mech2_dag_alt_tidy$data$name %in% latents(mech2_dag_alt), "latent",
                          ifelse(mech2_dag_alt_tidy$data$name %in% outcomes(mech2_dag_alt), "outcome",
                                 ifelse(mech2_dag_alt_tidy$data$name %in% exposures(mech2_dag_alt), "exposure", "other"))))

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mech2_alt_plot <- mech2_dag_alt_tidy %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = ntype)) +
  geom_dag_point(show.legend = FALSE) +
  geom_dag_edges() +
  geom_dag_label_repel(aes(label=label), show.legend = F, force = 0, col="black") +
  ggtitle("Lexical/Structural effect") +
  scale_colour_manual(values=cbbPalette) +
  theme_dag(plot.margin = margin(15, 15, 15, 15, "pt"), 
            panel.background = element_rect(fill = "grey95", linetype = "blank"))

adjustmentSets(mech2_dag_alt)

mech1_dag_alt <- dagify(Plural ~ ID + Singular + Style + Break + Stress + Sound,
                        Stress ~ Next_Word,
                        Sound ~ Next_Word,
                        latent = "ID",
                        exposure = c("Stress", "Break"),
                        outcome = "Plural",
                        labels = c("Plural" = "Plural",
                                   "Break" = "Break",
                                   "Stress" = "Stress",
                                   "Sound" = "Sound",
                                   "ID" = "ID",
                                   "Singular" = "Singular",
                                   "Style" = "Style",
                                   "Next_Word" = "Next Word"),
                        coords = list( x=c(Break=1,Stress=2,Plural=3,Next_Word=1,Sound=2,Style=4,Singular=4,ID=4), 
                                       y=c(Break=3,Stress=2,Plural=2,Next_Word=1,Sound=1,Style=2,Singular=3,ID=1) ))

mech1_dag_alt_tidy <- tidy_dagitty(mech1_dag_alt)

mech1_dag_alt_tidy$data = mech1_dag_alt_tidy$data %>% 
  add_column(ntype=ifelse(mech1_dag_alt_tidy$data$name %in% latents(mech1_dag_alt), "latent",
                          ifelse(mech1_dag_alt_tidy$data$name %in% outcomes(mech1_dag_alt), "outcome",
                                 ifelse(mech1_dag_alt_tidy$data$name %in% exposures(mech1_dag_alt), "exposure", "other"))))

mech1_alt_plot <- mech1_dag_alt_tidy %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = ntype)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_label_repel(aes(label=label), show.legend = F, force = 0, col="black") +
  scale_colour_manual(values=cbbPalette) +
  theme_dag()

adjustmentSets(mech1_dag_alt)

legend <- get_legend(mech1_alt_plot)

mech1_alt_plot <- mech1_dag_alt_tidy %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = ntype)) +
  geom_dag_point(show.legend = FALSE) +
  geom_dag_edges() +
  geom_dag_label_repel(aes(label=label), show.legend = F, force = 0, col="black") +
  ggtitle("Morphological effect") +
  scale_colour_manual(values=cbbPalette) +
  theme_dag(plot.margin = margin(15, 15, 15, 15, "pt"), 
            panel.background = element_rect(fill = "grey95", linetype = "blank"))


library(cowplot)
plot_grid(mech1_alt_plot, mech2_alt_plot, legend, ncol = 3, rel_widths = c(2/5, 2/5, 1/5))
#grid.arrange(mech1_alt_plot, mech2_alt_plot, ncol=2)

## wait a minute, what about another possible mechanism:
# Break and Stress influence the choice of lexeme if that lexeme likely results in -s
# perhaps these plurals are not variable at all for these individuals
# e.g., De directeurs heten Jan en Sara. vs. De bazen heten Jan en Sara
# can this mechanism be excluded using the var dataset?
#   - only if we add a Variability --> Plural relation, and the invariable data
#   - i.e. a 3-way interaction between Break, Stress, and Variability
#   - if break and stress only have an influence for variable plurals
#   - the 

# if we conceptualize the DAG as the process within individuals, then the way to
# account for the directeurs/bazen mechanism is to include a latent variable
# for Alternative Lexemes / Synonyms that were considered. This variable is latent
# a priori because we can't look inside the mental processes of the individuals that
# produced the data. This results in a confound for which we cannot control:
# Plural <-- Break/Stress --> Synonyms --> Plural
# However that would only make sense if Plural also included those synonyms
# plural variants.
# if we think generatively such a model still generates a distribution of variable
# plurals which after selection on variability should show an effect of Stress and Break
# Crucially, however, it should also generate a distribution of invariable plurals
# that shows this effect.


# I need to accept that the DAG cannot be used to visualize everything (missing data approach etc.)
# So why not have a direct path from Break/Stress --> PLural, 
# and a backdoor: Break/Stress --> Lexeme --> Plural
# And then specify in the text that to estimate the direct effect Break/Stress --> Plural
# we need to condition on Lexeme. And that would work if our outcome variable
# represented the plural productions of a single person


# btw this alt mechanism could also explain other Quené's findings:
# heel/bijster grote man

# btw potential reason for null effect in experiment: between a Subject and a verb
# might be a location which can easily result in prosodic break.


# haunted DAG interpretation:
haunted_dag <- dagitty("dag {
                    BS [exposure]
                    PL [outcome]
                    LEX
                    ID [unobserved]
                    ID -> LEX <- BS -> PL
                    LEX -> PL <- ID
}")

coordinates(haunted_dag) <- list( x=c(BS=1,PL=3,LEX=2,ID=2), 
                            y=c(BS=3,PL=3,LEX=2,ID=1) ) 
drawdag( haunted_dag )
par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1))

# collider interpretation: 
# let's say there is 0 direct effect from BS to PL
# 2 observations of the same LEXEME, 1 followed by stress 1 not. The only way this
# happens is if they are produced by different people

# let's simulate this





##
d <- read.csv(paste(f_path, "other/SonarVar2.csv", sep = ""), header = T, row.names = NULL)
d_dist <- table(d$item, d$s_plural)
d_dist2 <- d_dist[d_dist[,"0"] != 0 & d_dist[,"1"] != 0,]
d_dist3 <- data.frame(item=row.names(d_dist2), en=d_dist2[,"0"], s=d_dist2[,"1"])
d_dist3$prop <- d_dist3$s / (d_dist3$en + d_dist3$s)
d_dist4 <- d_dist3[d_dist3$prop > 0.05 & d_dist3$prop < 0.95,]

d2 <- d[d$item %in% d_dist4$item,]
d3 <- na.omit(d2)

rm_lines <- c("16145", "34370", "37630", "47134", "90132", "94461", "21343", "94998", "95010")
d3 <- d3[!(row.names(d3) %in% rm_lines),]
d3 <- d3[row.names(d3) != "46707",]

d3$next_sound2 <- ifelse(d3$next_sound %in% c("s", "z"), 1, 
                         ifelse(d3$next_sound %in% c("@", "a", "A", "e", "E", "i", "I", "K", "L", "M", "o", "O", "y"), 2, 3))

d3$sound_interaction <- ifelse(d3$prosodic_break == "1" & d3$next_sound2 == "1", 1,
                               ifelse(d3$prosodic_break == "1" & d3$next_sound2 == "2", 2,
                                      ifelse(d3$prosodic_break == "1" & d3$next_sound2 == "3", 3,
                                             ifelse(d3$prosodic_break == "0" & d3$next_sound2 == "1", 4, 
                                                    ifelse(d3$prosodic_break == "0" & d3$next_sound2 == "2", 5, 6)))))
d3$sound_interaction <- as.integer(d3$sound_interaction)

d3$next_stress2 <- ifelse(d3$next_stress == "1", 1, 2)

d3$stress_interaction <- ifelse(d3$prosodic_break == "1" & d3$next_stress == "1", 1,
                                ifelse(d3$prosodic_break == "1" & d3$next_stress == "0", 2,
                                       ifelse(d3$prosodic_break == "0" & d3$next_stress == "1", 3, 4)))
d3$stress_interaction <- as.integer(d3$stress_interaction)

d3$item_index <- coerce_index(d3$item)

d3$sc <- coerce_index(d3$sub_corpus)

m0a <- readRDS(file = "/Users/tim/Documents/prosody_models/m0a.rds")

m0a <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item] + b[sc] + g[sound] + d[stress],
    g[sound] ~ dnorm(0, 1),
    d[stress] ~ dnorm(0, 1),
    a[item] ~ dnorm(0, sigma_a),
    b[sc] ~ dnorm(0, sigma_b),
    c(sigma_a, sigma_b) ~ dexp(1)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              sc = d3$sc,
              sound = d3$sound_interaction,
              stress = d3$next_stress2
  ), 
  chains = 4, cores = 4, iter = 4000, log_lik = T
)

saveRDS(object = m0a, file = "/Users/tim/Documents/prosody_models/m0a.rds")

m0b <- readRDS(file = "/Users/tim/Documents/prosody_models/m0b.rds")

m0b <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item] + b[sc] + g[sound],
    g[sound] ~ dnorm(0, 1),
    a[item] ~ dnorm(0, sigma_a),
    b[sc] ~ dnorm(0, sigma_b),
    c(sigma_a, sigma_b) ~ dexp(1)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              sc = d3$sc,
              sound = d3$sound_interaction
  ), 
  chains = 4, cores = 4, iter = 4000, log_lik = T
)

saveRDS(object = m0b, file = "/Users/tim/Documents/prosody_models/m0b.rds")

m1 <- readRDS(file = "/Users/tim/Documents/prosody_models/m1.rds")

m1 <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- a[item] + b[sc] + g[sound] + d[stress],
    g[sound] ~ dnorm(0, 1),
    d[stress] ~ dnorm(0, 1),
    a[item] ~ dnorm(0, sigma_a),
    b[sc] ~ dnorm(0, sigma_b),
    c(sigma_a, sigma_b) ~ dexp(1)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              sc = d3$sc,
              sound = d3$sound_interaction,
              stress = d3$stress_interaction
  ), 
  chains = 4, cores = 4, iter = 4000, log_lik = T
)

saveRDS(object = m1, file = "/Users/tim/Documents/prosody_models/m1.rds")

m_comparison <- compare(m0a, m1)
m_comparison <- compare(m0b, m1)

post <- extract.samples(m1)

# /s/ - /n/ contrasts
# prosodic break
break_sn <- post$g[,1] - post$g[,3]
# no break
nobreak_sn <- post$g[,4] - post$g[,6]

plot(density(break_sn))
lines(density(nobreak_sn))

# no difference between boundary - no boundary conditions
# let's see if this effect also exists in invar
# that would indicate that this effect is different in nature


diffs <- list(
  "Boundary" = post$d[,1] - post$d[,2],
  "No Boundary" = post$d[,3] - post$d[,4]
)

boundary_dens <- density(diffs$Boundary)
boundary_dens$ynorm <- boundary_dens$y / (max(boundary_dens$y)*1.2)
noboundary_dens <- density(diffs$`No Boundary`)
noboundary_dens$ynorm <- noboundary_dens$y / (max(noboundary_dens$y)*1.2)

par(mar=c(5,6,4,2) + 0.1)
plot(NULL, xlim=c(min(diffs$`No Boundary`), max(diffs$Boundary)), ylim=c(-0.25,2),yaxt="n",ylab="",xlab="Log-odds difference", main = "Effect of following stress")
axis(2, at = c(0.5, 1.5), labels = c("No Boundary", "Boundary"), las=2)
abline(v=0, lty="dashed")
abline(h=0.1, lty="dotted")
abline(h=1.1, lty="dotted")

arrows(x0 = -0.05, y0 = -0.2, x1 = min(diffs$`No Boundary`) + 0.03, y1 = -0.2, col = "darkgrey", lwd=3, length = 0.15)
text(x=-0.17,y=-0.06,labels = "fewer -s", col = "darkgrey", lwd=2, cex=1.3)
arrows(x0 = 0.05, y0 = -0.2, x1 = max(diffs$Boundary) - 0.03, y1 = -0.2, col = "darkgrey", lwd=3, length = 0.15)
text(x=0.17,y=-0.07,labels = "more -s", col = "darkgrey", lwd=2, cex=1.3)

polygon(x = c(boundary_dens$x, max(boundary_dens$x)), y = c(boundary_dens$ynorm + 1.1, 1.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
polygon(x = c(noboundary_dens$x, max(noboundary_dens$x)), y = c(noboundary_dens$ynorm + 0.1, 0.1), col = adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
par(mar=c(5,4,4,2) + 0.1)

# random slopes
m2 <- ulam(
  alist(
    S ~ dbern(prob),
    # linear model
    logit(prob) <- a[item, stress] + b[sc] + g[sound] + d[stress],
    # adaptive priors
    vector[4]:a[item] ~ multi_normal(0, Rho_a, sigma_a),
    b[sc] ~ dnorm(0, sigma_b),
    # fixed priors
    d[stress] ~ dnorm(0, 1),
    g[sound] ~ dnorm(0, 1),
    c(sigma_a, sigma_b) ~ dexp(1),
    Rho_a ~ dlkjcorr(4)
  ), 
  data = list(S = d3$s_plural, 
              item = d3$item_index, 
              sc = d3$sc,
              sound = d3$sound_interaction,
              stress = d3$stress_interaction
  ), 
  chains = 4, cores = 4, iter = 4000#, log_lik = T
)

saveRDS(object = m2, file = "/Users/tim/Downloads/m2f.rds")

m_post <- extract.samples(m2)

# plot random slopes
p_link_custom <- function( item, treatment ) {
  logodds <- with( m_post , a[,item,treatment] + d[,treatment]) 
  return( inv_logit(logodds) )
}

p_raw <- mapply(p_link_custom, rep(1:max(d3$item_index), times = 4), rep(1:4, each = max(d3$item_index)))

p_ar <- array(data = as.vector(p_raw), dim = c(nrow(m_post$d), max(d3$item_index), 4))




noun_numbers <- table(d3$item, d3$stress_interaction)

library(scales)
coln <- 7
rown <- 3 
par(mfrow=c(rown,1), mar=c(2,3,1,1))
for (k in 0:(rown-1)){
  plot( NULL , xlim=c(1,4*coln) , 
        ylim=c(-0.1, 1) , 
        xlab="" ,
        ylab="" , xaxt="n", yaxt="n" )
  axis(side = 2, labels = c(0, 0.5, 1), at = c(0, 0.5, 1), las=1, cex.axis=0.8)
  text(-2, 0.5, "Proportion -s", xpd=T, srt=90)
  for ( j in 1:(coln-1) ) abline( v=(j-1)*4+4.5 , lwd=0.5 )
  for ( j in 1:coln ) {
    word_name <- d3[d3$item_index == k*coln+j,]$item[1]
    text( (j-1)*4+2.5 , 1.1 , word_name , xpd=TRUE, cex=0.8)
    if ((k+1) %% 2 != 0){
      if (j %in% seq(from = 1, to = coln, by = 2)){
        axis(side = 1, labels = c("B/S", "B/N", "N/S", "N/N"), at = (j-1)*4 + 1:4, cex.axis=0.6, padj = -1.2)
#        text(x = (j-1)*4 + 1:4, y = -0.22, labels = c("B/S", "B/N", "N/S", "N/N"), xpd=T, cex = 0.6)
      }
    } else {
      if (j %in% seq(from = 2, to = coln, by = 2)){
        axis(side = 1, labels = c("B/S", "B/N", "N/S", "N/N"), at = (j-1)*4 + 1:4, cex.axis=0.6, padj = -1.2)
#        text(x = (j-1)*4 + 1:4, y = -0.22, labels = c("B/S", "B/N", "N/S", "N/N"), xpd=T, cex = 0.6)
      }
    }
  }
  # draw in predictions
  for ( j in (1:coln) ) {
    for (i in 1:4) {
      p_mu <- mean(p_ar[,k*coln+j,i])
      p_ci <- PI(p_ar[,k*coln+j,i], prob = .95)
      points(c((j-1)*4 + i), c(p_mu))
      lines(c((j-1)*4 + i, (j-1)*4 + i), p_ci)
    }
    word_name <- d3[d3$item_index == k*coln+j,]$item[1]
    text(x=(1:4+(j-1)*4),y=rep(-0.05, times=4), labels=as.character(noun_numbers[word_name,]),cex=0.6)
  }
  if (k == 0){
    text( 17, PI(p_ar[,5,1], prob = .95)[2] + 0.05 , "B/S" , cex=0.7)
    text( 18 , PI(p_ar[,5,2], prob = .95)[1] - 0.05 , "B/N", cex=0.7 )
    text( 19 , PI(p_ar[,5,3], prob = .95)[2] + 0.05 , "N/S", cex=0.7 )
    text( 20 , PI(p_ar[,5,4], prob = .95)[1] - 0.05 , "N/N", cex=0.7  )
  }
}
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))

par(mar=c(2,2,2,2), mfrow=c(1,1))
plot( NULL, xlim=c(1.52,4*coln - 0.52), ylim=c(0.13,rown*1.2 - 0.13), xlab="", ylab="", xaxt="n", yaxt="n" )
abline( v=0.5 , lwd=0.5 )
abline( v=4*coln + 0.5 , lwd=0.5 )
for ( k in 1:rown ) {
  rect(0.5, k*1.2 - 0.1, 4*coln + 0.5, k*1.2, col = alpha("lightgrey", 1), border = "black")
  if (k %in% seq(from = 1, to = rown, by = 2)){
    axis(side = 2, labels = c(0, 0.5, 1), at = (k-1)*1.2 + c(0.1, 0.6, 1.1), cex.axis=0.6)
  } else {
    axis(side = 4, labels = c(0, 0.5, 1), at = (k-1)*1.2 + c(0.1, 0.6, 1.1), cex.axis=0.6)
  }
}
for ( k in 1:rown) {
  for ( j in 1:coln ) {
    word_name <- d3[d3$item_index == (k-1)*coln+j,]$item[1]
    text( (j-1)*4+2.5 , 1.2*(rown - (k-1) - 0.04), word_name , xpd=TRUE, cex=0.7)
    for (i in 1:4) {
      p_mu <- mean(p_ar[,(k-1)*coln+j,i])
      p_ci <- PI(p_ar[,(k-1)*coln+j,i], prob = .95)
      points(c((j-1)*4 + i), 1.2*rown - 1.2*k + p_mu + 0.1, pch = 16, cex=0.8)
      lines(c((j-1)*4 + i, (j-1)*4 + i), 1.2*rown - 1.2*k + p_ci + 0.1)
    }
    text(x=(1:4+(j-1)*4),y=rep(1.2*rown - 1.2*k + 0.05, times=4), labels=as.character(noun_numbers[word_name,]),cex=0.5)
  }
}
for ( j in 1:coln ) {
  if (j < coln){
    abline( v=(j-1)*4+4.5 , lwd=0.5 )
  }
  if (j %in% seq(from = 1, to = coln, by = 2)){
    axis(side = 3, labels = c("B/S", "B/N", "N/S", "N/N"), at = (j-1)*4 + 1:4, cex.axis=0.5, padj = 1.3)
  } else {
    axis(side = 1, labels = c("B/S", "B/N", "N/S", "N/N"), at = (j-1)*4 + 1:4, cex.axis=0.5, padj = -1.9)
  }
}
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))


# load the invar data
d_invar <- read.csv(paste(f_path, "other/SonarInvar.csv", sep = ""), header = T, row.names = NULL)

d_dist_i <- table(d_invar$item, d_invar$s_plural)

d_invar2 <- na.omit(d_invar)

# d3 still needs to be cleaned manually of a few French sentences
check_french <- d_invar2[d_invar2$item %in% c("cadeau", "bonbon", "auteur", "abonnee", "atelier", "bataljon", "matador") & d_invar2$s_plural == "1",]

# 13476: En bij een bruiloft horen natuurlijk cadeaus .   (duplicate)
# 24137: Auteurs die teveel onleesbare zinnen na elkaar schrijven  beginnen opnieuw .    (duplicate)
# 25970: Ateliers de lat Grosne  Cormatin .     (frans)

# automatically get rid of ALL duplicates:
dups <- rle(d_invar2$ort)
dups_i <- cumsum(c(1, dups$lengths[-length(dups$lengths)]))
d_invar3 <- d_invar2[dups_i,]

# get rid of the french sentence
d_invar3 <- d_invar3[!(row.names(d_invar3) %in% c("25970")),]

d_invar3$stress_interaction <- ifelse(d_invar3$prosodic_break == "1" & d_invar3$next_stress == "1", 1,
                                      ifelse(d_invar3$prosodic_break == "1" & d_invar3$next_stress == "0", 2,
                                             ifelse(d_invar3$prosodic_break == "0" & d_invar3$next_stress == "1", 3, 4)))
d_invar3$stress_interaction <- as.integer(d_invar3$stress_interaction)
d_invar3$item_index <- coerce_index(d_invar3$item)

d_invar3$pros_pl_interaction <- ifelse(d_invar3$prosodic_break == "1" & d_invar3$s_plural == "1", 1,
                                       ifelse(d_invar3$prosodic_break == "1" & d_invar3$s_plural == "0", 2,
                                              ifelse(d_invar3$prosodic_break == "0" & d_invar3$s_plural == "1", 3, 4)))
d_invar3$pros_pl_interaction <- as.integer(d_invar3$pros_pl_interaction)

# make next sound dependent variable
d_invar3$next_sound_dep <- ifelse(d_invar3$next_sound %in% c("s", "z"), 1, 0)
d_invar3$next_sound2 <- ifelse(d_invar3$next_sound %in% c("s", "z"), 1, 
                               ifelse(d_invar3$next_sound %in% c("@", "a", "A", "e", "E", "i", "I", "K", "L", "M", "o", "O", "y"), 2, 3))

# and interaction with prosody
d_invar3$sound_interaction <- ifelse(d_invar3$prosodic_break == "1" & d_invar3$next_sound2 == "1", 1,
                                     ifelse(d_invar3$prosodic_break == "1" & d_invar3$next_sound2 == "2", 2,
                                            ifelse(d_invar3$prosodic_break == "1" & d_invar3$next_sound2 == "3", 3,
                                                   ifelse(d_invar3$prosodic_break == "0" & d_invar3$next_sound2 == "1", 4, 
                                                          ifelse(d_invar3$prosodic_break == "0" & d_invar3$next_sound2 == "2", 5, 6)))))

d_invar3 <- na.omit(d_invar3)


m1_invar <- ulam(
  alist(
    S ~ dbern(prob),
    logit(prob) <- z[item]*sigma + b[stress] + g[sound],
    b[stress] ~ dnorm(0, 1),
    g[sound] ~ dnorm(0, 1),
    z[item] ~ dnorm(0, 1),
    sigma ~ dexp(1),
    gq> vector[item]:a <<- z*sigma
  ), 
  data = list(S = d_invar3$s_plural, 
              item = d_invar3$item_index, 
              stress = d_invar3$stress_interaction,
              sound = d_invar3$sound_interaction
  ), 
  chains = 4, cores = 4, iter = 4000
)

post_invar <- extract.samples(m1_invar)

# /s/ - C contrasts
# prosodic break
break_sC <- post_invar$g[,1] - post_invar$g[,3]
# no break
nobreak_sC <- post_invar$g[,4] - post_invar$g[,6]

plot(density(break_sC))
lines(density(nobreak_sC))


m0_invar <- ulam(
  alist(
    sound ~ dbern(prob),
    logit(prob) <- z[item]*sigma + b[pros_pl],
    b[pros_pl] ~ dnorm(0, 1),
    z[item] ~ dnorm(0, 1),
    sigma ~ dexp(1),
    gq> vector[item]:a <<- z*sigma
  ), 
  data = list(sound = d_invar3$next_sound_dep,
              item = d_invar3$item_index,
              pros_pl = d_invar3$pros_pl_interaction
  ), 
  chains = 4, cores = 4, iter = 4000, control = list(adapt_delta = 0.98)#, log_lik = T
)

post_invar <- extract.samples(m0_invar)

# /s/ - C contrasts
# prosodic break
break_sn <- post_invar$b[,1] - post_invar$b[,2]
# no break
nobreak_sn <- post_invar$b[,3] - post_invar$b[,4]

plot(density(break_sn))
lines(density(nobreak_sn))

