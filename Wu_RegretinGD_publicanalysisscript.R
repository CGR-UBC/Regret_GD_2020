###R analysis scripts for Yin Wu et al "Should've known better": Counterfactual processing in disordered gambling', 
###under review at Addictive Behaviors
###original scripts prepared by Yin Wu yinwu0407@gmail.com
###final analyses for this manuscript, script prepared and archived by Luke Clark luke.clark@psych.ubc.ca with participant IDs blinded

rm(list=ls())
setwd("c: .../R_analysis")
raw_data <- read.csv("raw_data_new.csv", header = T)

### Install these packages if needed
install.packages("lme4")
install.packages("ggplot2")
install.packages("languageR")
install.packages("lmerTest")
install.packages("MuMIn")
library(lme4)
library(ggplot2)
library(languageR)
library(lmerTest)
library(MuMIn)
library(memisc)

###Prepare some variables from the long sheet: 1) Centre the ratings. 2) compute chance and agent counterfactual (difference) scores
raw_data$rating_obtained <- raw_data$rating_obtained - 5
hist(raw_data$rating_obtained)
raw_data$rating_nonobtained <- raw_data$rating_nonobtained - 5
hist(raw_data$rating_nonobtained)
raw_data$agent_counterfactual <- raw_data$k - raw_data$l
raw_data$chance_counterfactual <- raw_data$k - raw_data$j

###remove trials 1 and 2 as practice
raw_data <- raw_data[raw_data$trial != 1, ]
raw_data <- raw_data[raw_data$trial != 2, ]

###Assign participant IDs to the two groups. For public archiving, these IDs are shielded. You will need to insert the extra instances e.g. controls 2 to 24
raw_data$group[raw_data$subject == 1 |
                 ###...  other subject IDs for control participants in your long file
               | raw_data$subject == 25  ] <- "Con"

raw_data$group  [raw_data$subject == 101 
                 ##...  Let's assume your GD participants have IDs from 100
                 | raw_data$subject ==   146 ] <- "GD"
Con <- raw_data[raw_data$group == "Con",  ]
GD <- raw_data[raw_data$group == "GD",  ]

names(raw_data)
table(raw_data$subject)
table(raw_data$group)
table(raw_data$rating_obtained)

###model obtained outcome on rating 1 ('partial')
model1 <- lmer(rating_obtained ~ k*group + (1|subject), data = raw_data)
summary(model1)
confint(model1)

###For Cohen's f2, calculate the R2 for the full model, and re-calculate after removing the group interaction term
MuMIn::r.squaredGLMM(model1)
model1b <- lmer(rating_obtained ~ k + group + (1|subject), data = raw_data)
summary(model1b)
MuMIn::r.squaredGLMM(model1b)
###f2 = R2(full)-R2(with term removed)/(1-R2full), see Selya et al (2012)

####generate figure 2A in paper
scatter <- ggplot(raw_data, aes(k, rating_obtained, colour = group ) )
scatter  + geom_smooth(method = "lm", aes(fill = group), alpha = 0.5) + labs(x = "obtained outcome", y = "affect ratings" ) + scale_x_continuous(breaks=c(-210,0,210)) + ylim(-4,4) + 
      theme_bw() +      
      theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
      axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22),
      legend.position = "none")
ggsave("partial_obtained.tiff", width = 7, height = 5, dpi = 150)

####This code runs the model separately in each group, to generate the subset model in GD only, 2) the subject coefficients for use in the individual differences analyses
model1con <- lmer(rating_obtained ~ k + (1|subject), data = Con)
summary(model1con)
model1gd <- lmer(rating_obtained ~ k + (1|subject), data = GD)
summary(model1gd)
confint(model1gd)

####This model generates the subject coefficients for use in the individual differences analyses
model1concoeff <- lmer(rating_obtained ~ k + (1 + k|subject), data = Con)
summary(model1concoeff)
coef(model1concoeff)
write.csv(coef(model1concoeff)$subject, "Rating1Obtained_Con.csv")
model1gdcoeff <- lmer(rating_obtained ~ k + (1 + k|subject), data = GD)
coef(model1gdcoeff)
write.csv(coef(model1gdcoeff)$subject, "Rating1obtained_PG.csv")

###This is the first rating ('partial'), the model of the 'chance counterfactual' 
model2 <- lmer(rating_obtained ~ chance_counterfactual*group + (1|subject), data = raw_data)
summary(model2)
confint(model2)

###Cohen's f2
MuMIn::r.squaredGLMM(model2)
model2b <- lmer(rating_obtained ~ chance_counterfactual + group + (1|subject), data = raw_data)
summary(model2b)
MuMIn::r.squaredGLMM(model2b)

###Draw Figure 2b
scatter <- ggplot(raw_data, aes(chance_counterfactual, rating_obtained, colour = group ) )
scatter  + geom_smooth(method = "lm", aes(fill = group), alpha = 0.5) + labs(x = "chance counterfactual", y = "affect ratings" ) + scale_x_continuous(breaks=c(-420,0,420)) + ylim(-4,4) +
      theme_bw() +     
      theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
      axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22),
      legend.position = "none")
ggsave("partial_chance.tiff", width = 7, height = 5, dpi = 150)

####subset models
model2con <- lmer(rating_obtained ~ chance_counterfactual + (1|subject), data = Con)
summary(model2con)

model2gd <- lmer(rating_obtained ~ chance_counterfactual + (1|subject), data = GD)
summary(model2gd)
confint(model2gd)

####generate coefficients for slope.csv for rating 1 nonobtained
model2concoeff <- lmer(rating_obtained ~ chance_counterfactual + (1 + chance_counterfactual|subject), data = Con)
coef(model2concoeff)
write.csv(coef(model2concoeff)$subject, "Rating1chanceCF_Con.csv")

model2gdcoeff <- lmer(rating_obtained ~ chance_counterfactual + (1 + chance_counterfactual|subject), data = GD)
coef(model2gdcoeff)
write.csv(coef(model2gdcoeff)$subject, "Rating1chanceCF_PG.csv")

##Rating 2, the model of the obtained (stats should be very similar to model 1)
model3 <- lmer(rating_nonobtained ~ k*group + (1|subject), data = raw_data)
summary(model3)
confint(model3)

###Cohen's f2
MuMIn::r.squaredGLMM(model3)
model3b <- lmer(rating_nonobtained ~ k + group + (1|subject), data = raw_data)
summary(model3b)
MuMIn::r.squaredGLMM(model3b)

###Draw Figure 2c
scatter <- ggplot(raw_data, aes(k, rating_nonobtained, colour = group ) )
scatter  + geom_smooth(method = "lm", aes(fill = group), alpha = 0.5) + labs(x = "obtained outcome", y = "affect ratings" ) + scale_x_continuous(breaks=c(-210,0,210)) + ylim(-4,4)+
        theme_bw() +     
        theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
        axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22),
        legend.position = "none")
ggsave("Rating2_obtained.tiff", width = 7, height = 5, dpi = 150)

###subset models
model3con <- lmer(rating_nonobtained ~ k + (1|subject), data = Con)
summary(model3con)
model3gd <- lmer(rating_nonobtained ~ k + (1|subject), data = GD)
summary(model3gd)

####derive subject coefficients for individual diffs analysis
model3concoeff <- lmer(rating_nonobtained ~ k + (1 + k|subject), data = Con)
coef(model3concoeff)
write.csv(coef(model3concoeff)$subject, "Rating2Obtained_Con.csv")
model3gdcoeff <- lmer(rating_nonobtained ~ k + (1 + k|subject), data = GD)
coef(model3gdcoeff)
write.csv(coef(model3gdcoeff)$subject, "Rating2Obtained_PG.csv")

####Rating 2 Affect ratings, agent counterfactual on non-obtained outcome (centred)
model4 <- lmer(rating_nonobtained ~ agent_counterfactual*group + (1|subject), data = raw_data)
summary(model4)
confint(model4)

###Cohen's f2
MuMIn::r.squaredGLMM(model4)
model4b <- lmer(rating_nonobtained ~ agent_counterfactual + group + (1|subject), data = raw_data)
summary(model4b)
MuMIn::r.squaredGLMM(model4b)

###generate figure 2d
scatter <- ggplot(raw_data, aes(agent_counterfactual, rating_nonobtained, colour = group ) )
scatter  + geom_smooth(method = "lm", aes(fill = group), alpha = 0.5) + labs(x = "agent counterfactual", y = "affect ratings" ) + scale_x_continuous(breaks=c(-420,0,420)) + ylim(-4,4)+
        theme_bw() +     
        theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
        axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22),
        legend.position = "none")
ggsave("Rating2_counterfactual.tiff", width = 7, height = 5, dpi = 150)

####Subset model for agent CF in each group
model4con <- lmer(rating_nonobtained ~ agent_counterfactual + (1|subject), data = Con)
summary(model4con)

model4gd <- lmer(rating_nonobtained ~ agent_counterfactual + (1|subject), data = GD)
summary(model4gd)
confint(model4gd)

####derive subject coefficients for individual diffs analysis
model4concoeff <- lmer(rating_nonobtained ~ agent_counterfactual + (1 + agent_counterfactual|subject), data = Con)
coef(model4concoeff)
write.csv(coef(model4concoeff)$subject, "Rating2agentCF_Con.csv")
model4gdcoeff <- lmer(rating_nonobtained ~ agent_counterfactual + (1 + agent_counterfactual|subject), data = GD)
coef(model4gdcoeff)
write.csv(coef(model4gdcoeff)$subject, "Rating2agentCF_PG.csv")

####CHOICE MODEL STARTS HERE 
##replace key 2 into 0
table(raw_data$key)

raw_data$choice[raw_data$key == 1] <- 1
raw_data$choice[raw_data$key == 2] <- 0

####E / V / R untransformed model by Gillan (run the pairs of line together!)
model5 <- glmer(choice ~ e + v + r + group:e + group:v + group:r +  (1|subject), family = binomial(), data = raw_data)
summary(model5)
confint(model5)

### model 52: z transform the choice parameters to address scaling
raw_data$e_z <- (raw_data$e-mean(raw_data$e))/sd(raw_data$e)
raw_data$r_z <- (raw_data$r-mean(raw_data$r))/sd(raw_data$r)
raw_data$v_z <- (raw_data$v-mean(raw_data$v))/sd(raw_data$v)

model6 <- glmer(choice ~ e_z + v_z + r_z + group:e_z + group:v_z + group:r_z +  (1|subject), family = binomial(), data = raw_data)
summary(model6)
confint(model6)
logLik(model6)
oddsratios6 <- exp(model6@beta)
View(oddsratios6)

###Cohen's f2: these models generate the r2 values with each of the 3 interaction terms removed.
MuMIn::r.squaredGLMM(model6)
model6b <- glmer(choice ~ e_z + v_z + r_z + group:v_z + group:r_z +  (1|subject), family = binomial(), data = raw_data)
summary(model6b)
MuMIn::r.squaredGLMM(model6b)

model6c<- glmer(choice ~ e_z + v_z + r_z + group:e_z + group:r_z +  (1|subject), family = binomial(), data = raw_data)
summary(model6c)
MuMIn::r.squaredGLMM(model6c)

model6d <- glmer(choice ~ e_z + v_z + r_z + group:v_z + group:e_z +  (1|subject), family = binomial(), data = raw_data)
summary(model6d)
MuMIn::r.squaredGLMM(model6d)



### Code for Figure 3 subpanels
scatter <- ggplot(raw_data, aes(r, choice, colour = group ) )
scatter  + geom_smooth(method = "glm", family="binomial", aes(fill = group), alpha = 0.3) + labs(x = "r", y = "P (wheel 1)" ) +  scale_x_continuous(breaks=c(-280,0,280)) + scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        theme_bw() +     
        theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
        axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22),
        legend.position = "none")
#        plot.title = "Regret anticipation", plot.title = element_text(size = 25, face = "bold")
ggsave("r.tiff",  width = 7, height = 5, dpi = 150)

scatter <- ggplot(raw_data, aes(e, choice, colour = group ) )
scatter  + geom_smooth(method = "glm", method.args=list(family="binomial"), aes(fill = group), alpha = 0.3) + labs(x = "e", y = "P (wheel 1)" )  +  scale_x_continuous(breaks=c(-140,0,245)) + scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        theme_bw() +      
        theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
        axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22),
#        plot.title = "Expected value, e", plot.title = element_text(size = 25, face = "bold",
        legend.position = "none")
ggsave("e.tiff",  width = 7, height = 5, dpi = 150)

scatter <- ggplot(raw_data, aes(v, choice, colour = group ) )
scatter  + geom_smooth(method = "glm", family="binomial", aes(fill = group), alpha = 0.3) + labs(x = "v", y = "P (wheel 1)" )  +  scale_x_continuous(breaks=c(-29400,0,40425)) + scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        theme_bw() +      
        theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
        axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22),
        #        plot.title = "Risk variance, v", plot.title = element_text(size = 25, face = "bold",
        legend.position = "none")
ggsave("v.tiff", width = 7, height = 5, dpi = 150)

####subset model for the E / V / R in the 2 groups separately, z transformed choice parameters
Con <- raw_data[raw_data$group == "Con",  ]
GD <- raw_data[raw_data$group == "GD",  ]

model6gd <- glmer(choice ~ e_z + v_z + r_z + (1|subject), family = binomial(), data = GD)
summary(model6gd)
confint(model6gd)
logLik(model6gd)
oddsratios52gd <- exp(model6gd@beta)
View(oddsratios6gd)

###extract coefficients, for the individual differences analysis
model6conR <- glmer(choice ~ r_z + (1 + r_z|subject), family = binomial(), data = Con)
coef(model6conR)
write.csv(coef(model6conR)$subject, "r_Con.csv")

model6gdR <- glmer(choice ~ r_z + (1 + r_z|subject), family = binomial(), data = GD)
coef(model6gdR)
write.csv(coef(model6gdR)$subject, "r_PG.csv")

model6conE <- glmer(choice ~ e_z + (1 + e_z|subject), family = binomial(), data = Con)
coef(model6conE)
write.csv(coef(model6conE)$subject, "e_Con.csv")

model6gdE <- glmer(choice ~ e_z + (1 + e_z|subject), family = binomial(), data = GD)
coef(model6gdE)
write.csv(coef(model6gdE)$subject, "e_PG.csv")

model6conV <- glmer(choice ~ v_z + (1 + v_z|subject), family = binomial(), data = Con)
coef(model6conV)
write.csv(coef(model6conV)$subject, "v_Con.csv")

model6gdV <- glmer(choice ~ v_z + (1 + v_z|subject), family = binomial(), data = GD)
coef(model6gdV)
write.csv(coef(model6gdV)$subject, "v_PG.csv")

###analysis of decision latencies, Supp Table 2. Start by calculating absolute values of 3 parameters
raw_data$e_abs <- abs(raw_data$e)
raw_data$r_abs <- abs(raw_data$r)
raw_data$v_abs <- abs(raw_data$v)

model7 <- lmer(decisionRT ~  e_abs + v_abs + r_abs + group:e_abs + group:v_abs + group:r_abs + (1|subject), data = raw_data)
summary(model7)
confint(model7)
logLik(model7)

### Latency plots
scatter <- ggplot(raw_data, aes(r_abs, decisionRT, colour = group ) )
scatter  + geom_smooth(method = "glm", aes(fill = group), alpha = 0.5) + labs(x = "r_abs", y = "latency" ) +  scale_x_continuous(breaks=c(0,280)) + scale_y_continuous(breaks=c(0, 5000, 1000, 15000, 20000)) +
  theme_bw() +     
  theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
        axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22))
#        legend.position = "none")
ggsave("r_lat.tiff",  width = 7, height = 5, dpi = 150)

scatter <- ggplot(raw_data, aes(e_abs, decisionRT, colour = group ) )
scatter  + geom_smooth(method = "glm", aes(fill = group), alpha = 0.5) + labs(x = "e_abs", y = "latency" )  +  scale_x_continuous(breaks=c(0,245)) + scale_y_continuous(breaks=c(0, 5000, 1000, 15000, 20000)) +
  theme_bw() +      
  theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
        axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22))
        #legend.position = "none")
ggsave("e_lat.tiff",  width = 7, height = 5, dpi = 150)

scatter <- ggplot(raw_data, aes(v_abs, decisionRT, colour = group ) )
scatter  + geom_smooth(method = "glm", aes(fill = group), alpha = 0.5) + labs(x = "v_abs", y = "latency" )  +  scale_x_continuous(breaks=c(0,40425)) + scale_y_continuous(breaks=c(0, 5000, 1000, 15000, 20000)) +
  theme_bw() +      
  theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
        axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22))
        #legend.position = "none")
ggsave("v_lat.tiff", width = 7, height = 5, dpi = 150)