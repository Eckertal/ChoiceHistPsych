# only neutral blocks

# import lme4 module for mixed models
library(lme4)
library(emmeans)
library(MuMIn)
library(optimx)

# read data - auditory
data_aud <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//revision_SB2//exp1_repOnly.csv")

# read data - visual
data_vis <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//revision_SB2//exp2_repOnly.csv")

# set up model on auditory data...
# patsy auditory
patsy_aud <- 'response ~ (1|sbj_id) + (1|block:sbj_id) + target_z*evidence_z + stim_1_z*evidence_1_z + resp_1_z*PPS_z + cue_z*PPS_z'


model1_aud <- glmer(patsy_aud, data=data_aud, 
                    family=binomial('logit'), 
                    control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))

# save model in summary text file for later manip. in python
sink('new_exp1_repOnly.csv')
print(summary(model1_aud))
sink()

# set up model on visual data... 
# patsy visual
patsy_vis <- 'response ~ (1|sbj_id)  + (1|Block:sbj_id) + target_z*coherence_z + stim_1_z*prev_coh_z + resp_1_z*PPS_z + cue_z*PPS_z'


model1_vis <- glmer(patsy_vis, data=data_vis,
                    family=binomial('logit'),
                    control=glmerControl(optimizer='optimx', optCtrl = list(method='nlminb')))


# save model summary in text file
sink('new_exp2_repOnly.csv')
print(summary(model1_vis))
sink()

r.squaredGLMM(model1_aud)
r.squaredGLMM(model1_vis)


