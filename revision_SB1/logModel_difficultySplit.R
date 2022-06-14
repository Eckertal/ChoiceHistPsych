# import lme4 module for mixed models
library(lme4)
library(emmeans)
library(MuMIn)
library(optimx)

################################################################################
### AUDITORY

# read data - auditory
data_aud_easy <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//Data//Exp1_auditory//exp1_easyOnly.csv")

# read data - visual
data_vis_easy <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//Data//Exp2_visual//exp2_easyOnly.csv")

# set up model on auditory data...
# patsy auditory
patsy_aud <- 'response ~ (1|sbj_id) + target_z + diff_z + stim_1_z + resp_1_z*PPS_z + prev_diff_z*resp_1_z + cue_z*PPS_z '

model1_aud_easy <- glmer(patsy_aud, data=data_aud_easy, 
                    family=binomial('logit'), 
                    control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))

# save model in summary text file for later manip. in python
sink('exp1_fits_onlyEasy.txt')
print(summary(model1_aud_easy))
sink()

###############################################################################
# VISUAL EXPERIMENT
# patsy visual
patsy_vis <- 'response ~ (1|sbj_id) + (1|Block:sbj_id) + target_z + coherence_z + stim_1_z + resp_1_z + prev_coh_z*resp_1_z + cue_z*PPS_z*block_type + resp_1_z*PPS_z*block_type_z'


model1_vis_easy <- glmer(patsy_vis, data=data_vis_easy,
                    family=binomial('logit'),
                    control=glmerControl(optimizer='optimx', optCtrl = list(method='nlminb')))


# save model summary in text file
sink('exp2_fits_onlyEasy.txt')
print(summary(model1_vis_easy))
sink()

################################################################################
### HARD 

# read data - auditory
data_aud_hard <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//revision_SB1//supplementary_data//exp1_precedingTrialHard.csv")

# read data - visual
data_vis_hard <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//revision_SB1//supplementary_data//exp2_precedingTrialHard.csv")

# patsy for auditory neutral blocks only
#patsy_aud <- 'response_z ~ (1|sbj_id) + target_z + stimulus_z + cue_z * PPS_z + stim_1_z + resp_1_z * PPS_z'

model1_aud_hard <- glmer(patsy_aud, data=data_aud_hard, 
                    family=binomial('logit'), 
                    control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))

# save model in summary text file for later manip. in python
sink('exp1_fit_hardOnly.txt')
print(summary(model1_aud_hard))
sink()

# patsy visual, neutral blocks only
#patsy_vis <- 'response ~ (1|sbj_id) + target + coherence + cue * PPS_z + stim_1 + resp_1 * PPS_z'

model1_vis_hard <- glmer(patsy_vis, data=data_vis_hard,
                    family=binomial('logit'),
                    control=glmerControl(optimizer='optimx', optCtrl = list(method='nlminb')))


# save model summary in text file
sink('exp2_fit_hardOnly.txt')
print(summary(model1_vis_hard))
sink()