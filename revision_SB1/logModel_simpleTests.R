# basic tests of manipulations


# import lme4 module for mixed models
library(lme4)
library(emmeans)
library(MuMIn)
library(optimx)

# read data - auditory
data_aud <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//Data//Exp1_auditory//exp1_model1_auditory.csv")

# read data - visual
data_vis <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//Data//Exp2_visual//exp2_model1_visual.csv")

# TEST 1: CUE

patsy_cue_aud <- 'response ~ (1|sbj_id) + (1|block:sbj_id) + target_z  + cue_z'

cue_aud <- glmer(patsy_cue_aud, data=data_aud, 
                    family=binomial('logit'), 
                    control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))



# save model in summary text file for later manip. in python
sink('cue_aud.txt')
print(summary(cue_aud))
sink()


# TEST 2: BLOCK

patsy_block_aud <- 'response ~ (1|sbj_id) + (1|block:sbj_id) + target_z + stim_1_z + block_type_z * resp_1_z'

block_aud <- glmer(patsy_block_aud, data=data_aud, 
                   family=binomial('logit'), 
                   control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))

sink('block_aud.txt')
print(summary(block_aud))
sink()



# TESTS ON VISUAL DATA
patsy_cue_vis <- 'response ~ (1|sbj_id) + target_z  + cue_z'


cue_vis <- glmer(patsy_cue_vis, data=data_vis,
                    family=binomial('logit'),
                    control=glmerControl(optimizer='optimx', optCtrl = list(method='nlminb')))


# save model summary in text file
sink('cue_vis.txt')
print(summary(cue_vis))
sink()

# TEST block type on visual data

patsy_block_vis <- 'response ~ (1|sbj_id) + target_z + stim_1_z + block_type_z * resp_1_z'

block_vis <- glmer(patsy_block_vis, data=data_vis, 
                   family=binomial('logit'), 
                   control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))

sink('block_vis.txt')
print(summary(block_vis))
sink()


# TEST 3: Interaction between block type and stimulus history.
# Experiment 1
patsy_stim_aud <- 'response ~ (1|sbj_id) + target_z  + block_type_z * stim_1_z'

stim_aud <- glmer(patsy_stim_aud, data=data_aud, 
                  family=binomial('logit'), 
                  control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))

sink('stim_aud.txt')
print(summary(stim_aud))
sink()

# Stimulus History
patsy_stim_vis <- 'response ~ (1|sbj_id) + target_z  + block_type_z * stim_1_z'

stim_vis <- glmer(patsy_stim_vis, data=data_vis, 
                   family=binomial('logit'), 
                   control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))

sink('stim_vis.txt')
print(summary(stim_vis))
sink()

# TEST 4: Prev Choice * prev Diff

# read data - auditory
data_aud_prevDiff <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//Data//Exp1_auditory//exp1_prevMotor_prevEvidence.csv")

# read data - visual
data_vis_prevDiff <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//Data//Exp2_visual//exp2_prevMotor_prevDiff.csv")

# auditory data
patsy_prevDiff_aud <- 'response ~ (1|sbj_id) + target_z  + stim_1_z + resp_1_z * prev_diff_z'

prevDiff_aud <- glmer(patsy_prevDiff_aud, data=data_aud_prevDiff, 
                  family=binomial('logit'), 
                  control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))

sink('prevDiff_aud.txt')
print(summary(prevDiff_aud))
sink()

# visual data
patsy_prevDiff_vis <- 'response ~ (1|sbj_id) + target_z + stim_1_z + resp_1_z * prev_coh_z'

prevDiff_vis <- glmer(patsy_prevDiff_vis, data=data_vis_prevDiff, 
                      family=binomial('logit'), 
                      control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))

sink('prevDiff_vis.txt')
print(summary(prevDiff_vis))
sink()

