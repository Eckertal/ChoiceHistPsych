################################################################################
# Random slope models vs. no random slope models
# VISUAL

# set up model on visual data... 
# patsy visual

# read data - visual
data_vis <- read.csv("C://Users//annae//Desktop//ChoiceHistory_Psych//Data//Exp2_visual//exp2_model1_visual.csv")

# 1. MODEL WITHOUT RANDOM SLOPES
patsy_vis_noRS <- 'response ~ (1|sbj_id)  + (1|Block:sbj_id) + target_z + diff_z + cue_z  + stim_1_z + resp_1_z * PPS_z + PPS_z * stim_1_z '

model1_vis_noRS <- glmer(patsy_vis_noRS, data=data_vis,
                    family=binomial('logit'),
                    control=glmerControl(optimizer='optimx', optCtrl = list(method='nlminb')))


# 2. MODEL WITH ALL RANDOM SLOPES (CUE, STIMULUS AND CHOICE HISTORY)
patsy_vis_RS <- 'response ~ (1|sbj_id) + (1|Block:sbj_id) + (1+cue_z|sbj_id) + (1+resp_1_z|sbj_id) + (1+stim_1_z|sbj_id) + target_z + diff_z + stim_1_z + cue_z * PPS_z + resp_1_z * PPS_z '

model_vis_RS <- glmer(patsy_vis_RS, data=data_vis,
                  family=binomial('logit'),
                  control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))


# 3. MODEL WITH RANDOM SLOPE FOR CUE
patsy_RScue <- 'response ~ (1|sbj_id) + (1|Block:sbj_id) + (1+cue_z|sbj_id) + target_z + diff_z + stim_1_z + cue_z * PPS_z + resp_1_z * PPS_z '

model_vis_RScue <- glmer(patsy_RScue, data=data_vis,
                         family=binomial('logit'),
                         control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))


# 4. MODEL WITH RANDOM SLOPE FOR CHOICE HISTORY
patsy_RS_rt1 <- 'response ~ (1|sbj_id) + (1|Block:sbj_id) + (1+resp_1_z|sbj_id) + target_z + diff_z + stim_1_z + cue_z * PPS_z + resp_1_z * PPS_z '

model_aud_RS_rt1 <- glmer(patsy_RS_rt1, data=data_vis,
                          family=binomial('logit'),
                          control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))

# 5. MODEL WITH RANDOM SLOPE FOR STIMULUS HISTORY
patsy_RS_st1 <- 'response ~ (1|sbj_id) + (1|Block:sbj_id) + (1+stim_1_z|sbj_id) + target_z + diff_z + stim_1_z + cue_z * PPS_z + resp_1_z * PPS_z '

model_vis_RS_st1 <- glmer(patsy_RS_st1, data=data_vis,
                          family=binomial('logit'),
                          control=glmerControl(optimizer='optimx', optCtrl=list(method='nlminb')))
