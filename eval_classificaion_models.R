data("spambase")
spamTrain <- subset(spambase, spambase$capital_run_length_average >= 10)
spamTest <-  subset(spambase, spambase$capital_run_length_average < 10)

# taking all the variables i.e. column names except those; that are ouput or training data
# in this case"capital_run_length_average"  is the training criteria and "is_spam" is the output
# exclude these two variables and spamVars is the 
spamVars <- setdiff(colnames(spambase), list('is_spam'))



spamModel <-glm(spamFormula,family = binomial(link = 'logit'), data = spamTrain)
spamTrain$pred <- predict(spamModel,newdata = spamTrain, type='response')

spamTest$pred <- predict(spamModel, newdata=spamTest, type='response')

print(with(spamTest, table(y=is_spam, glmPred = pred>0.5)))

names  <- colnames(spamTrain)
a = ''
for ( i in 1:57){
       
        a = paste(a,names[i],sep = " + ")                                
}

mod <- glm(is_spam ~ word_freq_make + word_freq_address + word_freq_all + word_freq_3d + word_freq_our 
+ word_freq_over + word_freq_remove + word_freq_internet + word_freq_order + word_freq_mail + word_freq_receive 
+ word_freq_will + word_freq_people + word_freq_report + word_freq_addresses + word_freq_free 
+ word_freq_business + word_freq_email + word_freq_you + word_freq_credit + word_freq_your 
+ word_freq_font + word_freq_000 + word_freq_money + word_freq_hp + word_freq_hpl 
+ word_freq_george + word_freq_650 + word_freq_lab + word_freq_labs + word_freq_telnet
+ word_freq_857 + word_freq_data + word_freq_415 + word_freq_85 + word_freq_technology 
+ word_freq_1999 + word_freq_parts + word_freq_pm + word_freq_direct + word_freq_cs 
+ word_freq_meeting + word_freq_original + word_freq_project + word_freq_re + word_freq_edu
+ word_freq_table + word_freq_conference + char_freq_semicolon + char_freq_left_paren 
+ char_freq_left_bracket + char_freq_exclamation + char_freq_dollar + char_freq_pound 
+ capital_run_length_average + capital_run_length_longest + capital_run_length_total,
family = binomial(link = 'logit'), data = spamTrain)

spamTest$pred <- predict(mod,newdata = spamTest)
print(with(spamTest,table(y=is_spam, glmPred = pred > 0.5)))
