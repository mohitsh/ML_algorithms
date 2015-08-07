######################################################
# Splitting data set into multiple parts for training, testing and calibaration
d <- read.table('orange_small_test.data', header = T, sep = '\t', na.strings = c("NA",""))

# churn is account cancellation
churn <- read.table('orange_small_train_churn.labels', header = F,sep = '\t')

d$churn <- churn$V1

# the innate tendency to use new products and services
appetency <- read.table('orange_small_train_appetency.labels', header = F, sep = '\t')

d$appetency <- appetency$V1

# willingness to respond favourably to marketing pitches
upselling <- read.table('orange_small_train_upselling.labels', header = F, sep = '\t')

d$upselling <- upselling$V1

set.seed(729375)
# add rgroup colomn; will be used to split dataset into training and test data
d$rgroup <- runif(dim(d)[1]) # no of rows in d

# splitting data into train and test datasets 
dTrainAll <- subset(d,rgroup <= 0.9)
dTest <- subset(d, rgroup > 0.9)

outcomes <- c('churn','appetency', 'upselling')
# taking all the columns except outcomes and rgoup i.e. 'churn','appetency', 'upselling' and rgoup
vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))

catVars <- vars[sapply(dTrainAll[,vars], class) %in% c('factor','character')]

numericVars <- vars[sapply(dTrainAll[,vars],class) %in% c('numeric','integer')]

outcome <- 'churn'
pos <- '1'

# divinding train data for training and calibratin
useForCal <- rbinom(n=dim(dTrainAll[1]),size=1,prob = .4) > 0

dCal <- subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll, !useForCal)

# data splitting ends here
#########################################################























