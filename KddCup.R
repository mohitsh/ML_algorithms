# main task of the project is to estimate the churn, appetency and up-selling probability

# Splitting data set into multiple parts for training, testing and calibaration
d <- read.table('orange_small_test.data', header = T, sep = '\t', na.strings = c("NA",""))

# churn is tendency of users to switch provider
churn <- read.table('orange_small_train_churn.labels', header = F,sep = '\t')

d$churn <- churn$V1

# appetency: propensity to buy new products and services
appetency <- read.table('orange_small_train_appetency.labels', header = F, sep = '\t')

d$appetency <- appetency$V1

# upselling: buy upgrades or add ons proposed to them to make the sale profitable 
upselling <- read.table('orange_small_train_upselling.labels', header = F, sep = '\t')

d$upselling <- upselling$V1

set.seed(729375)
# add rgroup colomn; will be used to split dataset into training and test data
d$rgroup <- runif(dim(d)[1]) # no of rows in d

# splitting data into train and test datasets 
dTrainAll <- subset(d,rgroup <= 0.9)
dTest <- subset(d, rgroup > 0.9)

outcomes <- c('churn','appetency', 'upselling')

# taking all the columns except outcomes and rgoup i.e. 'churn','appetency', 'upselling' and rgroup
vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))

# get the category variables i.e. factors and characters
catVars <- vars[sapply(dTrainAll[,vars], class) %in% c('factor','character')]

# get the nueric variables
numericVars <- vars[sapply(dTrainAll[,vars],class) %in% c('numeric','integer')]

outcome <- 'churn'
pos <- '1'

# divinding train data into training and calibratin

useForCal <- rbinom(n=dim(dTrainAll[1]),size=1,prob = .5) > 0

dCal <- subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll, !useForCal)

# data splitting ends here

# using categorical features for building single variable models

table218 <- table(
                var218=dTrain[,"Var218"],
                churn = dTrain[,outcome],  # remember outcome here is "churn"
                useNA = 'ifany'
)

#churn grouped by table218
print(table218)

#churn rates grouped by table218. churn happens when churn == 1 hence table[,2] is selected
print(table218[,2]/(table218[,1] + table218[,2]))

# single variable model for categorical variables

mkPredC <- function(outCol, varCol, appCol){
        
        # outCol holds output for churn so take those churn values that are positive i.e. 1
        pPos <- sum(outCol == pos)/length(outCol)  
        
        # select all those entries from outCol i.e. outcome when input variable is NA and change them to factor
        naTab <- table(as.factor(outCol[is.na(varCol)]))
        
        
        pPosWna <- (naTab/sum(naTab))[pos]
        
        vTab <- table(as.factor(outCol), varCol)
        
        pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
                
        pred <- pPosWv[appCol]
        
        pred[is.na(appCol)] <- pPosWna
        
        pred[is.na(pred)] <- pPosWv
        
        pred
        
}

# making prediction based on categorical variables


for (v in catVars){
        
        pi <- paste('pred',v,sep = '')
        dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
        dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
        dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}

# Scoring categorical variable by AUC

library('ROCR')

calcAUC <- function(predcol, outcol){
        perf <- performance(prediction(predcol, outcol==pos), 'auc')
        as.numeric(perf@y.values)
}

for ( v in catVars){
        pi <- paste('pred',v,sep = '')
        aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
        if(aucTrain >= 0.8){
                aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
                print (sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f", pi,aucTrain, aucCal))
        }
}


























