rm(list=ls())

######Workings of Loan Tranche (dataset 1 and 2)
##### Dataset 1 has all tranches and dataset 2  has tranches with blank Logarithm of spreads is replaced with mean of logarithm of spread from tranches of the same deal

library(readr)
library(dplyr)

dsc_tranche_finv3 <- read_csv("dsc_tranche_finv3.csv")

#Variable adjustments
dsc_tranche_finv3$Violator_Remarks <- as.factor(dsc_tranche_finv3$Violator_Remarks)
dsc_tranche_finv3Tranche_type_coded <- as.factor(dsc_tranche_finv3$Tranche_type_coded)
dsc_tranche_finv3Collateral_coded <- as.factor(dsc_tranche_finv3$Collateral_coded)
dsc_tranche_finv3$Primary_Purpose_Coded <- as.factor(dsc_tranche_finv3$Primary_Purpose_Coded)
dsc_tranche_finv3$Covenant_coded <- as.factor(dsc_tranche_finv3$Covenant_coded)
dsc_tranche_finv3$Syndicated_loan_coded <- as.factor(dsc_tranche_finv3$Syndicated_loan_coded)
dsc_tranche_finv3$Borrower_Id <- as.numeric(dsc_tranche_finv3$Borrower_Id)
dsc_tranche_finv3$LPC_Tranche_ID <- as.character(dsc_tranche_finv3$LPC_Tranche_ID)
dsc_tranche_finv3$Ln_Spread <- as.numeric (dsc_tranche_finv3$Ln_Spread)
dsc_tranche_finv3$Ln_Tranche_Amount_Converted <- as.numeric(dsc_tranche_finv3$Ln_Tranche_Amount_Converted)
dsc_tranche_finv3$Bank_Fixed_Effects <- as.factor(dsc_tranche_finv3$Bank_Fixed_Effects)

dsc_tranche_fin <- dsc_tranche_finv3
dsc_tranche_fin$Penalty_Amount <- as.numeric(dsc_tranche_fin$Penalty_Amount)
dsc_tranche_fin$Penalty_Cat <- ifelse(dsc_tranche_fin$Penalty_Amount >= 50000, 1, 0)

#dsc_tranche_fin$id <- paste(dsc_tranche_fin$Borrower_Id, dsc_tranche_fin$Tranche_Year)
### Removing unnecessary columns
dsc_tranche_fin = select(dsc_tranche_fin, -c(FirmNameCompustatYear2.x,FirmNameCompustatYear1.x,coname_h.x...6,Lead_Arranger,Deal_Purpose,Tenor_Maturity,Ln_Spread_bps_calc,FirmNameYear,Violator_Database_Remark,Compustat_year1,Compustat_year2,Compustat_year3,fyear.x,coname_h.y...33,curncd.x,fyear.y,coname_h.x...43,curncd.y,Current_Asset_ratio.y,Log_assets.y,Leverage_ratio.y,Profitability_ratio.y , Fixed_Asset_ratio.y,Tobins_q.y,Z_score.y,FirmNameCompustatYear1.y,fyear,coname_h.y...54,curncd,Current_Asset_ratio,Log_assets,Leverage_ratio,Profitability_ratio,Fixed_Asset_ratio,Tobins_q,Z_score,FirmNameCompustatYear1,FirmNameCompustatYear2.y,FirmNameCompustatYear3))

### Counting number of years of borrower data
no_of_years <- dsc_tranche_fin %>% group_by(Borrower_Id, Tranche_Year) %>% summarize(number=n()) %>% 
  group_by(Borrower_Id) %>% mutate(number2=n())
no_of_years <- subset(no_of_years, number2>8)

#subset only borrowers that have data over 6 years since we want 3 years pre treatment and 2 years post treatment 
dsc_tranche_fin2 <- dsc_tranche_fin[ dsc_tranche_fin$Borrower_Id %in% unique(no_of_years$Borrower_Id), ]
### This line is causing problems
#dsc_tranche_fin2$id <- paste(dsc_tranche_fin2$Borrower_Id, dsc_tranche_fin2$Compustat_year1) #Tranche_year


### Working on only borrowers which have 6 years of data 
data <- dsc_tranche_fin2
#### This function has confusion
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
data[is.nan(data)] <- NA
#### Removing NA from borrower data with 6 years
library(tidyr)
data %>% drop_na(Current_Asset_ratio_Avg, Log_assets_Avg, Leverage_ratio_Avg, Profitability_ratio_Avg, Fixed_Asset_ratio_Avg, Tobins_q_Avg, Z_score_Avg) -> data
### Identifying years where there is no violation through mean and removing them
data2 <- data %>% group_by(as.factor(Tranche_Year)) %>% 
  mutate(violate = mean(as.numeric(as.character(Violator_Remarks, na.rm=TRUE)))) %>% ungroup() 

data2 <- subset(data2, violate>0) #i need to remove years where there is no treated unit

#### Working on PSM only on violation years and with borrowers of 6 years of data
#### Creating a list where each dataframe is for a violation year
### PSM calculates treatment probability based on borrower characteristics for each treated firm in a treatment year.

data2 %>%
  group_split(Tranche_Year) -> listtranche1

listtranche1 = listtranche1[-18] #2013
listtranche1 = listtranche1[-21] #2021

##### Running PSM only on treatment years.To run code, run codes before the loop, run the equation only in  the loop and fix variables.
library(MatchIt)
### Creating an empty list and populating treatment firm, control firm and treatment year in the df list
df <- list()

nn=2
### nn is number of controls
### i indicates the nth treatment year
for(i in 1:length(listtranche1)){
  
  PSMModelTranche1 <- matchit(Violator_Remarks ~ Current_Asset_ratio_Avg + Log_assets_Avg + Leverage_ratio_Avg + Profitability_ratio_Avg + 
                                Fixed_Asset_ratio_Avg, data=listtranche1[[i]], ratio=nn,
                              method = 'genetic', distance = 'Mahalanobis', replace=FALSE) #CHANGED
  
  #summary(PSMModelTranche1)
  #plot(PSMModelTranche1, type = "jitter", interactive = FALSE)
  ### Identifying borrower id of violators from treatment year
  treat <- subset(listtranche1[[i]], Violator_Remarks==1)$Borrower_Id
  ### Identifying the borrower id of  nth control of PSM match matrix from the treatment year
  ### PSMModel match matrix gives the nth control for each treated firm in the treatement year
  ### It shows the row number of control firms for each treatment firm's row number in the treatment year
  control1 <- listtranche1[[i]][as.numeric(PSMModelTranche1$match.matrix[,1]),]$Borrower_Id
  control2 <- listtranche1[[i]][as.numeric(PSMModelTranche1$match.matrix[,2]),]$Borrower_Id
  #control3 <- listtranche1[[i]][as.numeric(PSMModelTranche1$match.matrix[,3]),]$Borrower_Id
  #control4 <- listtranche1[[i]][as.numeric(PSMModelTranche1$match.matrix[,4]),]$Borrower_Id.x
  #control5 <- listtranche1[[i]][as.numeric(PSMModelTranche1$match.matrix[,5]),]$Borrower_Id.x
  ### Combining controls
  control <- c(control1, control2)
  
  ### Assigning control firms to each treatment firm with treatment firm being replicated nn times  
  datframe <- data.frame(treat=rep(treat, times=nn), control=control)
  ### Taking row number 1 from each dataframe in the list where each dataframe is year
  datframe$year <- rep(listtranche1[[i]]$Tranche_Year[1], each=nrow(datframe))
  ### Adding serial number - serial number ranges from 1 to 3; this is needed to remove serials 2 and 3 of treated to avoid repetition of treated unit
  datframe$serial <- rep(1:nn, each=length(treat)) #ADDED
  ### Populating list with dataframe of treament firm, control firm and year
  df[[i]] <- datframe
  
} 

## Calculating standard deviation of covariates in treated group

###Converting list df to a dataframe known as datframe
df <- do.call(rbind.data.frame, df)
df[!is.na(df$control),]

### Checking if treatment and control firms do not match with each other 
df$what <- ifelse(df$treat==df$control, 1, 0)
## Deleting what column
df$what <- NULL

### Creating empty list 
df_1 <- list()
df_2 <- list()
### i is violation year and j is treated firm
### We first select the violation year and then for that year, we identify the violating firm
## This however builds all the years of trnaches of treated firm
for(i in 1:length(unique(df$year))){
  
  sub <- subset(df, year==unique(df$year)[i]) #subset by year number
  
  for(j in 1:nrow(sub)){
    ### for each year number, identifying treatment firm, treatment year and borrower id
    ### why do i need a treated id column ?
    treat <- subset(data, Borrower_Id==sub$treat[j]) #subset by borrower id of treat
    treat$treated_id <- sub$treat[j] #it's one borrower id assigned to the new variable
    treat$treated_year <- unique(df$year)[i] #it's one borrower id assigned to the new variable
    control <- subset(data, Borrower_Id==sub$control[j]) #subset by borrower id of control
    control$treated_id <- sub$treat[j]
    control$treated_year <- unique(df$year)[i]
    
    ### ---------------Separating POTENTIAL post treatment years and adding number sequence for treatment firm and control firm
    ### -----------------Remember v and up and down dont necessarily contain years, what they have is the year difference
    
    #### treat ####
    ### This is being done for each treated firm in a violation year
    
    #in this section, we regularize the irregular years. Creating the pre and post treatment window. 
    
    treat$year_diff <- -1*(treat$treated_year-treat$Tranche_Year) #collecting all year info for each treatment firm 
    
    v_df <- data.frame(year_diff=unique(treat$year_diff))
    up_v <- subset(v_df, year_diff>0) #identified potential post treatment years
    up_v[1,] <- ifelse(nrow(up_v)==0, NA, up_v[1,]) #check
    #### why did i multiply with -1
    up_v$id <- 1*(1:nrow(up_v)) #added number sequence
    
    #### Separating  POTENTIAL pre treatment years and giving number sequence
    down_v <- subset(v_df, year_diff<0)
    down_v <- arrange(down_v,desc(year_diff))
    down_v[1,] <- ifelse(nrow(down_v)==0, NA, down_v[1,]) #check
    down_v$id <- -1*(1:nrow(down_v))
    #Aggregating pre and post treatment window number sequence (not years)
    v_df <- rbind(up_v, down_v)
    
    #Removing the treatment year and and merging number sequence in the window of pre and post treatment years
    ### Gets confusing here because I created treat before and then I am again creating treat. Same I can say for control
    treat_not0 <- subset(treat, year_diff != 0) #removed violation year
    treat_not0 <- merge(treat_not0, v_df, by="year_diff")
    treat_not0$year_diff <- NULL
    #### Changing names of id in  treat_not 0 to year difference since it wont rbind with treat_0 which does not have id column but year difference column
    names(treat_not0)[names(treat_not0) == 'id'] <- 'year_diff'
    #names(treat_not0)[names(treat_not0) == 'id.x'] <- 'id'
    treat_0 <- subset(treat, year_diff == 0)
    treat <- rbind(treat_0, treat_not0) #adding violation year
    
    ##### controls ####
    
    control$year_diff <- -1*(control$treated_year-control$Tranche_Year)
    
    v_df <- data.frame(year_diff=unique(control$year_diff))
    ###Up_v is potential post treatment year ? and down_v is potential pre treatment year ?
    up_v <- subset(v_df, year_diff>0)
    up_v[1,] <- ifelse(nrow(up_v)==0, NA, up_v[1,])
    up_v$id <- 1*(1:nrow(up_v))
    
    down_v <- subset(v_df, year_diff<0)
    down_v <- arrange(down_v,desc(year_diff))
    down_v[1,] <- ifelse(nrow(down_v)==0, NA, down_v[1,]) #check
    down_v$id <- -1*(1:nrow(down_v))
    
    v_df <- rbind(up_v, down_v)
    
    ### what is control_not0 and control_0?
    control_not0 <- subset(control, year_diff != 0)
    control_not0 <- merge(control_not0, v_df, by="year_diff")
    control_not0$year_diff <- NULL
    names(control_not0)[names(control_not0) == 'id'] <- 'year_diff'
    #names(control_not0)[names(control_not0) == 'id.x'] <- 'id'
    control_0 <- subset(control, year_diff == 0)
    control <- rbind(control_0, control_not0)
    
    ###---------------------- Shortening the window of pre and post treatment years to 3 years pre and 2 years post
    #truncate
    treat <- subset(treat, year_diff>=-4 & year_diff<=4) #pre-3 and post-2 years 
    treat$can_use <- ifelse(max(treat$year_diff)==4 & min(treat$year_diff)==-4, 1, 0) #ADDED
    treat$tyear <- treat$Tranche_Year
    #treatyear <- c(unique(treat$Tranche_Year))
    #ntreat <- length(unique(treat$Tranche_Year))
    treat$status <- 1
    treat$use <- 1
    
    #control <- subset(control, year_diff>=-2 & year_diff<=2)
    ### Instead of subsetting by year_diff, I'm subsetting by the range of the treated unit
    control <- subset(control, Tranche_Year>=min(treat$Tranche_Year) & Tranche_Year<=max(treat$Tranche_Year)) #ADDED
    control$can_use <- ifelse(max(control$Tranche_Year)==max(treat$Tranche_Year) & min(control$Tranche_Year)==min(treat$Tranche_Year), 1, 0) #ADDED
    #control <- control[control$Tranche_Year %in% treatyear,] 
    #ncontrol <- length(unique(control$Tranche_Year))
    control$tyear <- control$Tranche_Year
    control$status <- 0
    control$use <- ifelse(nrow(control)>1, 1, 0)
    control <- subset(control, use==1)
    
    ### See is one stack of treatment and control firm
    
    see <- rbind(treat, control)
    see <- subset(see, use==1)
    see$use <- NULL
    #see$can_use <- ifelse(ntreat==ncontrol, 1,0)
    
    #see$can_use <- ifelse(max(see$year_diff)==2 & min(see$year_diff)==-3, 1, 0) #filtering obs that start at any number greater then -3 and stop at any number less than +2
    ### Checking that if treatment firms have trated firms have treatment year same as tranche year
    see$violate <- ifelse(see$Violator_Remarks==1 & see$treated_year==see$Tranche_Year, 1, 0) #taking violation only in tranche year
    ###--------------- New code (To exclude loan observation in treatment years) 
    ###see <- subset(see, year_diff!= 0) <---- TURNED THIS INTO A COMMENT
    ### Post variable in DID
    see$time = ifelse(see$year_diff > 0, 1, 0) #post
    ### Treatment variable in DID
    see$treated = ifelse(see$treated_id == see$Borrower_Id, 1, 0) #treatment
    ### Post*Treat variable in DID
    see$did = see$time * see$treated
    
    ### Interaction variables for treated and category
    see$treated_HP = see$treated*see$Penalty_Cat
    see$did_HP = see$did*see$Penalty_Cat
    see$time_HP = see$time*see$Penalty_Cat
    see$HP = see$Penalty_Cat
    ### stack is composed of all tranches in the same year for a specific treatment firm and control firms
    
    ### Adding serial number to dataframe
    see$j <- sub$serial[j] #ADDED
    #### Each dataframe in the list consists of the treatment firm and single control firm in a specific year
    df_1[[j]] <- see #it selects a treated unit and the corresponding control units, and iterates it over all years
    
  }
  
  see_df <- do.call(rbind.data.frame, df_1) #ADDED
  ### Multiplying j (serial number) with status (1 vs. 0)
  see_df$keep <- see_df$status*see_df$j #ADDED
  ### Removing any observations where interaction between j and status is more than 1
  see_df <- subset(see_df, keep<=1) #ADDED
  #### Aggregating  all treatment and control firms  for the same year in list where each dataframe corresponds one treatment year
  df_2[[i]] <- see_df
  
  #df_2[[i]] <- do.call(rbind.data.frame, df_1)
  
} 

### Binding  all dataframes in list df_2
df_final <- do.call(rbind.data.frame, df_2) #aggregating all dataframes in list

df_final <- subset(df_final, can_use==1) #must have all six years of data for all treat and control units

#Stack identifier
df_final$stack = df_final$treated_id*df_final$treated_year



#only once begin
df_final$to_remove <- ifelse(df_final$year_diff <= 0 & df_final$year_diff >= -4 & df_final$treated==0 & df_final$Violator_Remarks==1, 1, 0)
tranche <- subset(df_final, to_remove==1)$Tranche_Unique
data2$Tranche_Unique <- as.factor(data2$Tranche_Unique)
tranche <- as.factor(tranche)
data2 <- data2[ ! data2$Tranche_Unique %in% tranche, ]
#only once end



##### Conducting t test
library(dplyr)
t.test(df_final$Current_Asset_ratio_Avg ~ df_final$status)
t.test(df_final$Log_assets_Avg ~ df_final$status)
t.test(df_final$Leverage_ratio_Avg ~ df_final$status)
t.test(df_final$Profitability_ratio_Avg ~ df_final$status)
t.test(df_final$Fixed_Asset_ratio_Avg ~ df_final$status)
t.test(df_final$Tobins_q_Avg ~ df_final$status)
t.test(df_final$Z_score_Avg ~ df_final$status)


#### Normal DID
library(sandwich)
library(lmtest)
### Ln Spread
didreg = lm(Ln_Spread ~ treated + time + did + Current_Asset_ratio.x +
              Log_assets.x + Leverage_ratio.x + Profitability_ratio.x + Fixed_Asset_ratio.x + Tobins_q.x + Z_score.x + 
              Tranche_type_coded+ Primary_Purpose_Coded+ 
              Covenant_coded+ Syndicated_loan_coded + Ln_Tenor+ Collateral_coded + Ln_Tranche_Amount_Converted +
              Bank_Fixed_Effects+ LIBOR_Rate + as.factor(treated_year)+ as.factor(treated_id) +as.factor(stack), data = df_final)

coeftest(didreg, vcov = vcovHC(didreg, type = "HC1"))
(exp(summary(didreg)$coefficients[4])-1)*100

## Tenor
didreg = lm(Ln_Tenor ~ treated + time + did + Current_Asset_ratio.x +
              Log_assets.x + Leverage_ratio.x + Profitability_ratio.x + Fixed_Asset_ratio.x + Tobins_q.x + Z_score.x + 
              Tranche_type_coded+ Primary_Purpose_Coded+ 
              Covenant_coded+ Syndicated_loan_coded + Collateral_coded + Ln_Tranche_Amount_Converted + Ln_Spread +
              Bank_Fixed_Effects+ as.factor(treated_year)+ as.factor(treated_id) +as.factor(stack)
            , data = df_final)

coeftest(didreg, vcov = vcovHC(didreg, type = "HC1"))
(exp(summary(didreg)$coefficients[4])-1)*100

## Tranche amount
didreg = lm(Ln_Tranche_Amount_Converted ~ treated + time + did + Current_Asset_ratio.x +
              Log_assets.x + Leverage_ratio.x + Profitability_ratio.x + Fixed_Asset_ratio.x + Tobins_q.x + Z_score.x + 
              Tranche_type_coded+ Primary_Purpose_Coded+ 
              Covenant_coded+ Syndicated_loan_coded + Ln_Tenor+ Collateral_coded + Ln_Spread + Bank_Fixed_Effects+
              as.factor(treated_year)+ as.factor(treated_id) +as.factor(stack), data = df_final)
coeftest(didreg, vcov = vcovHC(didreg, type = "HC1"))
(exp(summary(didreg)$coefficients[4])-1)*100

### Collateral
didreg = glm(Collateral_coded ~ treated + time + did + Current_Asset_ratio.x +
               Log_assets.x + Leverage_ratio.x + Profitability_ratio.x + Fixed_Asset_ratio.x + Tobins_q.x + Z_score.x + 
               Tranche_type_coded+ Primary_Purpose_Coded+ 
               Covenant_coded+ Syndicated_loan_coded + Ln_Tenor+ Ln_Tranche_Amount_Converted + Ln_Spread + Bank_Fixed_Effects+
               as.factor(treated_year)+ as.factor(treated_id) +as.factor(stack) , data = df_final, family=binomial())
coeftest(didreg, vcov = vcovHC(didreg, type = "HC1"))

#####-------------------------------- Interaction

### Ln Spread with HP
didreg = lm(Ln_Spread ~ treated + time + did + HP+ treated_HP+ did_HP + time_HP + Current_Asset_ratio.x +
              Log_assets.x + Leverage_ratio.x + Profitability_ratio.x + Fixed_Asset_ratio.x + Tobins_q.x + Z_score.x + 
              Tranche_type_coded+ Primary_Purpose_Coded+ 
              Covenant_coded+ Syndicated_loan_coded + Ln_Tenor+ Collateral_coded + Ln_Tranche_Amount_Converted +
              Bank_Fixed_Effects+ as.factor(stack)+ as.factor(treated_year)+ as.factor(treated_id)
            , data = df_final)

coeftest(didreg, vcov = vcovHC(didreg, type = "HC1"))

### Collateral_Coded with HP
didreg = glm(Collateral_coded ~ treated + time + did +HP + treated_HP+ did_HP + time_HP + Current_Asset_ratio.x +
               Log_assets.x + Leverage_ratio.x + Profitability_ratio.x + Fixed_Asset_ratio.x + Tobins_q.x + Z_score.x + 
               Tranche_type_coded+ Primary_Purpose_Coded+ 
               Covenant_coded+ Syndicated_loan_coded + Ln_Tenor+ Ln_Tranche_Amount_Converted + Ln_Spread + Bank_Fixed_Effects+
               as.factor(stack)+ as.factor(treated_year)+ as.factor(treated_id), data = df_final, family=binomial())

coeftest(didreg, vcov = vcovHC(didreg, type = "HC1"))


### Tenor with HP
didreg = lm(Ln_Tenor ~ treated + time + did + HP+ treated_HP+ did_HP + time_HP + Current_Asset_ratio.x +
              Log_assets.x + Leverage_ratio.x + Profitability_ratio.x + Fixed_Asset_ratio.x + Tobins_q.x + Z_score.x + 
              Tranche_type_coded+ Primary_Purpose_Coded+ 
              Covenant_coded+ Syndicated_loan_coded + Collateral_coded + Ln_Tranche_Amount_Converted + Ln_Spread +
              Bank_Fixed_Effects+ as.factor(stack)+ as.factor(treated_year)+ as.factor(treated_id) , data = df_final)

coeftest(didreg, vcov = vcovHC(didreg, type = "HC1"))


### Tranche amount converted with HP
didreg = lm(Ln_Tranche_Amount_Converted ~ treated + time + did + HP+ treated_HP+ did_HP + time_HP + Current_Asset_ratio.x +
              Log_assets.x + Leverage_ratio.x + Profitability_ratio.x + Fixed_Asset_ratio.x + Tobins_q.x + Z_score.x + 
              Tranche_type_coded+ Primary_Purpose_Coded+ 
              Covenant_coded+ Syndicated_loan_coded + Ln_Tenor+ Collateral_coded + Ln_Spread + Bank_Fixed_Effects+
              as.factor(stack)+ as.factor(treated_year)+ as.factor(treated_id) , data = df_final)
coeftest(didreg, vcov = vcovHC(didreg, type = "HC1"))









