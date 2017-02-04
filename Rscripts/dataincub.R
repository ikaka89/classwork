## Load dplyr package
## Github link https://github.com/ikaka89/dataincubator/

library("dplyr")

## Load the datasets
PartD_Prescriber_PUF_NPI_14 <- read.delim("~/PartD_Prescriber_PUF_NPI_14/PartD_Prescriber_PUF_NPI_14.txt", row.names=NULL, quote="")
PARTD_PRESCRIBER_PUF_NPI_13 <- read.delim("~/PartD_Prescriber_PUF_NPI_13/PARTD_PRESCRIBER_PUF_NPI_13.tab", row.names=NULL, quote="")

## Create tbl classes
dat2014 = tbl_df(PartD_Prescriber_PUF_NPI_14)
dat2013 = tbl_df(PARTD_PRESCRIBER_PUF_NPI_13)

# Question 1
group_by(dat2014,NPI) %>%   
  summarise(totBEN = sum(BENE_COUNT))%>% 
  filter(totBEN>10) %>% 
  summarise(mean(totBEN,na.rm=TRUE))

# Question 2
group_by(dat2014,NPI) %>% 
  summarise(totClaim = sum(TOTAL_CLAIM_COUNT), totDaySupp = sum(TOTAL_DAY_SUPPLY)) %>% 
  summarise(median(totDaySupp/totClaim))

#Question 3
filter(dat2014, BRAND_SUPPRESS_FLAG =="") %>% 
  group_by(SPECIALTY_DESCRIPTION) %>% 
  summarise(totClaims = sum(TOTAL_CLAIM_COUNT), totBrandClaims = sum(BRAND_CLAIM_COUNT)) %>% 
  filter(totClaims > 1000) %>% 
  summarise(sd(totBrandClaims/totClaims))

#Question 4
group_by(dat2014,NPPES_PROVIDER_STATE) %>% 
  summarise(opiodTot = sum(OPIOID_BENE_COUNT,na.rm=TRUE), anitTot = sum(ANTIBIOTIC_BENE_COUNT,na.rm=TRUE)) %>% 
  mutate(ratio=opiodTot/anitTot) %>% 
  summarise(max(ratio)-min(ratio))
## turns out top min 5 are not states, so correct that
## 0.5575387

#Question 5
tmp = filter(dat2014,GE65_SUPPRESS_FLAG=="") %>% 
  filter(LIS_SUPPRESS_FLAG=="") %>% 
  mutate(gefrac = TOTAL_CLAIM_COUNT_GE65/TOTAL_CLAIM_COUNT, lisfrac = LIS_CLAIM_COUNT/TOTAL_CLAIM_COUNT) 

cor(tmp$gefrac,tmp$lisfrac,method=c("pearson"))

#Question 6
stateSpecialityPlength = group_by(dat2014,NPI) %>% 
  summarise(claimCount = sum(OPIOID_CLAIM_COUNT,na.rm=TRUE),daysSupply = sum(OPIOID_DAY_SUPPLY,na.rm =TRUE), state = first(NPPES_PROVIDER_STATE),speciality = first(SPECIALTY_DESCRIPTION)) %>%
  mutate(prescLen = ifelse(claimCount == 0, 0 , daysSupply/claimCount), count=1) %>% 
  group_by(state,speciality) %>% 
  summarise(totPresc = sum(count), avgPres=mean(prescLen)) %>% 
  filter(totPresc>100)

SpecialityPlength = group_by(dat2014,NPI) %>% 
  summarise(claimCount = sum(OPIOID_CLAIM_COUNT,na.rm=TRUE),daysSupply = sum(OPIOID_DAY_SUPPLY,na.rm =TRUE), state = first(NPPES_PROVIDER_STATE),speciality = first(SPECIALTY_DESCRIPTION)) %>% 
  mutate(prescLen = ifelse(claimCount == 0, 0 , daysSupply/claimCount), count=1) %>% 
  group_by(speciality) %>% 
  summarise(totPresc = sum(count), avgPres=mean(prescLen)) %>% 
  filter(totPresc>100)

full_join(SpecialityPlength,stateSpecialityPlength,by="speciality") %>% 
  mutate(ratio = avgPres.y/avgPres.x) 
## NC has the highest ratio 

#Question 7
drugCost2014 = select(dat2014,NPI,TOTAL_DRUG_COST,TOTAL_CLAIM_COUNT) %>% 
  mutate(prescCost = TOTAL_DRUG_COST/TOTAL_CLAIM_COUNT)

drugCost2013 = select(dat2013,NPI,TOTAL_DRUG_COST,TOTAL_CLAIM_COUNT) %>% 
  mutate(prescCost = TOTAL_DRUG_COST/TOTAL_CLAIM_COUNT)

drugCostMerged = inner_join(drugCost2013,drugCost2014,by="NPI")
drugCostMerged %>% 
  mutate(inflationRate = prescCost.y/prescCost.x) %>% 
  summarise(mean(inflationRate))

#Question 8
tmp2014 = dat2014 %>% 
  select(NPI,SPECIALTY_DESCRIPTION)
tmp2013 = dat2013 %>% 
  select(NPI,SPECIALTY_DESC)
specChange = inner_join(tmp2013,tmp2014,by="NPI")
out  = specChange %>% 
  mutate(match = ifelse(as.character(SPECIALTY_DESC) == as.character(SPECIALTY_DESCRIPTION), 1 , 0),totBySpec = 1) %>% 
  group_by(SPECIALTY_DESC) %>% 
  summarise(remainingNum = sum(match),origTot = sum(totBySpec)) %>% 
  mutate(fracLeft =(origTot-remainingNum)/origTot) %>% 
  filter(origTot>1000) 

