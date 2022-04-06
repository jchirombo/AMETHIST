pacman::p_load(tidyverse,
               lubridate,
               RDS,
               arsenal,
               pps)

rds_data <- read_csv("data/amethist_rds_survey_raw.csv")
coupon_data <- read_csv("data/amethist_coupon_raw.csv")
hiv_data <- read_csv("data/amethist_results_raw.csv")

# some data management
# seeds
k <- which(coupon_data$ctype==1)
seedID <- coupon_data[k,"pid"]$pid

# function to organize data into rds format
get_recruiter_id <- function(data){
  idx <- rep(NA,length(nrow(data)))
  for(i in 1:nrow(data)){
    j <- which(data$recruit_id[i] == data$seed_coupon1|data$recruit_id[i] == data$seed_coupon2)
    if(length(j)==0){
      idx[i] <- "Seed" 
    } else {
      idx[i] <- data$pid[j]
    }
  }
  return(idx)
}

recruit_id2 <- rep(NA,length = nrow(coupon_data))
for(i in 1:nrow(coupon_data)){
  x <- which(coupon_data$recruit_id[i] == coupon_data$seed_coupon1|coupon_data$recruit_id[i] == coupon_data$seed_coupon2)
  if(length(x)==0){
    recruit_id2[i] <- "Seed" 
  } else {
    recruit_id2[i] <- coupon_data$pid[x]
  }
}
recruit_id2
seed_val <- coupon_data[,c("pid","recruit_id","seed_coupon1","seed_coupon2")]

rds <- rds_data %>%   
  rename(sex_pst_mnth=elig01,knwDOB=elig02,compltsvy=elig03,lengthstay=elig04,findplace=elig05a,
         onlinefind=elig05b,mrktfind=elig05c,lodgefind=elig05d,trucksfind=elig05e,nonefind=elig05f,
         chargefee=elig06,knwpersoncoupon=elig07,knowpersoncoupon2=elig07i,
         educ=a02,marstatus=a03,liveLastMnth=a04,liveLastMnth2=a04i,hhsize=a05,
         childsupp=a06,regincome=a07,toilet=a08,electricity=a09a,fridge=a09b,stove=a09c,
         tapwater=a09d,livestock=a09e,bicycle=a09f,motorcycle=a09g,car=a09h,cart=a09i,wheelbarrow=a09j,
         phone=a09k,radio=a09l,tv=a09m,accnt=a10,phoneuse=a11,sexworkerKnow=o01,
         sexworkerSeen=o02,sexworkerRecruit=o03,agewomenrecruit1=o04a,agewomenrecruit2=o04b,
         sexWoker=a24,agesexwork=a25,sexbreak=a26,sexbreakyr=a27,income=a28,
         hivtestplace=c01,hivtested=c02,whenhivtest=c03,wherehivTested=c04,
         hivres=c05,whenpostest=c06,hivrisk=c07,dailyprotect=c08,
         condomprev=b01,hivfoodsharing=b02,hivtablets=b03,tabletsrisk=b04,
         healthpersonHIV=b05,hivARV=b06,viralLoadRisk=b07,pregHIVbaby=b08,
         pepknow=c09,willingpep=c10,pepreason=c11,offerdpep=c12,takenpep=c13,
         prpepknow=c14,willingprpep=c15,prpepreason=c16,offeredprpep=c17,
         takenprpep=c18,prpephowlong=c19,takingprpepnow=c20,whynotprpep=c21,
         prpeppastmonth=c22,prpeppastmonthreason=c23,whereprpep=c24,
         prpepprotect=c26,womenwillingprpep=c27) %>%
  mutate(data_date=dmy(data_date)) %>%
  filter(!is.na(pid)) %>%
  mutate(seedpid=case_when(
    knowpersoncoupon2=="Ndi seed" ~ "seed",
    knowpersoncoupon2=="She is a seed" ~ "seed",
    knowpersoncoupon2=="Seed" ~ "seed")) %>%
  mutate(educ=factor(case_when(
    educ==0 ~ "Never",
    educ==1 ~ "Primary",
    educ==3 ~ "JCE",
    educ==4 ~ "MSCE",
    educ==5 ~ "Tertiary"),levels = c("Never","Primary","JCE","MSCE","Tertiary"))) %>%
  mutate(sex_pst_mnth=case_when(
    sex_pst_mnth==0 ~ "No",
    sex_pst_mnth==1 ~ "Yes")) %>%
  mutate(lengthstay=factor(case_when(
    lengthstay==0 ~ "< 6 months",
    lengthstay==1 ~ "1-6 months",
    lengthstay==2 ~ "> 6 months"),levels = c("< 6 months","1-6 months","> 6 months"))) %>%
  mutate(findbar=factor(case_when(
    findplace==0 ~ "No",
    findplace==1 ~ "Yes"),levels = c("Yes","No"))) %>%
  mutate(findonline=factor(case_when(
    onlinefind==0 ~ "No",
    onlinefind==1 ~ "Yes"),levels = c("Yes","No"))) %>%
  mutate(mrktfind=factor(case_when(
    mrktfind==0 ~ "No",
    mrktfind==1 ~ "Yes"),levels = c("Yes","No"))) %>%
  mutate(lodgefind=factor(case_when(
    lodgefind==0 ~ "No",
    lodgefind==1 ~ "Yes"),levels = c("Yes","No"))) %>%
  mutate(trucksfind=factor(case_when(
    trucksfind==0 ~ "No",
    trucksfind==1 ~ "Yes"
  ),levels = c("Yes","No"))) %>%
  mutate(nonefind=factor(case_when(
    nonefind==0 ~ "No",
    nonefind==1 ~ "Yes"
  ),levels = c("Yes","No"))) %>%
  mutate(chargefee==factor(case_when(
    chargefee==0 ~ "< 1000",
    chargefee==1 ~ "1000-5000",
    chargefee==2 ~ "5000-10,000",
    chargefee==3 ~ "10,000-20,000",
    chargefee==4 ~ "> 20,000",
    chargefee==5 ~ "Don't know"
  ),levels = c("< 1000","1000-5000","5000-10,000","10,000-20,000","> 20,000","Don't know"))) %>%
  mutate(knowpersoncoupon=factor(case_when(
    knwpersoncoupon==1 ~ "Close friend",
    knwpersoncoupon==2 ~ "Friend",
    knwpersoncoupon==3 ~ "Acquaintance/Colleague",
    knwpersoncoupon==4 ~ "Stranger",
    knwpersoncoupon==5 ~ "Relative",
    knwpersoncoupon==6 ~ "Other"
  ),levels = c("Close friend","Friend","Acquaintance/Colleague","Stranger","Relative","Other"))) %>%
  mutate(marstatus=factor(case_when(
    marstatus==0 ~ "Never married",
    marstatus==1 ~ "Widow",
    marstatus==2 ~ "Divorced/Separated",
    marstatus==3 ~ "Cohabiting",
    marstatus==4 ~ "Married but seperated",
    marstatus==5 ~ "Married living together"
  ),levels = c("Never married","Widow","Divorced/Separated","Cohabiting","Married but seperated","Married living together"))) %>%
  mutate(liveLastMnth=factor(case_when(
    liveLastMnth==1 ~ "Rented bar room",
    liveLastMnth==2 ~ "Rented bar room, shared",
    liveLastMnth==3 ~ "Own house shared",
    liveLastMnth==4 ~ "Parent's house",
    liveLastMnth==5 ~ "Hostel room",
    liveLastMnth==6 ~ "Lodge/Hotel",
    liveLastMnth==7 ~ "Other"))) %>%
  mutate(agecat=factor(case_when(
    calc_age < 18 ~ "< 18",
    calc_age >=18 & calc_age < 25 ~ "18-24",
    calc_age >=25 & calc_age < 33 ~ "25-32",
    calc_age >=33 & calc_age < 40 ~ "33-39",
    calc_age >=40 & calc_age < 47 ~ "40-46",
    calc_age >=47 & calc_age < 55 ~ "47-54",
    calc_age >= 54 ~ "55+"
  ),levels = c("18-24","25-32","33-39","40-46","47-54","55+"))) %>%
  mutate(sexWoker=factor(case_when(
    sexWoker==1 ~ "Yes",
    sexWoker==0 ~ "No",
    sexWoker==2 ~ "Sometimes"
  ),levels = c("Yes","No","Sometimes"))) %>%
  mutate(sexbreak=factor(case_when(
    sexbreak==1 ~ "Yes",
    sexbreak==0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(sexbreakyr=factor(case_when(
    sexbreakyr==1 ~ "Yes",
    sexbreakyr==0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(income=factor(case_when(
    income==1 ~ "Very little",
    income==2 ~ "Some",
    income==3 ~ "More than half",
    income==4 ~ "All"))) %>%
  mutate(hivtestplace=factor(case_when(
    hivtestplace==1 ~ "Yes",
    hivtestplace==0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(hivtested=factor(case_when(
    hivtested==1 ~ "Yes",
    hivtested==0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(whenhivtest=factor(case_when(
    whenhivtest==1 ~ "Last 3 months",
    whenhivtest==2 ~ "Last 6 months",
    whenhivtest==3 ~ "Last 12 months",
    whenhivtest==4 ~ "1-2 yrs",
    whenhivtest==5 ~ "> 2 yrs"
  ),levels = c("Last 3 months","Last 6 months","Last 12 months","1-2 yrs","> 2 yrs"))) %>%
  mutate(hivres=factor(case_when(
    hivres==0 ~ "HIV -ve",
    hivres==1 ~ "HIV +ve",
    hivres==888 ~ "Don't know"
  ),levels = c("HIV +ve","HIV -ve","Don't know"))) %>%
  mutate(whenpostest==factor(case_when(
    whenpostest==1 ~ "Last 3 months",
    whenpostest==2 ~ "Last 6 months",
    whenpostest==3 ~ "Last 12 months",
    whenpostest==4 ~ "1-2 yrs",
    whenpostest==5 ~ "> 2 yrs"
  ),levels = c("Last 3 months","Last 6 months","Last 12 months","1-2 yrs","> 2 yrs"))) %>%
  mutate(hivrisk=factor(case_when(
    hivrisk==0 ~ "None",
    hivrisk==1 ~ "Low",
    hivrisk==2 ~ "Medium",
    hivrisk==3 ~ "High",
    hivrisk==888 ~ "Don't know" 
  ),levels = c("None","Low","Medium","High","Don't know"))) %>%
  mutate(dailyprotect==factor(case_when(
    dailyprotect==1 ~ "Yes",
    dailyprotect==0 ~ "No",
    dailyprotect==888 ~ "Don't know"
  ),levels = c("Yes","No","Don't know"))) %>%
  mutate(wherehivTested=factor(case_when(
    wherehivTested==1 ~ "Govt Hosp",
    wherehivTested==2 ~ "Pakachere DIC",
    wherehivTested==3 ~ "ANC",
    wherehivTested==4 ~ "Private doctor",
    wherehivTested==5 ~ "Mission hosp",
    wherehivTested==6 ~ "Mobile testing centres",
    wherehivTested==7 ~ "BLM",
    wherehivTested==8 ~ "MACRO",
    wherehivTested==9 ~ "Other"
  ),levels = c("ANC","Govt Hosp","Pakachere DIC","Mission hosp","BLM","MACRO","Private doctor","Mobile testing centres","Other"))) %>%
  mutate(condomprev=factor(case_when(
    condomprev==1 ~ "Yes",
    condomprev==0 ~ "No",
    condomprev==888 ~ "I don't know"
  ),levels = c("Yes","No","I don't now"))) %>%
  mutate(hivfoodsharing=factor(case_when(
    hivfoodsharing==1 ~ "Yes",
    hivfoodsharing==0 ~ "No",
    hivfoodsharing==888 ~ "I don't know"
  ),levels = c("Yes","No","I don't know"))) %>%
  mutate(tabletsrisk=factor(case_when(
    tabletsrisk==1 ~ "Less than most of the time",
    tabletsrisk==0 ~ "Most of the time",
    tabletsrisk==2 ~ "Never"
  ),levels = c("Never","Most of the time","Less than most of the time"))) %>%
  mutate(hivtablets=factor(case_when(
    hivtablets==1 ~ "Yes",
    hivtablets==0 ~ "No",
    hivtablets==888 ~ "I don't know"))) %>%
  mutate(healthpersonHIV=factor(case_when(
    healthpersonHIV==1 ~ "Yes",
    healthpersonHIV==0 ~ "No",
    healthpersonHIV==888 ~ "I don't know"
  ),levels = c("Yes","No","I don't know"))) %>%
  mutate(hivARV=factor(case_when(
    hivARV==1 ~ "Yes",
    hivARV==0 ~ "No",
    hivARV==888 ~ "I don't know"
  ),levels = c("Yes","No","I don't know"))) %>%
  mutate(viralLoadRisk=factor(case_when(
    viralLoadRisk==0 ~ "High",
    viralLoadRisk==1 ~ "Medium",
    viralLoadRisk==2 ~ "Low",
    viralLoadRisk==3 ~ "Very Low",
    viralLoadRisk==4 ~ "Zero"
  ),levels = c("Zero","Very Low","Low","Medium","High"))) %>%
  mutate(pregHIVbaby=factor(case_when(
    pregHIVbaby==1 ~ "Yes",
    pregHIVbaby==0 ~ "No",
    pregHIVbaby==888 ~ "I don't know"
  ),levels = c('Yes',"No","I don't know"))) %>%
  mutate(pepknow=factor(case_when(
    pepknow==1 ~ "Yes",
    pepknow==0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(willingpep=factor(case_when(
    willingpep==1 ~ "Yes",
    willingpep==0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(pepreason=factor(case_when(
    pepreason==0 ~ "Not sure about access",
    pepreason==1 ~ "Don't understand",
    pepreason==2 ~ "Don't know side effects",
    pepreason==3 ~ "Don't like pills everyday",
    pepreason==4 ~ "People will think I am HIV+",
    pepreason==5 ~ "Use condoms",
    pepreason==6 ~ "Other",
    pepreason==888 ~ "Don't know"
  ),levels = c("Not sure about access","Don't understand","Don't know side effects","Don't like pills everyday","People will think I am HIV+","Use condoms","Other","Don't know"))) %>%
  mutate(offerdpep=factor(case_when(
    offerdpep==1 ~ "Yes",
    offerdpep==0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(takenpep=factor(case_when(
    takenpep==1 ~ "Yes",
    takenpep==0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(seedInfo=ifelse(pid %in% seedID,"seed","recruitee"))
  
# process hiv result data
hivdata <- hiv_data %>%
  select(data_date,pid,syphresult,determine,unigold,hivresult) %>%
  mutate(data_date=as.Date(data_date,format="%d%b%Y")) %>%
  mutate(hivstatus=factor(ifelse(hivresult==1,"Positive","Negative"),levels = c("Positive","Negative")),
         syphstatus=factor(ifelse(syphresult==1,"Positive","Negative"),levels = c("Positive","Negative")))
  
  
# merge rds and hiv results data
rdsfinal <- left_join(hivdata,rds,by="pid")

## ------------------------------------------------------------------------------------------------------------------
## -----------------------------------RDS diagnostics----------------------------------------------------------------
dat_rds <- rds %>%
  select(pid,sexworkerKnow,seedInfo) %>%
  rename(networksize=sexworkerKnow) %>%
  mutate(maxcoupon=3) %>%
  filter(!is.na(pid)) %>%
  add_column(idx=1:nrow(dat_rds),.before = "pid")

dat_rds$pid[dat_rds$seedInfo=="seed"] <- 0
M <- as.rds.data.frame(dat_rds,id="idx",recruiter.id = "pid",max.coupons = "maxcoupon",network.size = "networksize")
# demographic characteristics
# baseline tables
tabcontrols  <- tableby.control(test=F, total=TRUE,
                                numeric.test="kwt", cat.test="fe",  
                                numeric.stats=c("N","medianq1q3"),
                                cat.stats=c("N","countpct"),
                                digits = 1,
                                digits.pct = 1,
                                digits.p = 2,
                                stats.labels=list(
                                  N='N',
                                  meansd="Mean(SD)",
                                  medianq1q3='Median(IQR)'),
                                simulate.p.value = T)

tablabs <- list(calc_age="Age",lengthstay="Length of stay",
                findbar="Bars/Nightclubs/Entertaiment place",
                findonline="Phone/WhatsApp/Internet",
                agecat="Age group",
                syphstatus="Syphilis",
                mrktfind="Market place/Street",
                trucksfind="Trucks/Highway",
                lodgefind="Lodge/Hotel/Shabeen",
                educ="Education",
                marstatus="Marital status",
                hhsize="HH size",
                knwpersoncoupon="Coupon giver",
                sexworkerKnow="Sex workers known",
                sexworkerSeen="Sex seen in past month",
                sexworkerRecruit="Sex worker recruited")

tab1 <- tableby(hivstatus~calc_age+agecat+syphstatus+marstatus+lengthstay+findbar+findonline+mrktfind+trucksfind+
                  lodgefind+educ+knwpersoncoupon+sexworkerKnow +
                  sexworkerSeen + sexworkerRecruit,
                data = rdsfinal,control = tabcontrols)
U = summary(tab1,labelTranslations = tablabs,text = T)
write2word(U,"baseline_table_characteristics.doc")



U <- prop.table(table(rds$findonline,rds$educ),2)*100
W <- prop.table(table(rds$findbar,rds$educ),2)*100
Q <- prop.table(table(rds$mrktfind,rds$educ),2)*100
barplot(U,beside = T)

# baseline figures
datasumm <- rds %>%
  filter(!is.na(educ)) %>%
  group_by(educ,findonline) %>%
  summarise(nres=n_distinct(pid)) %>%
  mutate(nprop=nres/sum(nres)*100)
 
datasumm2 <- rds %>%
  filter(!is.na(educ)) %>%
  group_by(educ,findbar) %>%
  summarise(nres=n_distinct(pid)) %>%
  mutate(nprop=nres/sum(nres)*100) 

# graphs by location 
ggplot(datasumm2,aes(x=educ,y=nprop,fill=findbar)) +
  geom_bar(stat = "identity",position = position_dodge(preserve = "single"))+
  theme_minimal() +
  labs(title = "Client meeting location by education",subtitle = "Bar/Nightclubs/Entertainment place") +
  ylab("Proportion")+
  xlab("") +
  scale_fill_manual(values = c("lightsalmon",'lightblue'))+
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.title = element_blank(),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 25))
ggsave("images/meeting_place_bar_education.tiff",width = 500,height = 450,units = "mm",compression="lzw")


ggplot(datasumm,aes(x=educ,y=nprop,fill=findonline)) +
  geom_bar(stat = "identity",position = position_dodge(preserve = "single"))+
  #geom_text(aes(label=round(nprop,1)),hjust=0.5,position = position_dodge(0.7),size=6) +
  theme_minimal() +
  labs(title = "Client meeting location by education",subtitle = "Telephone/WhatsApp/Internet") +
  ylab("Proportion")+
  xlab("") +
  scale_fill_manual(values = c("lightsalmon",'lightblue'))+
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.title = element_blank(),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 25))
ggsave("images/meeting_place_online_education.tiff",width = 500,height = 450,units = "mm",compression="lzw")


ggplot(datasumm,aes(x=educ,y=nprop,fill=findbar)) +
  geom_bar(stat = "identity",position = position_dodge(preserve = "single"))+
  #geom_text(aes(label=round(nprop,1)),hjust=0.5,position = position_dodge(0.7),size=6) +
  theme_minimal() +
  labs(title = "Client meeting location by education",subtitle = "Bar") +
  ylab("Proportion")+
  xlab("") +
  scale_fill_manual(values = c("lightsalmon",'lightblue'))+
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.title = element_blank(),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 25))


# plotting function
plot_meeting_location <- function(data,meeting_place,meeting_place_title){
  ggplot(datasumm,aes(x=educ,y=nprop,fill=meeting_place)) +
    geom_bar(stat = "identity",position = position_dodge(preserve = "single"))+
    geom_text(aes(label=round(nprop,1)),hjust=0.5,position = position_dodge(0.7),size=6) +
    theme_minimal() +
    labs(title = "Client meeting location by education",subtitle = meeting_place_title) +
    ylab("Proportion")+
    scale_fill_manual(values = c("lightsalmon",'lightblue'))+
    theme(axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          legend.text = element_text(size = 28),
          legend.title = element_blank(),
          plot.title = element_text(size = 30),
          plot.subtitle = element_text(size = 25))
}

# convert data to long format
rdsLong <- rds %>%
  pivot_longer(cols = c("findbar","findonline","mrktfind","lodgefind","trucksfind"),names_to = "loc_type",values_to = "location")%>%
  mutate(location_type=factor(case_when(
    loc_type=="findbar" ~ "Bar",
    loc_type=="findonline" ~ "Online",
    loc_type=="mrktfind" ~ "Market",
    loc_type=="lodgefind" ~ "Lodge",
    loc_type=="trucksfind" ~ "Trucks"
  )))


rdsLocation <- rdsLong %>%
  #filter(!is.na(educ))+
  group_by(location,location_type) %>%
  summarise(nres=n_distinct(pid)) %>%
  mutate(nprop=nres/sum(nres)*100)

ggplot(rdsLocation,aes(x=location_type,y=nprop)) +
  geom_bar(stat = "identity")+
  theme_minimal() +
  ylim(0,100)+
  ylab("Proportion")+
  #facet_wrap(~location_type)+
  scale_fill_manual(values = c("lightsalmon",'lightblue'))+
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.title = element_blank())

# hiv testing location
datatesting <- rds %>%
  filter(!is.na(wherehivTested)) %>%
  group_by(wherehivTested) %>%
  summarise(nres=n_distinct(pid)) %>%
  mutate(nprop=nres/sum(nres)*100)

ggplot(datatesting,aes(x=wherehivTested,y=nprop))+
  geom_bar(stat = "identity",fill="steelblue")+
  theme_minimal() +
  xlab("")+
  ylab("Proportion") +
  ylim(0,100)+
  coord_flip()+
  labs(title = "Where HIV test was done")+
  theme(axis.text.x = element_text(size = 38),
        axis.text.y = element_text(size = 38),
        axis.title.y = element_text(size = 38),
        axis.title.x = element_text(size = 38),
        plot.title = element_text(size = 40))
ggsave("images/last_hiv_test_place.tiff",width = 500,height = 450,units = "mm",compression="lzw")


lastTest <- rds %>%
  filter(!is.na(whenhivtest))%>%
  group_by(whenhivtest) %>%
  summarise(nres=n_distinct(pid)) %>%
  mutate(nprop=nres/sum(nres)*100)


h1 <- ggplot(lastTest,aes(x=whenhivtest,y=nprop))+
  geom_bar(stat = "identity",fill="salmon")+
  theme_minimal() +
  xlab("")+
  ylab("Proportion") +
  ylim(0,100)+
  labs(title = "When last HIV test was done")+
  coord_flip()+
  theme(axis.text.x = element_text(size = 38),
        axis.text.y = element_text(size = 38),
        axis.title.y = element_text(size = 38),
        axis.title.x = element_text(size = 38),
        plot.title = element_text(size = 40))
ggsave("images/last_hiv_test.tiff",width = 500,height = 450,units = "mm",compression="lzw")

# network sizes
agepositive <- rds %>%
  filter(!is.na(hivres)) %>%
  group_by(hivres,agecat) %>%
  summarise(nres=n_distinct(pid)) %>%
  mutate(nprop=nres/sum(nres)*100)

ggplot(agepositive,aes(x=agecat,y=nprop,fill=hivres))+
  geom_bar(stat = "identity",position = position_dodge(preserve = "single"))+
  theme_minimal()+
  xlab("")+
  ylab("Proportion") +
  ylim(0,100) +
  labs(title = "HIV status by age") +
  scale_fill_manual(values = c("salmon","steelblue","gold"))+
  theme(axis.text = element_text(size = 38),
        axis.title = element_text(size = 38),
        legend.title = element_blank(),
        legend.text = element_text(size = 33),
        legend.position = c(.87,.87),
        plot.title = element_text(size = 40))
ggsave("images/hiv_age.tiff",width = 500,height = 450,units = "mm",compression="lzw")

# network by age
networkData <- rds %>%
  group_by(agecat) %>%
  summarise(nres=n_distinct(pid),
            nnsize=median(sexworkerKnow))
# characteristics by sex outcome

# break in sex work over the past 6 month
breakdata <- rds %>%
  filter(!is.na(sexbreak))%>%
  filter(!is.na(hivres)) %>%
  group_by(sexbreak,hivres) %>%
  summarise(nres=n_distinct(pid)) %>%
  mutate(nprop=nres/sum(nres)*100)

ggplot(breakdata,aes(x=sexbreak,y=nprop,fill=hivres))+
  geom_bar(stat = "identity",position = position_dodge())+
  theme_minimal()+
  xlab("")+
  ylab("Proportion") +
  ylim(0,100) +
  labs(title = "Break in sex work in the past 6 months by HIV status") +
  scale_fill_manual(values = c("salmon","gold","steelblue"))+
  theme(axis.text = element_text(size = 38),
        axis.title = element_text(size = 38),
        legend.title = element_blank(),
        legend.text = element_text(size = 33),
        legend.position = c(.87,.87),
        plot.title = element_text(size = 40))
ggsave("images/sex_work_break.tiff",width = 500,height = 450,units = "mm",compression="lzw")

# future HIV risk
futurerisk <- rds %>%
  filter(!is.na(hivrisk))%>%
  #filter(!is.na(hivres)) %>%
  group_by(hivrisk) %>%
  summarise(nres=n_distinct(pid)) %>%
  mutate(nprop=nres/sum(nres)*100)

h2 <- ggplot(futurerisk,aes(x=hivrisk,y=nprop))+
  geom_bar(stat = "identity",fill="salmon")+
  theme_minimal() +
  xlab("")+
  ylab("Proportion") +
  ylim(0,100)+
  labs(title = "HIV risk in the next 5 years")+
  #coord_flip()+
  theme(axis.text.x = element_text(size = 38),
        axis.text.y = element_text(size = 38),
        axis.title.y = element_text(size = 38),
        axis.title.x = element_text(size = 38),
        plot.title = element_text(size = 40))
ggsave("images/future_hiv_risk.tiff",width = 500,height = 450,units = "mm",compression="lzw")

gridExtra::grid.arrange(h1,h2)
ggsave("images/testing_future_risk.tiff",height = 600,width = 500,units = "mm",compression="lzw")

#--------------------- diary survey sampling ------------------------------
set.seed(309)
diary_data_pos <- rdsfinal %>%
  filter(hivstatus=="Positive") %>%
  #group_by(hivstatus) %>%
  slice_sample(n=60)

set.seed(230)
diary_data_neg <- rdsfinal %>%
  filter(hivstatus=="Negative") %>%
  slice_sample(n=40)

diary_data <- bind_rows(diary_data_neg,diary_data_pos)
write.csv(diary_data,"data/diary_data.csv",row.names = F)
