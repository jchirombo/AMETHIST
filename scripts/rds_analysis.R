pacman::p_load(tidyverse,
               lubridate,
               RDS,
               lme4,
               arsenal,
               stringr,
               tmap,sf,
               tmaptools,
               htmlwidgets)

# load functions
source("scripts/rds_analysis_functions.R")

rds_data <- read_csv("data/amethist_rds_survey_raw.csv")
coupon_data <- read_csv("data/amethist_coupon_raw240522.csv")
hiv_data <- read_csv("data/amethist_results_raw.csv")
chlamydia_data <- read_csv("data/chlamydia_data.csv")
pts <- read_csv("data/Pakachere _MLW _coords.csv")
locator_data <- read_csv("data/amethist_locator_final.csv")
blantyre <- st_read("data/blantyre_shapefiles/blantyre_district_boundary.shp")
blantyre_city <- st_read("data/blantyre_shapefiles/blantyre_city_boundary.shp")

# process spatial data
spatial_dat_dir <- "C:/Users/jchirombo/Dropbox/GIS_malawi/Enumeration areas 2018"
spatial_dat <- st_read(paste(spatial_dat_dir,"2018_Malawi_Enumeration_Areas.shp",sep="/"))
spatial_BT_city <- spatial_dat |>
  filter(DIST_NAME == "Blantyre City") |>
  st_transform(4326)

ndirande <- filter(spatial_BT_city, TA_NAME %in% c("Ndirande Matope Ward","Ndirande Makata Ward","Ndirande Gamulani Ward")) %>%
  st_union()

mbayani <- filter(spatial_BT_city,TA_NAME == "Mbayani Ward") %>%
  st_union()

bangwe <- filter(spatial_BT_city,TA_NAME %in% c("Bangwe Ward","Bangwe Mthandizi Ward","Namiyango Ward")) %>%
  st_union()

spatial_BT_rural <- spatial_dat |>
  filter(DIST_NAME == "Blantyre") |>
  st_transform(4326) 

# blantyre locations
blantyre_pts <- st_as_sf(pts,coords = c("Latitude","Longitude"),crs=4326)

# plot interactive maps

BT_areas <- leaflet() %>%
  setView(lat = -15.77,lng = 35.04,zoom = 5) %>%
  addTiles() %>%
  addPolylines(data = bangwe,color = "#9c2b86",fillOpacity = 0.3) %>%
  addPolylines(data = ndirande,color = "#9c2b86",fillOpacity = 0.3) %>%
  addPolylines(data = mbayani,color = "#9c2b86",fillOpacity = 0.3) %>%
  addMarkers(lat = pts$Latitude,lng = pts$Longitude,label = pts$Institution,labelOptions = labelOptions(noHide = FALSE,textsize = "16px"))

saveWidget(BT_areas,"data/hotspot_areas.html")


# plot locations
bt_bbox <- st_bbox(blantyre_city)
osm_base <- read_osm(bt_bbox)
tm_shape(blantyre_city) +
  tm_polygons() +
  tm_shape(blantyre_pts) +
  tm_symbols()

# ----------------------- process locator data ----------------------------------------

locator_data <- locator_data %>%
  select(-redcap_event_name,-redcap_repeat_instrument,-redcap_repeat_instance,-pid1)


# some data management
# seeds
k <- which(coupon_data$ctype==1)
seedID <- coupon_data[k,"pid"]$pid

# isolate and remove some rows
#coupon_data <- coupon_data[-c(8,27),]

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

# add recruiter id column
coupon_data_final <- coupon_data %>%
  add_column(recruit_id2=get_recruiter_id(data = coupon_data),.after="recruit_id") %>%
  select(pin,data_date,pid,recruit_id2) %>%
  rename(recruit_id=recruit_id2) %>%
  mutate(data_date=dmy(data_date))

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
  mutate(seedInfo=ifelse(pid %in% seedID,"seed","recruitee")) %>%
  mutate(think_deep = factor(ifelse(f01==1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(violence01 = factor(ifelse(m01==1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(drink_freq = factor(case_when(
    n01==0 ~ "Never",
    n01==1 ~ "Once a month or less",
    n01==2 ~ "2-4 times/month",
    n01==3 ~ "2-3 times/week",
    n01==4 ~ ">=4 times/week"
  ),levels = c("Never","Once a month or less","2-4 times/month","2-3 times/week",">=4 times/week"))) %>%
  mutate(drink_dy=factor(case_when(
    n02==0 ~ "1 or 2",
    n02==1 ~ "3 or 4",
    n02==2 ~ "5 or 6",
    n02==3 ~ "7,8 or 9",
    n02==4 ~ ">=10"),levels = c("1 or 2","3 or 4","5 or 6","7,8 or 9",">=10"))) %>%
  mutate(drink_occ=factor(case_when(
    n03==0 ~ "Never",
    n03==1 ~ "Less than montly",
    n03==2 ~ "Monthly",
    n03==3 ~ "Weekly",
    n03==4 ~ "Daily or almost daily",
    n03==5 ~ "Other"),levels = c("Never","Less than montly","Monthly","Weekly","Daily or almost daily","Other"))) %>%
  mutate(agegrp = ifelse(calc_age <= 25,1,2))
  
# process hiv result data
hivdata <- hiv_data %>%
  select(data_date,pid,syphresult,determine,unigold,hivresult) %>%
  mutate(data_date=as.Date(data_date,format="%d%b%Y")) %>%
  mutate(hivstatus=factor(ifelse(hivresult==1,"Positive","Negative"),levels = c("Positive","Negative"))) %>%
  mutate(syphstatus=factor(ifelse(syphresult==1,"Positive","Negative"),levels = c("Positive","Negative")))
  
# process chlamydia data
chlamydia <- chlamydia_data %>%
  select(PATIENT_CODE,PATIENT_BIRTH,RESULT_TEST_NAME,RESULT) %>%
  rename(pid=PATIENT_CODE,DOB=PATIENT_BIRTH,test_name=RESULT_TEST_NAME,result=RESULT)%>%
  mutate(DOB=as.Date(DOB,format="%d/%m/%Y")) %>%
  mutate(result=factor(result,levels = c("Detected","Not Detected"))) %>%
  mutate(pid=gsub("-","",pid))

# gonorrhea
gonorrhea_test <- chlamydia %>%
  filter(test_name=="Chlamydia trachomatis (CT)")

chlamydia_test <- chlamydia %>%
  filter(test_name=="Neisseria gonorrhoeae (NG)")
# RDS098 and RDS04111 not available in the main data

U <- left_join(hivdata,rds,by="pid") %>% 
  left_join(.,coupon_data_final,by="pid") %>%
  left_join(.,chlamydia) %>%
  select(-data_date.x,-deviceid,-odk_id,-odk_subdate,-data_date.y,-pin.y,-pin.x,-project_dsid)


# merge rds and hiv results data
#rdsfinal <- left_join(hivdata,rds,by="pid")
rdsfinal <- left_join(hivdata,rds,by="pid") %>% 
  left_join(.,coupon_data_final,by="pid") %>%
  left_join(.,locator_data,by="pid") %>%
  select(-data_date.x,-deviceid,-odk_id,-odk_subdate,-data_date.y,-pin.y,-pin.x,-project_dsid)

# perform another merge
datlist <- list(rds,hivdata,coupon_data_final)
datamerge <- datlist %>%
  reduce(left_join,by="pid")
# ------------------------------------ regroup some locations to get bigger numbers ---------------------------------

## -----------------------------------RDS diagnostics----------------------------------------------------------------
# stratify the ages by two age groups
rdsfinal <- rdsfinal %>%
  mutate(age=factor(ifelse(calc_age<=30,"Young","Adult"),levels = c("Young","Adult")))

# make chains for RDS0536 and RDS0544 for now until issue resolved
# assign random recruiters
j <- c(55,60,62,63)
finaldata <- rdsfinal
#finaldata[j,"recruit_id"] <- c("RDS0387","RDS0494","RDS0270","RDS0304")
finaldata[j,"recruit_id"] <- c("RDS0494","RDS0494","RDS0502","RDS0502")

# -------------------------------------------------------------------------------------------------------------------
# combine some locations to obtain bigger numbers
finaldata$locxn[finaldata$locxn=="BCA"] <- "Bangwe"
finaldata$locxn[finaldata$locxn=="Bvumbwe"] <- "Chigumula"
finaldata$locxn[finaldata$locxn=="Chazunda"] <- "Chadzunda"
finaldata$locxn[finaldata$locxn=="Chemusa"] <- "Mbayani"
finaldata$locxn[finaldata$locxn=="Nancholi"] <- "Manase"
finaldata$locxn[finaldata$locxn=="Chichiri"] <- "Blantyre"
finaldata$locxn[finaldata$locxn=="Chirimba"] <- "Mbayani"
finaldata$locxn[finaldata$locxn=="Mpingwe"] <- "Machinjiri"
finaldata$locxn[finaldata$locxn=="Chiwembe"] <- "Soche (Manje)" # consider combining soche, chilobwe and zingwangwa
finaldata$locxn[finaldata$locxn=="Mpemba"] <- "Manase"
finaldata$locxn[finaldata$locxn=="Chadzunda"] <- "Zingwangwa"
finaldata$locxn[finaldata$locxn=="Chilobwe"] <- "Zingwangwa"
finaldata$locxn[finaldata$locxn=="Soche (Manje)"] <- "Zingwangwa"
finaldata$locxn[finaldata$locxn=="Chigumula"] <- "Bangwe"
finaldata$locxn[finaldata$locxn=="Chileka"] <- "Kameza"
finaldata$locxn[finaldata$locxn=="Chilomoni"] <- "Blantyre"


# combine topology
#finaldata$topology[finaldata$topology=="Social Media"] <- "Home based"
finaldata$topology[finaldata$topology=="Manase (Kampala)"] <- "Venue based"
finaldata$topology[finaldata$topology=="VENUE BASED"] <- "Venue based"
finaldata$topology[finaldata$topology=="STREET BASED"] <- "Street based"
finaldata$topology[finaldata$topology=="HOME BASED"] <- "Home based"
finaldata$topology[finaldata$topology=="VENUE AND STREET"] <- "Street based"

# put locations into wards

datRDS <- as.rds.data.frame(finaldata,id="pid",recruiter.id = "recruit_id",network.size = "sexworkerKnow")
datRDS$wave <- get.wave(datRDS)
datRDS$seed <- get.seed.id(datRDS)
datRDS$nrecruit <- get.number.of.recruits(datRDS)

tiff("images/recruitment_tree.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow=c(1,1),mar=c(2,2,2,2))
set.seed(7445)
reingold.tilford.plot(datRDS,
                      vertex.label.cex = 2,
                      vertex.color = "hivstatus",
                      vertex.label =  NA,
                      main = "Recruitment tree by age")
dev.off()

make_reingold_tilford_plot <- function(rds.data,stratify.var,label.var,seed=NULL){
  if(is.null(seed)){
    set.seed(9024)
  }
  reingold.tilford.plot(rds.data,
                             vertex.label.cex = 2,
                             vertex.color = stratify.var,
                             vertex.label = NA,
                        main=paste("Recruitment tree by",label.var,sep = " "))
}

# create and save recruitment trees
tiff("images/recruitment_tree_age.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow=c(1,1),mar=c(2,2,2,2))
make_reingold_tilford_plot(datRDS,stratify.var = "agecat",label.var = "age")
dev.off()

# education
tiff("images/recruitment_tree_educ.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow=c(1,1),mar=c(2,2,2,2))
make_reingold_tilford_plot(datRDS,stratify.var = "educ",label.var = "education")
dev.off()

# location
tiff("images/recruitment_tree_bar.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow=c(1,1),mar=c(2,2,2,2))
make_reingold_tilford_plot(datRDS,stratify.var = "findbar",label.var = "bar location")
dev.off()

# bar location
tiff("images/recruitment_tree_lodge.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow=c(1,1),mar=c(2,2,2,2))
make_reingold_tilford_plot(datRDS,stratify.var = "locxn",label.var = "Location")
dev.off()

# marital status
tiff("images/recruitment_tree_marstatus.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow=c(1,1),mar=c(2,2,2,2))
make_reingold_tilford_plot(datRDS,stratify.var = "marstatus",label.var = "marital status")
dev.off()

# syphilis
tiff("images/recruitment_tree_syphilis.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow=c(1,1),mar=c(2,2,2,2))
make_reingold_tilford_plot(datRDS,stratify.var = "syphstatus",label.var = "syphilis")
dev.off()

# topology
tiff("images/recruitment_tree_topology.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow=c(1,1),mar=c(2,2,2,2))
make_reingold_tilford_plot(datRDS,stratify.var = "topology",label.var = "Topology")
dev.off()

# actual location
tiff("images/recruitment_tree_location.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow=c(1,1),mar=c(2,2,2,2))
make_reingold_tilford_plot(datRDS,stratify.var = "locxn",label.var = "Location")
dev.off()


# diagnostic plots
W <- datRDS %>%
  group_by(seed,hivstatus) %>%
  summarise(n=length(recruit_id))

ggplot(W,aes(x=seed,y=n,fill=hivstatus)) +
  geom_bar(stat = "identity",position = "dodge") +
  theme_bw() +
  scale_fill_manual(values = c("lightblue","lightsalmon")) +
  labs(x="",y="Number of recruits",fill="HIV status",title = "Recruits per seed") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.position = c(0.1,0.93),
        plot.title = element_text(size = 22))
ggsave("images/recruits_seed.tiff",width = 430,height = 450,units = "mm",compression = "lzw")

# summary figure of recruits by wave

newdat <- datRDS %>%
  group_by(wave,hivstatus) %>%
  summarise(nrecruit=length(recruit_id))

newdat_syph <- datRDS %>%
  group_by(wave,syphstatus) %>%
  summarise(nrecruit=length(recruit_id))

ggplot(newdat,aes(x=wave,y=nrecruit,colour=hivstatus)) +
  geom_line(size=1.5) +
  theme_bw()+
  scale_colour_manual(values = c("darkgreen","gold"))+
  scale_x_continuous(breaks = c(seq(0,7,1)))+
  labs(x="Wave",y="Number of recruits",title = "Recruits by wave",colour="HIV status") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 21),
        legend.title = element_text(size = 21),
        legend.position = c(0.1,0.93),
        plot.title = element_text(size = 22))
ggsave("images/recruits_wave1.tiff",width = 300,height = 350,units = "mm",compression = "lzw")

# plot recruits by wave for syphilis

ggplot(newdat_syph,aes(x=wave,y=nrecruit,colour=syphstatus)) +
  geom_line(size=1.5) +
  theme_bw()+
  scale_colour_manual(values = c("darkgreen","gold"))+
  scale_x_continuous(breaks = c(seq(0,7,1)))+
  labs(x="Wave",y="Number of recruits",title = "Recruits by wave",colour="Syphilis status") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.position = c(0.1,0.93),
        plot.title = element_text(size = 22))
ggsave("images/recruits_wave2.tiff",width = 300,height = 350,units = "mm",compression = "lzw")

# network size by wave

d1 <- plot(datRDS,plot.type = "Recruits by wave",stratify.by = "hivstatus")
d2 <- plot(datRDS,plot.type = "Recruits per seed",stratify.by = "hivstatus")
d3 <- plot(datRDS,plot.type = "Recruits per subject")
d4 <- plot(datRDS,plot.type = "Network size by wave",stratify.by = "hivstatus")

cowplot::plot_grid(d1,d2)
ggsave("images/diagnostic_plots.tiff",width = (45*0.39),height = (28*0.39),units = "in",compression="lzw")

#------------------------------------------------ RDS outcome estimates ------------------------------------------
#rds_estim_I <- RDS.I.estimates(datRDS,outcome.variable = "hivstatus")
rds_estim_hiv <- RDS.II.estimates(datRDS,outcome.variable = "hivstatus")

rds_estim_syph <- RDS.II.estimates(datRDS,outcome.variable = "syphstatus")
rds_estim_lox <- RDS.II.estimates(datRDS,outcome.variable = "locxn")
rds_estim_lox <- RDS.II.estimates(datRDS[datRDS$locxn,],outcome.variable = "hivstatus")

RDS.I.estimates(datRDS,outcome.variable = "hivstatus",smoothed = T)

# ------------------------------ compare weights by outcome ------------------------------------------------------
# RDS-II
datRDS$wt_hiv <- rds.I.weights(datRDS,outcome.variable = "hivstatus")
datRDS$wt_syph <- rds.I.weights(datRDS,outcome.variable = "syphstatus")

# compute means
mean(datRDS$wt_hiv[datRDS$hivstatus=="Negative"])
mean(datRDS$wt_hiv[datRDS$hivstatus=="Positive"])

mean(datRDS$wt_syph[datRDS$syphstatus=="Positive"])
mean(datRDS$wt_syph[datRDS$syphstatus=="Negative"])


t.test(wt_hiv ~ hivstatus, data = datRDS)

# ----------------------------------------------- homophily analysis ---------------------------------------------

get_homophily_estimates(rds.data = datRDS,outcome.var = "hivstatus", estim.type = "RDS-II",recruitment = T)
get_homophily_estimates(rds.data = datRDS,outcome.var = "syphstatus", estim.type = "RDS-II",recruitment = T)
get_homophily_estimates(rds.data = datRDS,outcome.var = "marstatus", estim.type = "RDS-II",recruitment = T)
#get_homophily_estimates(rds.data = datRDS,outcome.var = "findbar",estim.type = "RDS-II",recruitment = T)
#get_homophily_estimates(rds.data = datRDS,outcome.var = "lodgefind",estim.type = "RDS-II",recruitment = T)

# convergence plot for hiv
rds_convergence_plot(datRDS,
                 outcome.variable = "hivstatus",
                 est.func = RDS.II.estimates,
                 n.eval.points = 30,
                 plot.title = "HIV",
                 max_waves = 7)
ggsave("images/convergence_hiv.tiff",width = 300,height = 350,compression="lzw",units="mm")

# convergence for syphilis
rds_convergence_plot(datRDS,
                     outcome.variable = "syphstatus",
                     est.func = RDS.II.estimates,
                     n.eval.points = 30,
                     plot.title = "Syphilis",
                     max_waves = 7)
ggsave("images/convergence_syphilis.tiff",width = 300,height = 350,compression="lzw",units="mm")

# ------------------------------------------- bottleneck plots -------------------------------------------------------------

make_bottleneck_plot(datRDS,outcome.variable = "hivstatus",est.func = RDS.II.estimates)
ggsave("images/bottleneck_hiv.tiff",width = 300,height = 350,compression="lzw",units="mm")

make_bottleneck_plot(datRDS,outcome.variable = "syphstatus",est.func = RDS.II.estimates)
ggsave("images/convergence_syphilis.tiff",width = 300,height = 350,compression="lzw",units="mm")


# -------------------------------- tests of independence -----------------------------------------------------------

make_bootstrap_contingency_test(rds_data = datRDS,row_var = "age",col_var = "hivstatus",nsim = 1000)
make_bootstrap_contingency_test(rds_data = datRDS,row_var = "agecat",col_var = "hivstatus",nsim = 1000)

# -------------------------------- estimate incidence -------------------------------------------------------------
bootstrap.incidence(datRDS,
                    recent.variable = "hivresult",
                    hiv.variable = "hivresult",
                    weight.type = "RDS-II",
                    number.of.bootstrap.samples = 100)
# ------------------------------ estimate weighted proportions ---------------------------------------------------------------
var_list <- c("agecat","educ")
prop_RDS_I <- RDS.bootstrap.intervals(datRDS,
                                      outcome.variable = var_list,
                                      weight.type = "RDS-I",
                                      N = 1000)
prop_RDS_II <- RDS.bootstrap.intervals(datRDS,
                                 outcome.variable = var_list,
                                 weight.type = "RDS-II",
                                 N=5000)

## ----------------------------------- Regression models ---------------------------
# extract RDS weights
datRDS$hiv_rdswght <- rds.I.weights(datRDS,outcome.variable = "hivstatus")
datRDS$syph_rdswght <- rds.I.weights(datRDS,outcome.variable = "syphstatus")

#dat$rds_wght <- compute.weights(datRDS,weight.type = "RDS-I")

# fit a simple glm
# process some variables
model_data <- datRDS %>%
  mutate(educ_level = case_when(
    educ == "MSCE" ~ "Secondary",
    educ == "JCE" ~ "Secondary",
    educ == "Tertiary" ~ "Secondary"))

# model without weights
m1 <- glm(hivresult ~ calc_age + syphstatus + locxn + educ,data = datRDS,family = "binomial")
summary(m1)
res <- data.frame(OR=exp(coef(m1)),ci = exp(confint(m1)))

# model with weigths
m2 <- glm(hivresult ~ calc_age + syphstatus + educ + (1|locxn),data = datRDS,weights = hiv_rdswght,family = "binomial")
summary(m2)

# mixed model 

m3 <- glmer(hivresult ~ calc_age + syphstatus + educ + locxn,
            family = "binomial",
            data = model_data)
summary(m3)
##  ---------------------------------- Baseline characteristics -----------------------------------------------------

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

tab1 <- tableby(wave~calc_age+agecat+syphstatus+marstatus+lengthstay+findbar+findonline+mrktfind+trucksfind+
                  lodgefind+educ+knwpersoncoupon+sexworkerKnow +
                  sexworkerSeen + sexworkerRecruit,
                data = datRDS,control = tabcontrols)
U = summary(tab1,labelTranslations = tablabs,text = T)
write2word(U,"baseline_table_characteristics.doc")
write2html(U,"baseline_RDS.htm")


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

# --------------------------------------------------------------------------------------------------------------------------------------
# relationships between mental health, alcohol abuse and violence
mentalHealth <- rds %>%
  group_by(violence01,drink_dy,think_deep) %>%
  summarise(nres=n()) %>%
  mutate(nprop=nres/sum(nres)*100)

ggplot(mentalHealth,aes(x=drink_dy,y=nprop,fill=think_deep))+
  geom_bar(stat = "identity",fill="salmon")+
  theme_bw() +
  xlab("")+
  ylab("Proportion") +
  ylim(0,100)+
  labs(title = "HIV risk in the next 5 years")+
  facet_wrap(~violence01)+
  theme(axis.text.x = element_text(size = 38),
        axis.text.y = element_text(size = 38),
        axis.title.y = element_text(size = 38),
        axis.title.x = element_text(size = 38),
        plot.title = element_text(size = 40))
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

# remaining participants
k <- which(rdsfinal$pid %in% setdiff(rdsfinal$pid,diary_data$pid))
dataRemain <- rdsfinal[k,]
set.seed(45)
dataRemainPos <- dataRemain %>%
  filter(hivstatus=="Positive") 

write.csv(dataRemainPos,"data/hivposData.csv",row.names = F)
dataRemainNeg <- dataRemain %>%
  filter(hivstatus=="Negative")
write.csv(dataRemainNeg,"data/hivnegData.csv",row.names = F)


# to do

# seeds characteristics table