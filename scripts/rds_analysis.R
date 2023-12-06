rm(list = ls())

library(tidyverse)
library(lubridate)
library(RDS)
library(lme4)
library(arsenal)
library(tmap)
library(sf)
library(RColorBrewer)
library(sjPlot)
library(janitor)

      
# load functions
source("scripts/rds_analysis_functions.R")

rds_data <- read_csv("data/amethist_rds_survey_raw.csv")
coupon_data <- read_csv("data/amethist_coupon_raw240522.csv")
hiv_data <- read_csv("data/amethist_results_raw.csv")
results_data <- read_csv("data/AMETHEIST_result_file.csv")
chlamydia_data <- read_csv("data/chlamydia_data.csv")
pts <- read_csv("data/Pakachere _MLW _coords.csv")
locator_data <- read_csv("data/amethist_locator_final.csv")
location <- read_csv("data/Locator_Data_TG.csv")
bardata <- read_csv("data/MP001_250222 Final.csv")
diary_data <- read_csv("data/amethist_acasi_diary_raw.csv")
sti_test_data <- read_csv("data/RDS TEST RESULTS_14th September 2023.csv", na = "N/A") |>
  clean_names() |>
  select(pid_number, hiv_test_result, viral_load_result, syphilis_result, gonorrhea_results, chlamydia_results) |>
  rename(pid = pid_number) |>
  mutate(gonorrhea_results = case_when(
    gonorrhea_results %in% c("Dectected" ,"Decteted", "DETECTED", "POS", "Detected") ~ "Positive",
    gonorrhea_results %in% c("NEG", "Not detected") ~ "Negative",
    gonorrhea_results == "No results" ~ "No results"
  )) |>
  mutate(chlamydia_results = case_when(
    chlamydia_results %in% c("Detected", "POS") ~ "Positive",
    chlamydia_results %in% c("Not detected", "No detected", "NOT DETECTED", "NEG") ~ "Negative",
    chlamydia_results %in% c("No results", "Noresults") ~ "No results"
  )) |>
  mutate(viral_load_result = str_replace(viral_load_result, "ldl", "LDL")) 

# change some readings in the viral load data

sti_test_data$viral_load_result[sti_test_data$viral_load_result == "No results"] <- NA # new positive
sti_test_data$viral_load_result[sti_test_data$viral_load_result == "LDL"] <- 838 # below the detectable 839
sti_test_data$viral_load_result[sti_test_data$viral_load_result == "<839"] <- 838 # below DL

sti_test_data$viral_load_result = as.numeric(sti_test_data$viral_load_result)

sti_test_data_final <- sti_test_data |>
  mutate(VLcat = factor(case_when(
    viral_load_result > 1000 ~ "Unsuppressed",
    viral_load_result >= 839 & viral_load_result <= 1000 ~ "Suppressed",
    viral_load_result < 839 ~ "Undetectable"), levels = c("Unsuppressed", "Suppressed", "Undetectable")))

# spatial data
blantyre <- st_read("data/blantyre_shapefiles/blantyre_district_boundary.shp")
blantyre_city <- st_read("data/blantyre_shapefiles/blantyre_city_boundary.shp")

# process spatial data
spatial_dat_dir <- "C:/Users/jchirombo/Dropbox/GIS_malawi/Enumeration areas 2018"
spatial_dat <- st_read(paste(spatial_dat_dir,"2018_Malawi_Enumeration_Areas.shp",sep = "/"))
spatial_BT_city <- spatial_dat %>%
  filter(DIST_NAME == "Blantyre City") %>%
  st_transform(4326)

#ndirande <- filter(spatial_BT_city, TA_NAME %in% c("Ndirande Matope Ward","Ndirande Makata Ward","Ndirande Gamulani Ward")) %>%
 # st_union()

#mbayani <- filter(spatial_BT_city,TA_NAME == "Mbayani Ward") %>%
#  st_union()

#bangwe <- filter(spatial_BT_city,TA_NAME %in% c("Bangwe Ward","Bangwe Mthandizi Ward","Namiyango Ward")) %>%
#  st_union()

spatial_BT_rural <- spatial_dat |>
  filter(DIST_NAME == "Blantyre") |>
  st_transform(4326) 

# process bar data
bar_location <- bardata %>%
  select(`Catchment area drop-in center`,`Geographical location`,
         `Name of venue`,`Venue type`,`Clustered venue`,PK_AMETHIST,Latitude,Longitude) %>%
  rename(DIC = `Catchment area drop-in center`,location = `Geographical location`,venue = `Name of venue`,
         venue_type = `Venue type`,cluster = `Clustered venue`)

# malawi districts and extract Blantyre rural and urban
mw_districts <- st_read("data/malawi/District.shp") %>%
  st_set_crs(32736) %>%
  st_transform(4326)

#mw_districts <- st_set_crs(mw_districts,32736) %>%
#  st_transform(4326)

# blantyre locations
blantyre_pts <- st_as_sf(pts, coords = c("Longitude","Latitude"), crs = 4326)

# plot locations

#blantyre_pts$Institution <- factor(blantyre_pts$Institution)
blantyre_study_pts <- blantyre_pts %>%
  mutate(Institution = factor(Institution)) %>%
  mutate(Label = case_when(
    Label == "Market" ~ "Market/Street/Other",
    Label == "Street" ~ "Market/Street/Other",
    Label == "Others" ~ "Market/Street/Other",
    Label == "Shabeen with lodging" ~ "Shabeen/Tavern",
    Label == "Shabeen without lodging" ~ "Shabeen/Tavern",
    Label == "Tarven" ~ "Shabeen/Tavern",
    Label == "Bar with lodging" ~ "Bar-lodging",
    Label == "Bar without lodging" ~ "Bar-no lodging",
    Label == "Rest house/lodge" ~ "Resthouse/Lodge"
  ))

#tm_shape(blantyre) +
#  tm_polygons(border.col = "black") +
#tm_shape(blantyre_city) +
#  tm_polygons(border.col = "black") +
#tm_shape(blantyre_pts) +
#  tm_dots("Institution",size = 1,palette = c("red","blue"))

# plot by label
#tm_shape(blantyre) +
#  tm_polygons(border.col = "black",lwd=2) +
#  tm_shape(blantyre_city) +
#  tm_polygons(border.col = "black") +
#  tm_shape(blantyre_pts) +
#  tm_dots("Label",size = 1)

plot_fsw_location <- function(plot.var=c("Institution","Label")){
  plot.var <- match.arg(plot.var)
  if (plot.var == "Institution") {
    tm_shape(blantyre) +
      tm_polygons(border.col = "black",col = "white",lwd = 2) +
      tm_shape(blantyre_city) +
      tm_polygons(border.col = "black",lwd = 2,col = "white") +
      tm_shape(blantyre_study_pts) +
      tm_dots(plot.var,size = 0.8,palette = c("lightsalmon","steelblue")) +
      tm_scale_bar(position = c("right","bottom"),text.size = 1.5,width = 3) +
      tm_layout(frame = T,
                asp = 1,
                frame.lwd = 3,
                legend.show = FALSE,
                legend.text.size = 1.5,
                legend.title.size = 1.8)
  } else{
    nc <- n_distinct(blantyre_study_pts$Label)
    tm_shape(blantyre) +
      tm_polygons(border.col = "black",col = "white") +
      tm_shape(blantyre_city) +
      tm_polygons(border.col = "black",col = "white") +
      tm_shape(blantyre_study_pts) +
      tm_dots(plot.var,size = 0.8,palette = RColorBrewer::brewer.pal(nc,"Paired"), title = "Venue type") +
      tm_scale_bar(position = c("right","bottom"),text.size = 1.5,width = 3) +
      tm_layout(frame = T,
                asp = 1,
                frame.lwd = 3,
                legend.show = FALSE,
                legend.outside = FALSE,
                legend.text.size = 1.5,
                legend.title.size = 1.8)
  }
}

h1 <- plot_fsw_location(plot.var = "Label")
tmap_save(h1,"images/location_types.png", width = 220, height = 220, units = "mm")
h2 <- plot_fsw_location(plot.var = "Institution")
tmap_save(h2,"images/location_institution.png", width = 200, height = 200, units = "mm")

# plot blantyre urban only
# bar type

ncolour <- n_distinct(blantyre_study_pts$Label)
tm_shape(blantyre_city) +
  tm_polygons(border.col = "black",col = "white",lwd = 2) +
tm_shape(blantyre_study_pts) +
  tm_dots("Label",size = 0.8, palette = brewer.pal(ncolour, "Paired")) +
  tm_layout(frame = T,
            asp = 1,
            frame.lwd = 3,
            legend.outside = TRUE,
            legend.text.size = 1.5,
            legend.title.size = 1.5)

# institution

# package blantyre urban plots into a function
plot_fsw_location_blantyre_urban <- function(plot.var = c("Institution", "Label")){
  plot.var <- match.arg(plot.var)
  if (plot.var == "Institution") {
    tm_shape(blantyre_city) +
      tm_polygons(border.col = "black",col = "white",lwd = 2) +
      tm_shape(blantyre_study_pts) +
      tm_dots(plot.var,size = 0.8,palette = c("lightsalmon","steelblue")) +
      tm_scale_bar(position = c("left","bottom"),text.size = 1.5,width = 3) +
      tm_layout(frame = T,
                asp = 1,
                frame.lwd = 3,
                legend.outside = TRUE,
                legend.outside.position = "right",
                legend.text.size = 1.5,
                legend.title.size = 1.5)
  } else{
    nc <- n_distinct(blantyre_study_pts$Label)
    tm_shape(blantyre_city) +
      tm_polygons(border.col = "black",col = "white") +
      tm_shape(blantyre_study_pts) +
      tm_dots(plot.var,size = 0.8, palette = RColorBrewer::brewer.pal(nc,"Paired"), title = "Venue type") +
      tm_scale_bar(position = c("left","bottom"), text.size = 1.5,width = 3) +
      tm_layout(frame = T,
                asp = 1,
                frame.lwd = 3,
                legend.outside = TRUE,
                legend.text.size = 1.5,
                legend.title.size = 1.8)
  }
}


plot_fsw_location_blantyre_urban(plot.var = "Institution")
venue_type <- plot_fsw_location_blantyre_urban(plot.var = "Label")
tmap_save(venue_type, "images/venue_type_blantyre.png", width = 250, height = 250, units = "mm")

# plot interactive maps

#BT_areas <- leaflet() %>%
#  setView(lat = -15.77,lng = 35.04,zoom = 5) %>%
#  addTiles() %>%
#  addPolylines(data = bangwe,color = "#9c2b86",fillOpacity = 0.3) %>%
#  addPolylines(data = ndirande,color = "#9c2b86",fillOpacity = 0.3) %>%
#  addPolylines(data = mbayani,color = "#9c2b86",fillOpacity = 0.3) %>%
#  addMarkers(lat = pts$Latitude,lng = pts$Longitude,label = pts$Institution,labelOptions = labelOptions(noHide = FALSE,textsize = "16px"))

#saveWidget(BT_areas,"data/hotspot_areas.html")


# plot locations
#bt_bbox <- st_bbox(blantyre_city)
#osm_base <- read_osm(bt_bbox)
#tm_shape(blantyre_city) +
#  tm_polygons() +
#  tm_shape(blantyre_pts) +
#  tm_symbols()

# ----------------------- process locator data ----------------------------------------

locator_data <- locator_data %>%
  select(-redcap_event_name,-redcap_repeat_instrument,-redcap_repeat_instance,-pid1)


# some data management
# seeds
k <- which(coupon_data$ctype == 1)
seedID <- coupon_data[k,"pid"]$pid

# isolate and remove some rows
#coupon_data <- coupon_data[-c(8,27),]


# add recruiter id column
coupon_data_final <- coupon_data %>%
  add_column(recruit_id2 = get_recruiter_id(data = coupon_data),.after = "recruit_id") %>%
  select(pin,data_date,pid,recruit_id2) %>%
  rename(recruit_id = recruit_id2) %>%
  mutate(data_date = dmy(data_date))

rds <- rds_data %>%   
  rename(sex_pst_mnth = elig01,knwDOB = elig02,compltsvy = elig03,lengthstay = elig04,findplace = elig05a,
         onlinefind = elig05b,mrktfind = elig05c,lodgefind = elig05d,trucksfind = elig05e,nonefind = elig05f,
         chargefee = elig06,knwpersoncoupon = elig07,knowpersoncoupon2 = elig07i,
         educ = a02,marstatus = a03,liveLastMnth = a04,liveLastMnth2 = a04i,hhsize = a05,
         childsupp = a06,regincome = a07,toilet = a08,electricity = a09a,fridge = a09b,stove = a09c,
         tapwater = a09d,livestock = a09e,bicycle = a09f,motorcycle = a09g,car = a09h,cart = a09i,wheelbarrow = a09j,
         phone = a09k,radio = a09l,tv = a09m,accnt = a10,phoneuse = a11,sexworkerKnow = o01,
         sexworkerSeen = o02,sexworkerRecruit = o03,agewomenrecruit1 = o04a,agewomenrecruit2 = o04b,
         sexWoker = a24,agesexwork = a25,sexbreak = a26,sexbreakyr = a27,income = a28,
         hivtestplace = c01,hivtested = c02,whenhivtest = c03,wherehivTested = c04,
         hivres = c05,whenpostest = c06,hivrisk = c07,dailyprotect = c08,
         condomprev = b01,hivfoodsharing = b02,hivtablets = b03,tabletsrisk = b04,
         healthpersonHIV = b05,hivARV = b06,viralLoadRisk = b07,pregHIVbaby = b08,
         pepknow = c09,willingpep = c10,pepreason = c11,offerdpep = c12,takenpep = c13,
         prpepknow = c14,willingprpep = c15,prpepreason = c16,offeredprpep = c17,
         takenprpep = c18,prpephowlong = c19,takingprpepnow = c20,whynotprpep = c21,
         prpeppastmonth = c22,prpeppastmonthreason = c23,whereprpep = c24,
         prpepprotect = c26,womenwillingprpep = c27) %>%
  mutate(data_date = dmy(data_date)) %>%
  filter(!is.na(pid)) %>%
  mutate(seedpid = case_when(
    knowpersoncoupon2 == "Ndi seed" ~ "seed",
    knowpersoncoupon2 == "She is a seed" ~ "seed",
    knowpersoncoupon2 == "Seed" ~ "seed")) %>%
  mutate(educ = factor(case_when(
    educ == 0 ~ "Never",
    educ == 1 ~ "Primary",
    educ == 3 ~ "JCE",
    educ == 4 | educ == 5 ~ "MSCE & above"), levels = c("Never","Primary","JCE","MSCE & above"))) %>%
  mutate(sex_pst_mnth = case_when(
    sex_pst_mnth == 0 ~ "No",
    sex_pst_mnth == 1 ~ "Yes")) %>%
  mutate(lengthstay = factor(case_when(
    lengthstay == 0 ~ "< 6 months",
    lengthstay == 1 ~ "1-6 months",
    lengthstay == 2 ~ "> 6 months"),levels = c("< 6 months","1-6 months","> 6 months"))) %>%
  mutate(findbar = factor(case_when(
    findplace == 0 ~ "No",
    findplace == 1 ~ "Yes"),levels = c("Yes","No"))) %>%
  mutate(findonline = factor(case_when(
    onlinefind == 0 ~ "No",
    onlinefind == 1 ~ "Yes"),levels = c("Yes","No"))) %>%
  mutate(mrktfind = factor(case_when(
    mrktfind == 0 ~ "No",
    mrktfind == 1 ~ "Yes"),levels = c("Yes","No"))) %>%
  mutate(lodgefind = factor(case_when(
    lodgefind == 0 ~ "No",
    lodgefind == 1 ~ "Yes"),levels = c("Yes","No"))) %>%
  mutate(trucksfind = factor(case_when(
    trucksfind == 0 ~ "No",
    trucksfind == 1 ~ "Yes"
  ),levels = c("Yes","No"))) %>%
  mutate(nonefind = factor(case_when(
    nonefind == 0 ~ "No",
    nonefind == 1 ~ "Yes"
  ),levels = c("Yes","No"))) %>%
  mutate(chargefee == factor(case_when(
    chargefee == 0 ~ "< 1000",
    chargefee == 1 ~ "1000-5000",
    chargefee == 2 ~ "5000-10,000",
    chargefee == 3 ~ "10,000-20,000",
    chargefee == 4 ~ "> 20,000",
    chargefee == 5 ~ "Don't know"
  ),levels = c("< 1000","1000-5000","5000-10,000","10,000-20,000","> 20,000","Don't know"))) %>%
  mutate(knowpersoncoupon = factor(case_when(
    knwpersoncoupon == 1 ~ "Close friend",
    knwpersoncoupon == 2 ~ "Friend",
    knwpersoncoupon == 3 ~ "Acquaintance/Colleague",
    knwpersoncoupon == 4 ~ "Stranger",
    knwpersoncoupon == 5 ~ "Relative",
    knwpersoncoupon == 6 ~ "Other"
  ),levels = c("Close friend","Friend","Acquaintance/Colleague","Stranger","Relative","Other"))) %>%
  mutate(marstatus = factor(case_when(
    marstatus == 0 ~ "Never married",
    marstatus == 1 ~ "Widow",
    marstatus == 2 ~ "Divorced/Separated",
    marstatus == 3 ~ "Cohabiting",
    marstatus == 4 ~ "Married but seperated",
    marstatus == 5 ~ "Married living together"
  ),levels = c("Never married","Widow","Divorced/Separated","Cohabiting","Married but seperated","Married living together"))) %>%
  mutate(liveLastMnth = factor(case_when(
    liveLastMnth == 1 ~ "Rented bar room",
    liveLastMnth == 2 ~ "Rented bar room, shared",
    liveLastMnth == 3 ~ "Own house shared",
    liveLastMnth == 4 ~ "Parent's house",
    liveLastMnth == 5 ~ "Hostel room",
    liveLastMnth == 6 ~ "Lodge/Hotel",
    liveLastMnth == 7 ~ "Other"))) %>%
  mutate(agecat = factor(case_when(
    calc_age < 18 ~ "< 18",
    calc_age >=18 & calc_age < 25 ~ "18-24",
    calc_age >=25 & calc_age < 33 ~ "25-32",
    calc_age >=33 & calc_age < 40 ~ "33-39",
    calc_age >=40 & calc_age < 47 ~ "40-46",
    calc_age >=47 & calc_age < 55 ~ "47-54",
    calc_age >= 54 ~ "55+"
  ),levels = c("18-24","25-32","33-39","40-46","47-54","55+"))) %>%
  mutate(sexWoker = factor(case_when(
    sexWoker == 1 ~ "Yes",
    sexWoker == 0 ~ "No",
    sexWoker == 2 ~ "Sometimes"
  ),levels = c("Yes","No","Sometimes"))) %>%
  mutate(sexbreak = factor(case_when(
    sexbreak == 1 ~ "Yes",
    sexbreak == 0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(sexbreakyr = factor(case_when(
    sexbreakyr == 1 ~ "Yes",
    sexbreakyr == 0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(income = factor(case_when(
    income == 1 ~ "Very little",
    income == 2 ~ "Some",
    income == 3 ~ "More than half",
    income == 4 ~ "All"))) %>%
  mutate(hivtestplace = factor(case_when(
    hivtestplace == 1 ~ "Yes",
    hivtestplace == 0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(hivtested = factor(case_when(
    hivtested == 1 ~ "Yes",
    hivtested == 0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(whenhivtest = factor(case_when(
    whenhivtest == 1 ~ "Last 3 months",
    whenhivtest == 2 ~ "Last 6 months",
    whenhivtest == 3 ~ "Last 12 months",
    whenhivtest == 4 ~ "1-2 yrs",
    whenhivtest == 5 ~ "> 2 yrs"
  ),levels = c("Last 3 months","Last 6 months","Last 12 months","1-2 yrs","> 2 yrs"))) %>%
  mutate(hivres = factor(case_when(
    hivres == 0 ~ "HIV -ve",
    hivres == 1 ~ "HIV +ve",
    hivres == 888 ~ "Don't know"
  ),levels = c("HIV +ve","HIV -ve","Don't know"))) %>%
  mutate(whenpostest == factor(case_when(
    whenpostest == 1 ~ "Last 3 months",
    whenpostest == 2 ~ "Last 6 months",
    whenpostest == 3 ~ "Last 12 months",
    whenpostest == 4 ~ "1-2 yrs",
    whenpostest == 5 ~ "> 2 yrs"
  ),levels = c("Last 3 months","Last 6 months","Last 12 months","1-2 yrs","> 2 yrs"))) %>%
  mutate(hivrisk = factor(case_when(
    hivrisk == 0 ~ "None",
    hivrisk == 1 ~ "Low",
    hivrisk == 2 ~ "Medium",
    hivrisk == 3 ~ "High",
    hivrisk == 888 ~ "Don't know" 
  ),levels = c("None","Low","Medium","High","Don't know"))) %>%
  mutate(dailyprotect == factor(case_when(
    dailyprotect==1 ~ "Yes",
    dailyprotect==0 ~ "No",
    dailyprotect==888 ~ "Don't know"
  ),levels = c("Yes","No","Don't know"))) %>%
  mutate(wherehivTested = factor(case_when(
    wherehivTested == 1 ~ "Govt Hosp",
    wherehivTested == 2 ~ "Pakachere DIC",
    wherehivTested == 3 ~ "ANC",
    wherehivTested == 4 ~ "Private doctor",
    wherehivTested == 5 ~ "Mission hosp",
    wherehivTested == 6 ~ "Mobile testing centres",
    wherehivTested == 7 ~ "BLM",
    wherehivTested == 8 ~ "MACRO",
    wherehivTested == 9 ~ "Other"
  ),levels = c("ANC","Govt Hosp","Pakachere DIC","Mission hosp","BLM","MACRO","Private doctor","Mobile testing centres","Other"))) %>%
  mutate(condomprev = factor(case_when(
    condomprev == 1 ~ "Yes",
    condomprev == 0 ~ "No",
    condomprev == 888 ~ "I don't know"
  ),levels = c("Yes","No","I don't now"))) %>%
  mutate(hivfoodsharing = factor(case_when(
    hivfoodsharing == 1 ~ "Yes",
    hivfoodsharing == 0 ~ "No",
    hivfoodsharing == 888 ~ "I don't know"
  ),levels = c("Yes","No","I don't know"))) %>%
  mutate(tabletsrisk = factor(case_when(
    tabletsrisk == 1 ~ "Less than most of the time",
    tabletsrisk == 0 ~ "Most of the time",
    tabletsrisk == 2 ~ "Never"
  ),levels = c("Never","Most of the time","Less than most of the time"))) %>%
  mutate(hivtablets = factor(case_when(
    hivtablets == 1 ~ "Yes",
    hivtablets == 0 ~ "No",
    hivtablets == 888 ~ "I don't know"))) %>%
  mutate(healthpersonHIV = factor(case_when(
    healthpersonHIV == 1 ~ "Yes",
    healthpersonHIV == 0 ~ "No",
    healthpersonHIV == 888 ~ "I don't know"
  ),levels = c("Yes","No","I don't know"))) %>%
  mutate(hivARV = factor(case_when(
    hivARV == 1 ~ "Yes",
    hivARV == 0 ~ "No",
    hivARV == 888 ~ "I don't know"
  ),levels = c("Yes","No","I don't know"))) %>%
  mutate(viralLoadRisk = factor(case_when(
    viralLoadRisk == 0 ~ "High",
    viralLoadRisk == 1 ~ "Medium",
    viralLoadRisk == 2 ~ "Low",
    viralLoadRisk == 3 ~ "Very Low",
    viralLoadRisk == 4 ~ "Zero"
  ),levels = c("Zero","Very Low","Low","Medium","High"))) %>%
  mutate(pregHIVbaby = factor(case_when(
    pregHIVbaby == 1 ~ "Yes",
    pregHIVbaby == 0 ~ "No",
    pregHIVbaby == 888 ~ "I don't know"
  ),levels = c('Yes',"No","I don't know"))) %>%
  mutate(pepknow = factor(case_when(
    pepknow == 1 ~ "Yes",
    pepknow == 0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(willingpep = factor(case_when(
    willingpep == 1 ~ "Yes",
    willingpep == 0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(pepreason = factor(case_when(
    pepreason == 0 ~ "Not sure about access",
    pepreason == 1 ~ "Don't understand",
    pepreason == 2 ~ "Don't know side effects",
    pepreason == 3 ~ "Don't like pills everyday",
    pepreason == 4 ~ "People will think I am HIV+",
    pepreason == 5 ~ "Use condoms",
    pepreason == 6 ~ "Other",
    pepreason == 888 ~ "Don't know"
  ),levels = c("Not sure about access","Don't understand","Don't know side effects","Don't like pills everyday","People will think I am HIV+","Use condoms","Other","Don't know"))) %>%
  mutate(offerdpep = factor(case_when(
    offerdpep == 1 ~ "Yes",
    offerdpep == 0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(takenpep = factor(case_when(
    takenpep == 1 ~ "Yes",
    takenpep == 0 ~ "No"
  ),levels = c("Yes","No"))) %>%
  mutate(seedInfo = ifelse(pid %in% seedID,"seed","recruitee")) %>%
  mutate(think_deep = factor(ifelse(f01 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(violence01 = factor(ifelse(m01 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(drink_freq = factor(case_when(
    n01 == 0 ~ "Never",
    n01 == 1 ~ "Once a month or less",
    n01 == 2 ~ "2-4 times/month",
    n01 == 3 | n01 == 4 ~ ">=2 times/week"
    #n01==4 ~ ">=2 times/week"  # combine groups
  ),levels = c("Never","Once a month or less","2-4 times/month",">=2 times/week"))) %>%
  mutate(drink_dy = factor(case_when(
    n02 == 0 ~ "1 or 2",
    n02 == 1 ~ "3 or 4",
    n02 == 2 ~ "5 or 6",
    n02 == 3 ~ "7,8 or 9",
    n02 == 4 ~ ">=10"),levels = c("1 or 2","3 or 4","5 or 6","7,8 or 9",">=10"))) %>%
  mutate(drink_occ = factor(case_when(
    n03 == 0 ~ "Never",
    n03 == 1 ~ "Less than montly",
    n03 == 2 ~ "Monthly",
    n03 == 3 ~ "Weekly",
    n03 == 4 ~ "Daily or almost daily",
    n03 == 5 ~ "Other"),levels = c("Never","Less than montly","Monthly","Weekly","Daily or almost daily","Other"))) %>%
  mutate(agegrp = factor(ifelse(calc_age <= 25,1,2))) %>%
  mutate(num_sex = i01) %>%
  mutate(condomuse = factor(ifelse(i02 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(condm_2wk = factor(case_when(
    i03 == 0 ~ "No",
    i03 == 1 ~ "Yes",
    i03 == 888 ~ "Don't remember"
  ),levels = c("Yes","No","Don't remember"))) %>%
  mutate(num_anal_sex = i04) %>%
  mutate(condm_anal = factor(ifelse(i05 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(condm_anal_2wk = factor(ifelse(i06 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(stdy_prtner = factor(case_when(
    i07==1 ~ "Yes",
    i07==0 ~ "No",
    i07==2 ~ 'Never'
  ),levels = c("Yes","No","Never"))) %>%
  mutate(stdy_prtner_len = i08) %>%
  mutate(stdy_prtner_freq = i09) %>%
  mutate(stdy_prtner_condm = factor(case_when(
    i10==1 ~ "Always",
    i10==2 ~ "Mostly",
    i10==3 ~ "Sometimes",
    i10==4 ~ "Rarely",
    i10==0 ~ "Never"
  ),levels = c("Never","Rarely","Sometimes","Mostly","Always"))) %>%
  mutate(stdy_prtner_condm_last = factor(ifelse(i11 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(stdy_prtner_3m = factor(ifelse(i12 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(stdy_prtner_hiv = factor(case_when(
    i13==0 ~ "HIV -ve",
    i13==1 ~ "HIV +ve",
    i13==888 ~ "Don't know"
  ),levels = c("HIV +ve","HIV -ve","Don't know"))) %>%
  mutate(stdy_prtner_trt = factor(case_when(
    i14 == 1 ~ "Yes",
    i14 == 0 ~ "No",
    i14 == 888 ~ "Don't know"
  ),levels = c("Yes","No","Don't know"))) %>%
  mutate(stdy_prtner_hiv_condm = factor(ifelse(i15 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(stdy_prtner_num = i16) %>%
  mutate(stdy_prtner_num_condm = i17) %>%
  mutate(num_clients = i18) %>%
  mutate(num_rate = factor(case_when(
    i19 == 0 ~ "More",
    i19 == 1 ~ "Average",
    i19 == 2 ~ "Less",
    i19 == 888 ~ "Don't know"
  ),levels = c("Less","Average","More","Don't know"))) %>%
  mutate(clients_sex_new = i20) %>%
  mutate(client_rpt = factor(ifelse(i21 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(client_rpt_freq = factor(case_when(
    i22 == 1 ~ ">1/week",
    i22 == 2 ~ "Once/week",
    i22 == 3 ~ "2-3/month",
    i22 == 4 ~ "Once/month",
    i22 == 5 ~ "<1/month",
    i22 == 6 ~ "1-2 times only"),levels = c("1-2 times only","<1/month","Once/month","2-3/month","Once/week",">1/week"))) %>%
  mutate(condm_new_client = factor(ifelse(i24 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(condm_rpt_client = factor(ifelse(i25 == 1,"Yes","No"),levels = c("Yes","No"))) %>%
  mutate(condom_freq_clients = factor(case_when(
    i26 == 1 ~ "Always",
    i26 == 2 ~ "Mostly",
    i26 == 3 ~ "Sometimes",
    i26 == 4 ~ "Rarely",
    i26 == 0 ~ "Never"),levels = c("Never","Rarely","Sometimes","Mostly","Always")))

# process hiv result data
hivdata <- hiv_data %>%
  select(data_date, pid, syphresult, determine, unigold, hivresult) %>%
  mutate(data_date = as.Date(data_date,format = "%d%b%Y")) %>%
  mutate(hivstatus = factor(ifelse(hivresult == 1,"Positive","Negative"), levels = c("Positive","Negative"))) %>%
  mutate(syphstatus = factor(ifelse(syphresult == 1,"Positive","Negative"), levels = c("Positive","Negative"))) 
  
# process chlamydia data
chlamydia <- chlamydia_data %>%
  select(PATIENT_CODE,PATIENT_BIRTH,RESULT_TEST_NAME,RESULT) %>%
  rename(pid = PATIENT_CODE,DOB = PATIENT_BIRTH,test_name = RESULT_TEST_NAME,result = RESULT) %>%
  mutate(DOB = as.Date(DOB,format = "%d/%m/%Y")) %>%
  mutate(result = factor(result,levels = c("Detected","Not Detected"))) %>%
  mutate(pid = gsub("-","",pid))

# gonorrhea
gonorrhea_test_data <- chlamydia %>%
  filter(test_name == "Neisseria gonorrhoeae (NG)") %>%
  rename(gono_test = test_name) %>%
  rename(gono_result = result)

chlamydia_test_data <- chlamydia %>%
  filter(test_name == "Chlamydia trachomatis (CT)") %>%
  rename(chla_test = test_name) %>%
  rename(chlam_result = result)
  #mutate(result2==ifelse(result=="Detected","Positive","Negative"),levels = c("Negative","Positive"))

# RDS098 and RDS04111 not available in the main data

#U <- left_join(hivdata,rds,by="pid") %>% 
#  left_join(.,coupon_data_final,by="pid") %>%
#  left_join(.,chlamydia_test,by="pid") %>% 
#  left_join(.,gonorrhea_test,by="pid") %>%
#  select(-data_date.x,-deviceid,-odk_id,-odk_subdate,-data_date.y,-pin.y,-pin.x,-project_dsid)


# merge rds and hiv results data
#rdsfinal <- left_join(hivdata,rds,by="pid")
rdsfinal <- left_join(hivdata,rds, by = "pid") %>% 
  left_join(., coupon_data_final, by = "pid") %>%
  left_join(., location,by = "pid") %>%
  left_join(., chlamydia_test_data, by = "pid") %>%
  left_join(., gonorrhea_test_data, by = "pid") %>%
  left_join(., sti_test_data_final, by = "pid") %>%
  select(-data_date.x, -deviceid, -odk_id, -odk_subdate, -data_date.y, -pin.y, -pin.x, -project_dsid)

# RDS 1054 is duplicated - remove either row 101 or 102
duplicate_pid <- which(rdsfinal$pid == "RDS1054")

rdsfinal <- rdsfinal[-duplicate_pid[1],]


# ------------------------------------ regroup some locations to get bigger numbers ---------------------------------

## -----------------------------------RDS diagnostics----------------------------------------------------------------
# stratify the ages by two age groups
# cutoff point of 25 years
rdsfinal <- rdsfinal %>%
  mutate(age = factor(ifelse(calc_age <= 25, "Young", "Adult"), levels = c("Young", "Adult")))

# make chains for RDS0536 and RDS0544 for now until issue resolved
# assign random recruiters
j <- c(55,60,62,63)
finaldata <- rdsfinal
#finaldata[j,"recruit_id"] <- c("RDS0387","RDS0494","RDS0270","RDS0304")
finaldata[j,"recruit_id"] <- c("RDS0494","RDS0494","RDS0502","RDS0502")

# -------------------------------------------------------------------------------------------------------------------
# combine some locations to obtain bigger numbers
#finaldata$locxn[finaldata$locxn=="BCA"] <- "Bangwe"
finaldata$locxn[finaldata$locxn == "Bvumbwe"] <- "Chigumula"
finaldata$locxn[finaldata$locxn == "Chazunda"] <- "Chadzunda"
finaldata$locxn[finaldata$locxn == "Chemusa"] <- "Mbayani"
finaldata$locxn[finaldata$locxn == "Nancholi"] <- "Manase"
#finaldata$locxn[finaldata$locxn=="Chichiri"] <- "Blantyre"
#finaldata$locxn[finaldata$locxn=="Chirimba"] <- "Mbayani"
#finaldata$locxn[finaldata$locxn=="Mpingwe"] <- "Machinjiri"
finaldata$locxn[finaldata$locxn == "Chiwembe"] <- "Soche (Manje)" # consider combining soche, chilobwe and zingwangwa
finaldata$locxn[finaldata$locxn == "Mpemba"] <- "Manase"
finaldata$locxn[finaldata$locxn == "Chadzunda"] <- "Zingwangwa"
finaldata$locxn[finaldata$locxn == "Chilobwe"] <- "Zingwangwa"
finaldata$locxn[finaldata$locxn == "Soche (Manje)"] <- "Zingwangwa"
finaldata$locxn[finaldata$locxn == "Chigumula"] <- "Bangwe"
finaldata$locxn[finaldata$locxn == "Chileka"] <- "Kameza"
finaldata$locxn[finaldata$locxn == "Chilomoni"] <- "Blantyre"


# combine locations with small numbers 

finaldata$locxn[finaldata$locxn == "Mdeka"] <- "Lunzu"
finaldata$locxn[finaldata$locxn == "Mpingwe"] <- "Kachere"
finaldata$locxn[finaldata$locxn == "Pa Ken"] <- "Kameza"
finaldata$locxn[finaldata$locxn == "Chirimba"] <- "Mbayani"

# combine topology

finaldata$topology[finaldata$topology == "Social Media"] <- "Home"
finaldata$topology[finaldata$topology == "Venue,Based"] <- "Venue"
finaldata$topology[finaldata$topology == "VENUE BASED"] <- "Venue"
finaldata$topology[finaldata$topology == "STREET BASED"] <- "Street"
finaldata$topology[finaldata$topology == "HOME BASED"] <- "Home"
finaldata$topology[finaldata$topology == "VENUE AND STREET"] <- "Street"

# create zones 
finaldata <- finaldata %>%
  mutate(zone = factor(case_when(
    locxn %in% c("Lunzu", "Kameza") ~ "Zone A",
    locxn %in% c("Mbayani", "Blantyre", "Manase", "Zingwangwa", "Naperi") ~ "Zone B",
    locxn %in% c("Machinjiri", "Ndirande") ~ "Zone C",
    locxn %in% c("Bangwe", "Kachere", "Manje") ~ "Zone D"
  ), levels = c("Zone A", "Zone B", "Zone C", "Zone D")))

# put locations into wards

datRDS <- as.rds.data.frame(finaldata, id = "pid", recruiter.id = "recruit_id", network.size = "sexworkerKnow")
datRDS$wave <- get.wave(datRDS)
datRDS$seed <- get.seed.id(datRDS)
datRDS$nrecruit <- get.number.of.recruits(datRDS)

# --------------------------------------------------------------------------------------------------------------
# generate recruitment trees 

#tiff("images/recruitment_tree.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
#par(mfrow=c(1,1),mar=c(2,2,2,2))
#set.seed(7445)
#reingold.tilford.plot(datRDS,
#                      vertex.label.cex = 2,
#                      vertex.color = "hivstatus",
#                      vertex.label =  NA,
#                      main = "Recruitment tree by age")
#dev.off()
# recruitment by hiv status
tiff("images/recruitment_tree_hiv.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(cex = 1.5, cex.lab = 1.5)
generate_recruitment_tree(datRDS,stratify.var = "hivstatus",label.var = "HIV")
dev.off()

# create and save recruitment trees
tiff("images/recruitment_tree_age.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow = c(1,1), mar = c(2,2,2,2))
generate_recruitment_tree(datRDS,stratify.var = "agecat",label.var = "age")
dev.off()

# education
tiff("images/recruitment_tree_educ.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow = c(1,1),mar = c(2,2,2,2))
generate_recruitment_tree(datRDS, stratify.var = "educ",label.var = "education")
dev.off()

# location
tiff("images/recruitment_tree_bar.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow = c(1,1),mar = c(2,2,2,2))
generate_recruitment_tree(datRDS,stratify.var = "findbar",label.var = "bar location")
dev.off()

# bar location
tiff("images/recruitment_tree_lodge.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow = c(1,1),mar = c(2,2,2,2))
generate_recruitment_tree(datRDS,stratify.var = "locxn",label.var = "Location")
dev.off()

# marital status
tiff("images/recruitment_tree_marstatus.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow = c(1,1),mar = c(2,2,2,2))
generate_recruitment_tree(datRDS,stratify.var = "marstatus",label.var = "marital status")
dev.off()

# syphilis
tiff("images/recruitment_tree_syphilis.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow = c(1,1),mar = c(2,2,2,2))
generate_recruitment_tree(datRDS,stratify.var = "syphstatus",label.var = "syphilis")
dev.off()

# gonorrhea
tiff("images/recruitment_tree_gonorrhea.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow = c(1,1),mar = c(2,2,2,2))
generate_recruitment_tree(datRDS,stratify.var = "gono_result",label.var = "gonorrhea")
dev.off()

tiff("images/recruitment_tree_chlamydia.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow = c(1,1),mar = c(2,2,2,2))
generate_recruitment_tree(datRDS, stratify.var = "chlam_result",label.var = "chlamydia")
dev.off()

# by zone
tiff("images/recruitment_tree_ZONE.tif", width = 35*0.39, height = 30*0.39, units = "in", compression = "lzw", res = 500)
par(mfrow = c(1,1), mar = c(2,2,2,2))
generate_recruitment_tree(datRDS, stratify.var = "zone", label.var = "Zone")
dev.off()

# topology
tiff("images/recruitment_tree_topology.tif",width = 35*0.39,height = 30*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow = c(1,1),mar = c(2,2,2,2),cex = 1.7, cex.lab = 1.7)
generate_recruitment_tree_plot(datRDS,stratify.var = "topology",label.var = "typology")
dev.off()

# actual location
tiff("images/recruitment_tree_location2.tif",width = 40*0.39,height = 43*0.39,units = "in",compression = "lzw",res = 500)
par(mfrow = c(1,1),mar = c(2,2,2,2),cex.lab = 1.4)
generate_recruitment_tree_plot(datRDS,stratify.var = "locxn", label.var = "location",legend.title = "Location")
dev.off()

# by viral load


# combine age and location

tiff("images/recruitment_tree_hiv_educ.tif", width = 40*0.39, height = 25*0.39, units = "in", compression = "lzw", res = 500)
par(mfrow = c(1,2), cex.lab = 1.5)
generate_recruitment_tree_plot(datRDS, stratify.var = "hivstatus", label.var = "HIV")
generate_recruitment_tree_plot(datRDS,stratify.var = "educ",label.var = "education")
dev.off()

# recruitment by zone
generate_recruitment_tree_plot(datRDS, stratify.var = "zone", label.var = "ZONE")


# diagnostic plots
recruitment_data <- datRDS %>%
  group_by(seed,hivstatus) %>%
  summarise(n = length(recruit_id))

p1 <- ggplot(recruitment_data,aes(x = seed,y = n,fill = hivstatus)) +
  geom_bar(stat = "identity",position = "stack") +
  theme_bw() +
  scale_fill_manual(values = c("lightsalmon","steelblue")) +
  labs(x = "",y = "Number of recruits",fill = "HIV",title = "") +
  theme(axis.text.x = element_text(size = 20, angle = 30, hjust = 0.9),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.position = c(0.2,0.93),
        plot.title = element_text(size = 20))
ggsave("images/recruits_by_seed.png",width = 300,height = 300,units = "mm")

# summary figure of recruits by wave

recruit_wave_hiv <- datRDS %>%
  group_by(wave, hivstatus) %>%
  summarise(nrecruit = length(recruit_id))

recruit_wave_syphilis <- datRDS %>%
  group_by(wave, syphstatus) %>%
  summarise(nrecruit = length(recruit_id))

ggplot(recruit_wave_hiv,aes(x = wave, y = nrecruit, colour = hivstatus)) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_colour_manual(values = c("lightsalmon", "steelblue")) +
  scale_x_continuous(breaks = c(seq(0, 7, 1))) +
  labs(x = "Wave", y = "Number of recruits", title = "", colour = "HIV") +
  theme(axis.text = element_text(size = 22),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.position = c(0.2, 0.93),
        legend.background = element_rect(colour = "black"),
        plot.title = element_text(size = 22))
ggsave("images/recruits_wave_hiv.tiff", width = 300, height = 350, units = "mm", compression = "lzw")

# plot recruits by wave for syphilis

ggplot(recruit_wave_syphilis, aes(x = wave, y = nrecruit, colour = syphstatus)) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  #xlim(0,7) +
  scale_colour_manual(values = c("lightsalmon","steelblue")) +
  scale_x_continuous(breaks = c(seq(0,7,1))) +
  labs(x = "Wave",y = "Number of recruits",title = "Recruits by wave",colour = "Syphilis status") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.position = c(0.1,0.93),
        legend.background = element_rect(colour = "black"),
        plot.title = element_text(size = 22))
ggsave("images/recruits_wave_syphilis.tiff",width = 300,height = 350,units = "mm",compression = "lzw")

plot_recruitment_wave <- function(summary.data, legend.title, outcome.var = c("HIV","Syphilis"), ...) {
  outcome.var <- match.arg(outcome.var)
  if (outcome.var == "HIV") {
    p <- ggplot(summary.data,aes(x = wave, y = nrecruit, colour = hivstatus)) +
      geom_line(linewidth = 1.5) +
      theme_bw() +
      scale_colour_manual(values = c("lightsalmon","steelblue")) +
      scale_x_continuous(breaks = c(seq(0,7,1))) +
      labs(x = "Wave",y = "Number of recruits",title = NULL,colour = legend.title) +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.position = c(0.2,0.93),
            plot.title = element_text(size = 22),
            plot.caption = element_text(size = 18))
    return(p)
  } else {
    p <- ggplot(summary.data, aes(x = wave, y = nrecruit, colour = syphstatus)) +
      geom_line(linewidth = 1.5) +
      theme_bw() +
      scale_colour_manual(values = c("lightsalmon","steelblue")) +
      scale_x_continuous(breaks = c(seq(0,7,1))) +
      labs(x = "Wave", y = "Number of recruits", title = NULL, colour = legend.title) +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.position = c(0.2,0.93),
            plot.title = element_text(size = 22),
            plot.caption = element_text(size = 18),
            ...)
  }
  return(p)
}

p2 <- plot_recruitment_wave(summary.data = recruit_wave_hiv,legend.title = "HIV",outcome.var = "HIV")
plot_recruitment_wave(summary.data = recruit_wave_syphilis,legend.title = "Syphilis",outcome.var = "Syphilis")

ggsave(p2, "images/recruitment_by_wave_hivstatus", width = 300, height = 300, units = "mm")
# combine recruitment graphs - only for HIV
#gridExtra::grid.arrange(p1,p2, ncol = 2)
cowplot::plot_grid(p1,p2,ncol = 2, labels = c("A","B"),label_size = 17)
ggsave("images/recruitment_by_seed_wave.png", width = 300, height = 250, units = "mm")

# network size by wave

d1 <- plot(datRDS,plot.type = "Recruits by wave", stratify.by = "hivstatus")
d2 <- plot(datRDS,plot.type = "Recruits per seed", stratify.by = "hivstatus")
d3 <- plot(datRDS,plot.type = "Recruits per subject")
d4 <- plot(datRDS,plot.type = "Network size by wave", stratify.by = "hivstatus")

cowplot::plot_grid(d1,d2)
ggsave("images/diagnostic_plots.tiff",width = (45*0.39),height = (28*0.39),units = "in",compression = "lzw")

#------------------------------------------------ RDS outcome estimates ------------------------------------------
#rds_estim_I <- RDS.I.estimates(datRDS,outcome.variable = "hivstatus")
rds_estim_hiv <- RDS.II.estimates(datRDS,outcome.variable = "hivstatus")

rds_estim_syph <- RDS.II.estimates(datRDS,outcome.variable = "syphstatus")
rds_estim_gono <- RDS.II.estimates(datRDS,outcome.variable = "gonorrhea_results")
rds_estim_chlam <- RDS.II.estimates(datRDS,outcome.variable = "chlamydia_results")
rds_estim_sti <- RDS.II.estimates(datRDS, outcome.variable = "sti_any")

rds_estim_lox <- RDS.II.estimates(datRDS,outcome.variable = "locxn")
rds_estim_zone <- RDS.II.estimates(datRDS,outcome.variable = "zone")

# prevalence by zone
zoneA_prev <- RDS.II.estimates(datRDS[datRDS$zone == "Zone A",], outcome.variable = "hivstatus")
zoneB_prev <- RDS.II.estimates(datRDS[datRDS$zone == "Zone B",], outcome.variable = "hivstatus")
zoneC_prev <- RDS.II.estimates(datRDS[datRDS$zone == "Zone C",], outcome.variable = "hivstatus")
zoneD_prev <- RDS.II.estimates(datRDS[datRDS$zone == "Zone D",], outcome.variable = "hivstatus")

#rds_estim_lox <- RDS.II.estimates(datRDS[datRDS$locxn,],outcome.variable = "hivstatus")

RDS.I.estimates(datRDS,outcome.variable = "hivstatus", smoothed = T)

# estimates by location
venue <- c("Bangwe","Blantyre","Chadzunda","Kachere","Kameza","Lunzu","Machinjiri","Manase","Manje","Naperi","Ndirande","Zingwangwa")
venue_data <- list()
for (i in 1:length(venue)) {
  venue_data[[i]] <- datRDS[datRDS$locxn == venue[i],]
}


# list all prevalence
rds_estim_list <- list()
for (i in 1:length(venue)) {
  rds_estim_list[[i]] <- RDS.II.estimates(venue_data[[i]],outcome.variable = "hivstatus")
}

# estimates by typology

rds_estim_venue <- RDS.II.estimates(datRDS[datRDS$topology == "Venue",],outcome.variable = "hivstatus")
rds_estim_home <- RDS.II.estimates(datRDS[datRDS$topology == "Home",],outcome.variable = "hivstatus")
rds_estim_str <- RDS.II.estimates(datRDS[datRDS$topology == "Street",],outcome.variable = "hivstatus")


# map prevalence by point location

location_prev_data <- data.frame(location = c("Bangwe","Blantyre","Chadzunda","Kachere","Kameza","Lunzu","Machinjiri","Manase","Manje","Naperi","Ndirande","Zingwangwa"),
           long = c(35.096,35.004,34.953,35.083,35.011,35.021,35.059,34.978,35.012,35.014,35.040,35.013),
           lat = c(-15.815,-15.788,-15.912,-15.793,-15.786,-15.653,-15.725,-15.812,-15.851,-15.809,-15.778,-15.818),
           prev = c(0.84,0.86,0.78,0.78,0.39,0.68,0.67,0.77,0.54,0.60,0.72,0.67))

location_prev_data <- location_prev_data %>%
  mutate(prevcat = factor(case_when(
    prev <= .50 ~ "<50",
    prev > .50 & prev <= .60 ~ "50-60",
    prev > .60 & prev <= .70 ~ "60-70",
    prev >= .70 ~ "70+"
  )))

location_prev_data_sf <- st_as_sf(location_prev_data,coords = c("long","lat"),crs = 4326)

ncolour <- n_distinct(location_prev_data_sf$prevcat)
location_prev <- tm_shape(blantyre) +
  tm_polygons(border.col = "black",col = "white",lwd = 2) +
  tm_shape(blantyre_city) +
  tm_polygons(border.col = "black",col = "white",lwd = 2) +
  tm_shape(location_prev_data_sf) +
  tm_dots("prevcat",size = 0.8,shape = 19, palette = brewer.pal(9,"YlOrRd")[3:6],title = "Prevalence") + 
  tm_scale_bar(position = c("right","bottom"),text.size = 1.5,width = 3) +
  tm_layout(frame = T,
            asp = 0,
            frame.lwd = 3,
            legend.text.size = 1.5,
            legend.title.size = 1.7)
tmap_save(location_prev,"images/hotspot_prevalence.png", height = 300, width = 300, units = "mm")

# create boundaries based on proximity
# and join locations based on the zones created above
bangwe <- spatial_BT_city %>% 
  filter(TA_NAME == "Bangwe Ward" | TA_NAME == "Bangwe Mthandizi Ward" | TA_NAME == "Mzedi Ward") %>%
  mutate(fsw_area = "Bangwe") %>%
  mutate(prevalence = 0.27)

blantyre_cbd <- spatial_BT_city %>%
  filter(TA_NAME == "Blantyre City Centre Ward") %>%
  mutate(fsw_area = "Blantyre CBD") %>%
  mutate(prevalence = 0.43)

kachere <- spatial_BT_city %>%
  filter(TA_NAME == "Mapanga Ward") %>%
  mutate(fsw_area = "Kachere") %>%
  mutate(prevalence = 0.27)

manase <- spatial_BT_city %>%
  filter(TA_NAME == "Blantyre South Ward") %>%
  mutate(fsw_area = "Manase") %>%
  mutate(prevalence = 0.43)

kameza <- spatial_BT_city %>%
  filter(EA_CODE %in% c(31531019,31531008,31531009,31532026,31532025,31531020,31531007)) %>%
  mutate(fsw_area = "Kameza") %>%
  mutate(prevalence = 0.16)

machinjiri <- spatial_BT_city %>%
  filter(TA_NAME == "Nkolokoti Ward") %>%
  mutate(fsw_area = "Machinjiri") %>%
  mutate(prevalence = 0.18)

naperi <- spatial_BT_city %>%
  filter(EA_CODE %in% c(31547003,31547002,31547011,31547004,3155009)) %>%
  mutate(fsw_area = "Naperi") %>%
  mutate(prevalence = 0.43)

zingwangwa <- spatial_BT_city %>%
  filter(EA_CODE %in% c(31550011,31550014,31550010,31550013,31550015,31550012,31550012,31550007)) %>%
  mutate(fsw_area = "Zingwangwa") %>%
  mutate(prevalence = 0.43)

manje <- spatial_BT_city %>%
  filter(EA_CODE %in% c(31553059,31553058,31553061,31553062,31553049,31553056,31553057)) %>%
  mutate(fsw_area = "Manje") %>%
  mutate(prevalence = 0.21)

ndirande <- spatial_BT_city %>%
  filter(TA_NAME %in% c("Ndirande Makata Ward","Ndirande Matope Ward","Ndirande Gamulani Ward")) %>%
  mutate(fsw_area = "Ndirande") %>%
  mutate(prevalence = 0.18)

# add Mbayani

# combine data
all_urban_data <- bind_rows(bangwe,blantyre_cbd,kameza,kachere,manase,naperi,zingwangwa,manje,machinjiri,ndirande)

# map disease prevalence by location
#mapping_data <- spatial_BT_city %>%
#  mutate(prevalence = case_when(
#    TA_NAME == "Bangwe Ward" ~ 0.84,
#    TA_NAME == "Bangwe Mthandizi Ward" ~ 0.84,
#    TA_NAME == "Mzedi Ward" ~ 0.84,
#    TA_NAME == "Blantyre City Centre Ward" ~ 0.86,
#    TA_NAME == "Mapanga Ward" ~ 0.78,
#    TA_NAME == "South Lunzu Ward" ~ 0.39,
#    TA_NAME == "Nkolokoti Ward" ~ 0.67,  # Nkolokoti for Machinjiri
#    TA_NAME == "Blantyre South Ward" ~ 0.77,
#    TA_NAME == "Soche East Ward" ~ 0.60,
#    TA_NAME == "Misesa Ward" ~ 0.54,
#    TA_NAME == "Ndirande Makata Ward" ~ 0.72,
#    TA_NAME == "Ndirande Matope Ward" ~ 0.72,
#    TA_NAME == "Ndirande Gamulani Ward" ~ 0.72,
#    TA_NAME == "Green Corner Ward" ~ 0.67
#  ))

# remove names from other EAs and only retain one name to reduce clutter
all_urban_data$venue_name <- NA
all_urban_data[which(all_urban_data$EA_CODE == 31545003),"venue_name"] <- ""#"Bangwe"
all_urban_data[which(all_urban_data$EA_CODE == 31541006),"venue_name"] <- ""#"Blantyre CBD"
all_urban_data[which(all_urban_data$EA_CODE == 31533035),"venue_name"] <- ""#"Kachere"
all_urban_data[which(all_urban_data$EA_CODE == 31548005),"venue_name"] <- ""#"Manase"
all_urban_data[which(all_urban_data$EA_CODE == 31550013),"venue_name"] <- ""#"Zingwangwa"
all_urban_data[which(all_urban_data$EA_CODE == 31553049),"venue_name"] <- ""#"Manje"
all_urban_data[which(all_urban_data$EA_CODE == 31534011),"venue_name"] <- ""#"Machinjiri"
all_urban_data[which(all_urban_data$EA_CODE == 31536901),"venue_name"] <- ""#"Ndirande"
all_urban_data[which(all_urban_data$EA_CODE == 31531009),"venue_name"] <- ""#"Kameza"
all_urban_data[which(all_urban_data$EA_CODE == 31547002),"venue_name"] <- ""#Naperi"

#venue_col <- colorRampPalette(c("lightblue","white","steelblue"))(10)
cluster_location_prev <- tm_shape(blantyre_city) +
  tm_polygons(border.col = "black",lwd = 2,col = "#ebedef") +
tm_shape(spatial_BT_city) +
  tm_polygons() +
tm_shape(all_urban_data) +
  tm_polygons(border.col = "grey50",col = "prevalence",lwd = 2,palette = 'viridis',title = "Prevalence") + # add breaks
  tm_scale_bar(position = c("left","bottom"),text.size = 1.4,width = 3) +
  tm_compass(type = "4star",size = 3,position = c("right","bottom"),text.size = 1) +
  tm_text("venue_name",col = "red",size = 1.5,fontface = "bold") +
  tm_layout(frame = T,
            asp = 1,
            frame.lwd = 3,
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.text.size = 1.3,
            legend.title.size = 1.5)
tmap_save(cluster_location_prev,"images/cluster_location_prev.png",width = 400,height = 400,units = "mm")
cluster_location_prev  

# location prevalence with labels
cluster_location_prev2 <- tm_shape(blantyre_city) +
  tm_polygons(border.col = "black",lwd = 2,col = "#ebedef") +
  tm_shape(spatial_BT_city) +
  tm_polygons() +
  tm_shape(all_urban_data) +
  tm_polygons(border.col = "grey50",col = "prevalence",lwd = 2,palette = 'viridis',title = "Prevalence") + # add breaks
  tm_scale_bar(position = c("right","top"),text.size = 1.4,width = 3) +
  tm_compass(type = "4star",size = 3,position = c("right","bottom"),text.size = 1) +
  tm_text("venue_name",col = "red",size = 1.5,fontface = "bold") +
  tm_shape(blantyre_study_pts) +
  tm_dots(size = 0.5,col = "Institution",palette = c("blue","magenta")) +
  tm_layout(frame = T,
            asp = 1,
            frame.lwd = 3,
            legend.outside = FALSE,
            legend.outside.position = "right",
            legend.text.size = 1.3,
            legend.title.size = 1.5)
tmap_save(cluster_location_prev2,"images/cluster_location_15octo.png",width = 350,height = 350,units = "mm")


# cluster map without labels
location_prev_instn <- tm_shape(blantyre_city) +
  tm_polygons(border.col = "black",lwd = 2,col = "#ebedef") +
  tm_shape(all_urban_data) +
  tm_polygons(border.col = "grey50",col = "prevalence",lwd = 2,palette = 'viridis',title = "Prevalence") + # add breaks
  tm_shape(blantyre_study_pts) +
  tm_dots(size = 0.5,col = "Institution",palette = c("blue","magenta")) +
  tm_scale_bar(position = c("left","bottom"),text.size = 1.4,width = 3) +
  tm_compass(type = "4star",size = 0.5,position = c("right","bottom"),text.size = 1) +
  #tm_text("venue_name",col = "red",size = 1.1,fontface = "bold") +
  tm_layout(frame = T,
            asp = 1,
            frame.lwd = 3,
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.text.size = 1.3,
            legend.title.size = 1.5)
tmap_save(location_prev_instn,"images/cluster_location_institution.png", width = 400, height = 400, units = "mm")
# plot bar locations

bar_location_sf <- bar_location %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)

tm_shape(blantyre_city) +
  tm_polygons(border.col = "black", lwd = 2, col = "#ebedef") +
  tm_shape(bar_location_sf) +
  tm_dots(size = 0.6, col = "PK_AMETHIST", title = "Institution") +
  tm_layout(frame = TRUE,
            frame.lwd = 3,
            legend.outside = TRUE,
            legend.text.size = 1.4,
            legend.title.size = 1.7)
  
# ----------------------------------------------- homophily analysis ---------------------------------------------

get_homophily_estimates(rds.data = datRDS,outcome.var = "hivstatus", estim.type = "RDS-II",recruitment = T)
get_homophily_estimates(rds.data = datRDS,outcome.var = "syphstatus", estim.type = "RDS-II",recruitment = T)
get_homophily_estimates(rds.data = datRDS,outcome.var = "marstatus", estim.type = "RDS-II",recruitment = T)
get_homophily_estimates(rds.data = datRDS,outcome.var = "num_sex",estim.type = "RDS-II",recruitment = T)
get_homophily_estimates(rds.data = datRDS,outcome.var = "locxn",estim.type = "RDS-II",recruitment = T)
get_homophily_estimates(rds.data = datRDS,outcome.var = "agegrp",estim.type = "RDS-II",recruitment = T)

#get_homophily_estimates(rds.data = datRDS,outcome.var = "lodgefind",estim.type = "RDS-II",recruitment = T)

# convergence plot for hiv
rds_convergence_plot(datRDS,
                 outcome.variable = "hivstatus",
                 est.func = RDS.II.estimates,
                 n.eval.points = 30,
                 plot.title = "HIV",
                 max.waves = 7)
ggsave("images/convergence_hiv.tiff",width = 200,height = 200,compression = "lzw",units = "mm")

# convergence for syphilis
rds_convergence_plot(datRDS,
                     outcome.variable = "syphstatus",
                     est.func = RDS.II.estimates,
                     n.eval.points = 30,
                     plot.title = "Syphilis",
                     max.waves = 7)
ggsave("images/convergence_syphilis.tiff",width = 200,height = 200,compression = "lzw",units = "mm")

# ------------------------------------------- bottleneck plots -------------------------------------------------------------

make_bottleneck_plot(datRDS,outcome.variable = "hivstatus",est.func = RDS.II.estimates)
ggsave("images/bottleneck_hiv.tiff",width = 300,height = 350,compression = "lzw",units = "mm")

make_bottleneck_plot(datRDS,outcome.variable = "syphstatus",est.func = RDS.II.estimates)
ggsave("images/convergence_syphilis.tiff",width = 300,height = 350,compression = "lzw",units = "mm")


# -------------------------------- tests of independence -----------------------------------------------------------

make_bootstrap_contingency_test(rds_data = datRDS,row_var = "age",col_var = "hivstatus",nsim = 1000)
make_bootstrap_contingency_test(rds_data = datRDS,row_var = "agecat",col_var = "hivstatus",nsim = 1000)
make_bootstrap_contingency_test(rds_data = datRDS,row_var = "agecat",col_var = "syphstatus",nsim = 1000)
make_bootstrap_contingency_test(rds_data = datRDS,row_var = "locxn",col_var = "hivstatus",nsim = 1000)
make_bootstrap_contingency_test(rds_data = datRDS,row_var = "locxn",col_var = "syphstatus",nsim = 1000)

# -------------------------------- estimate incidence -------------------------------------------------------------
bootstrap.incidence(datRDS,
                    recent.variable = "hivresult",
                    hiv.variable = "hivresult",
                    weight.type = "RDS-II",
                    number.of.bootstrap.samples = 100)

bootstrap.incidence(datRDS,
                    recent.variable = "syphresult",
                    hiv.variable = "syphresult",
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
                                 N = 5000)

## ----------------------------------- Regression models ---------------------------

# change outcome variable to 0,1
datRDS$hivout <- ifelse(datRDS$hivstatus == "Positive", 1, 0)
datRDS$locxn <- factor(datRDS$locxn)


# define composite terms - alcohol, violence  based on the given responses

# violence
datRDS$gbv <- 0
datRDS$gbv[datRDS$m01 == 1 | datRDS$m05 == 1 | datRDS$m10 == 1] <- 1

#datRDS$gbv[datRDS$m01 == 1 | datRDS$m05 == 1 | datRDS$m10 == 1 |
#             datRDS$m15a == 1 | datRDS$m16a == 1 | datRDS$m17a == 1] <- 1  # remove m15a, m16a and m17a
datRDS$gbv <- factor(ifelse(datRDS$gbv == 1, "Yes", "No"), levels = c("Yes","No"))

# police violence
datRDS$gbv_pol <- ifelse(datRDS$gbv == 1 & (datRDS$m17a == 1 | datRDS$m16a == 1 | datRDS$m15a == 1), 1, 0)
datRDS$stdy_prtner2 <- factor(ifelse(datRDS$stdy_prtner == "Never","No"), levels = c("Yes","No"))

datRDS$stdy_prtner2 <- as.character(datRDS$stdy_prtner)
datRDS$stdy_prtner2[datRDS$stdy_prtner2 == "Never"] <- "No"
datRDS$gbv_comp <- factor(ifelse(datRDS$gbv == 1 | datRDS$gbv_pol == 1, 1, 0))
# binary variable for drinking
# based on the CDC definition for heavy and excessive drinking
# For women, 4 or more drinks during a single occasion - binge drinking, a form of excessive drinking.
# For women, 8 or more drinks per week - heavy drinking
# these categories have been combined here

datRDS$heavy_drinking <- ifelse(datRDS$drink_dy == "5 or 6" | datRDS$drink_dy == "7,8 or 9" | datRDS$drink_dy == ">=10",1,0)

# include location information 
m <- which(datRDS$location1 %in% bar_location$venue)
bar_name <- datRDS[m,]$location1
datRDS$instn <- ifelse(datRDS$location1 %in% bar_name,"Pakachere","MLW")
#bar_name <- bar_location[m,c("venue","PK_AMETHIST")]

# composite term for drinking
# extract RDS weights
datRDS$hiv_rdswght <- rds.I.weights(datRDS,outcome.variable = "hivstatus")
datRDS$syph_rdswght <- rds.I.weights(datRDS,outcome.variable = "syphstatus")

datRDS$rds_wght <- compute.weights(datRDS,weight.type = "RDS-II") # sampling weights

# fit a simple glm
# process some variables
model_data <- datRDS %>%
  mutate(educ_level = educ) %>%
  mutate(educ_level = case_when(
    educ == "MSCE" ~ "Secondary",
    educ == "JCE" ~ "Secondary",
    educ == "Tertiary" ~ "Secondary"))

# model without weights
# recode missing values
# combine drinkin categories
# change reference categories for some variables in the model
datRDS$syphstatus <- relevel(datRDS$syphstatus,ref = "Negative")
datRDS$condomuse <- relevel(datRDS$condomuse,ref = "No") # did you use a condom the last time you had vaginal sex
datRDS$stdy_prtner <- relevel(datRDS$stdy_prtner,ref = "Never")
datRDS$num_sex[datRDS$num_sex == 888] <- NA # how many people have you had vaginal sex with in the past month
datRDS$drink_freq <- relevel(datRDS$drink_freq,ref = "Never")
datRDS$stdy_prtner_hiv <- relevel(datRDS$stdy_prtner_hiv,ref = "HIV -ve")
datRDS$condm_2wk <- relevel(datRDS$condm_2wk,ref = "Don't remember")
datRDS$locxn <- relevel(datRDS$locxn, ref = "Blantyre")
datRDS$VLcat <- relevel(datRDS$VLcat, ref = "Undetectable")

# add ART and prep use to the covariate list
#m1 <- glm(hivresult ~ calc_age + stdy_prtner + num_sex + syphstatus + condomuse + drink_freq,data = datRDS,family = "binomial")

fit1 <- glm(hivout ~ calc_age + stdy_prtner2 + num_sex + heavy_drinking + zone,
          data = datRDS,
          weights = ceiling(1/rds_wght),
          family = "binomial")
summary(fit1)
sjPlot::tab_model(fit1)

# plot using the survey package

#sjPlot::plot_model(m1)
#sjPlot::plot_model(m1,type = "pred",terms = "agecat")
res <- data.frame(OR = exp(coef(m1)),ci = exp(confint(m1)))


# glm model with weigths
# remove condom use?

fit2 <- glm(hivout ~ calc_age + stdy_prtner2 + num_sex + heavy_drinking + zone +,
            data = datRDS,
            family = "binomial")

summary(fit2)
sjPlot::tab_model(fit2)

# mixed model with weights

fit3 <- glmer(hivout ~ calc_age + stdy_prtner2 + num_sex + heavy_drinking + (1|zone),
            family = "binomial",
            weights = ceiling(1/rds_wght),
            data = datRDS)
summary(fit3)
tab_model(fit3)
plot_model(fit3, 
           title = "Fixed effects from the fitted mixed model",
           value.size = 5,
           vline.color = "#f1c616") +
  scale_x_discrete(labels = rev(c("Age","Steady patner","Number sex encounters","Syphilis +ve","Heavy drinker"))) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 18))
ggsave("images/mixed_model_coeff.png", width = 180, height = 180, units = "mm")

# mixed model without RDS waiting
fit4 <- glmer(hivout ~ calc_age + stdy_prtner2 + num_sex + heavy_drinking + (1|zone),
              family = "binomial",
              #weights = ceiling(1/rds_wght),
              data = datRDS)
tab_model(fit4)
AIC(fit4)

# logistic regression model using rms
rmsdata <- select(datRDS, calc_age, stdy_prtner2, num_sex, heavy_drinking, zone)
rms_data <- rms::datadist(rmsdata)
options(rms::datadist = "rms_data")

fit5 <- rms::lrm(hivout ~ calc_age + stdy_prtner2 + num_sex + heavy_drinking + zone,
                                        x = TRUE,
                                        y = TRUE,
                                        data = rms_data)
summary(fit5)

fit5 <- glm(hivout ~ calc_age + stdy_prtner2 + num_sex + syphstatus + heavy_drinking,
            family = "binomial",
            weights = ceiling(rds_wght),
            data = datRDS)
# using survey design
mydata <- datRDS[,c("calc_age","rds_wght","hivout","locxn","syphstatus","heavy_drinking","gbv_pol","num_sex","stdy_prtner","sexworkerKnow")]
mydesign <- svydesign(ids = ~ 1,
                      weights = ~ sexworkerKnow,
                      strata = NULL,
                      data = mydata)
m1 <- svyglm(hivout ~ calc_age + stdy_prtner + num_sex + syphstatus + heavy_drinking + gbv_pol + locxn,
             design = mydesign)
summary(m1)
tab_model(m1)
# working with random effects
#randoms <- ranef(m4,condVar = TRUE)
qq <- attr(ranef(m4, condVar = TRUE)[[1]], "condVar")
ggCaterpillar(ranef(fit3, condVar = TRUE), QQ = FALSE)
ggsave("images/forest_plot.png", width = 200, height = 300, units = "mm")
lattice::dotplot(ranef(fit3, condVar = TRUE))


# ROC curve for fixed effects model
class <- fit2$y
score <- qlogis(fit1$fitted.values)
roc_emp <- rocit(score = score,class = class,method = "emp")
ciROC_emp <- ciROC(roc_emp,level = 0.95)
tiff("images/ROC.tif",width = 250,height = 250,units = "mm",res = 500,compression = "lzw")
par(mfrow = c(1,1), cex = 1.5, cex.lab = 1.5, mar = c(4.7,4.5,2,2))
plot(roc_emp,values = T,lwd = 2, YIndex = FALSE)
text(0.15,0.95,"AUC = 0.73(0.67-0.83)")
dev.off()

# create a function to plot ROC curves for the different regression models

plot_model_roc <- function(model.fit,...){
  class <- model.fit$y
  score <- qlogis(model.fit$fitted.values)
  roc_emp <- rocit(score = score,class = class,method = "emp")
  ciROC_emp <- ciROC(roc_emp,level = 0.95)
  par(cex = 1.5, cex.lab = 1.5, mar = c(4.7,4.5,2,2))
  plot(roc_emp,values = T,lwd = 2, YIndex = FALSE,...)
}

plot_model_roc(fit2)

##  ---------------------------------- Baseline characteristics -----------------------------------------------------
baseline_data <- datRDS %>%
  #mutate(gbv = factor(ifelse(gbv == 1,"Yes","No"), levels = c("Yes","No"))) %>%
  mutate(gbv_pol = factor(ifelse(gbv_pol == 1, "Yes","No"),levels = c("Yes","No"))) %>%
  mutate(heavy_drinking = factor(ifelse(heavy_drinking == 1,"Yes","No"), levels = c("Yes","No")))

# remove seeds from baseline table
 seed_id <- which(datRDS$pid %in% seedID)
 baseline_data <- baseline_data[-seed_id,]

# baseline tables
tabcontrols  <- tableby.control(test = FALSE, 
                                total = TRUE,
                                numeric.test = "kwt", 
                                cat.test = "fe",  
                                numeric.stats = c("N","medianq1q3"),
                                cat.stats = c("N","countpct"),
                                digits = 1,
                                digits.pct = 1,
                                digits.p = 2,
                                stats.labels = list(
                                  N = 'N',
                                  meansd = "Mean(SD)",
                                  medianq1q3 = 'Median(IQR)'),
                                simulate.p.value = TRUE)
#labels(baseseline_data) <- c(calc_age = "Age (yrs)")
labels(baseline_data) <- c(calc_age = "Age (yrs)",
                #lengthstay = "Length of stay",
                findbar = "Bars/Nightclubs/Entertaiment place",
                findonline = "Phone/WhatsApp/Internet",
                gbv = "Gender-based violence",
                locxn = "Location",
                heavy_drinking = "Heavy drinking",
                #agecat = "Age group",
                syphstatus = "Syphilis",
                mrktfind = "Market place/Street",
                trucksfind = "Trucks/Highway",
                lodgefind = "Lodge/Hotel/Shabeen",
                educ = "Education",
                condomuse = "Condom use",
                marstatus = "Marital status",
                hhsize = "HH size",
                knwpersoncoupon = "Coupon giver",
                sexworkerKnow = "Sex workers known",
                sexworkerSeen = "Sex seen in past month",
                sexworkerRecruit = "Sex worker recruited")

tab1 <- tableby(hivstatus ~ calc_age + marstatus + educ + 
                  syphstatus +  
                  heavy_drinking + condomuse +
                  knwpersoncoupon + sexworkerKnow + gbv +
                  sexworkerSeen + sexworkerRecruit,
                data = baseline_data,
                control = tabcontrols)
t1_summary <- summary(tab1,text = T)
write2word(t1_summary,"baseline_table_characteristics.doc")
write2html(t1_summary,"baseline_RDS.htm")

tab2 <- tableby(hivstatus ~ locxn,
                data = baseline_data,
                control = tabcontrols)
write2word(tab2,"table_locations.doc")

# baseline table by zone

tab3 <- tableby(hivstatus ~ zone,
                data = baseline_data,
                control = tabcontrols)
summary(tab3)
write2word(tab3,"table_3_sep.docx")

mm <- tableby(gbv_comp ~ agecat + stdy_prtner + hivstatus + marstatus + topology,
              data = datRDS,
              control = tabcontrols)
summary(mm)


write2word(mm,"gbv_summaries.doc")
# baseline characteristics of the seeds
seeddata <- baseline_data[baseline_data$seedInfo == "seed",]

seed_properties <- seeddata[,c("pid","calc_age","locxn","hivstatus","sexworkerKnow")]
seed_properties <- baseline_data %>%
  as_tibble() %>%
  filter(seedInfo == "seed") %>%
  select(pid,calc_age,locxn,hivstatus,sexworkerKnow,educ,marstatus,topology)


U <- prop.table(table(rds$findonline,rds$educ),2)*100
W <- prop.table(table(rds$findbar,rds$educ),2)*100
Q <- prop.table(table(rds$mrktfind,rds$educ),2)*100
barplot(U,beside = T)


# baseline figures
datasumm <- rds %>%
  filter(!is.na(educ)) %>%
  group_by(educ,findonline) %>%
  summarise(nres = n_distinct(pid)) %>%
  mutate(nprop = nres/sum(nres)*100)
 
datasumm2 <- rds %>%
  filter(!is.na(educ)) %>%
  group_by(educ,findbar) %>%
  summarise(nres = n_distinct(pid)) %>%
  mutate(nprop = nres/sum(nres)*100) 

# graphs by location 
ggplot(datasumm2,aes(x = educ,y = nprop,fill = findbar)) +
  geom_bar(stat = "identity",position = position_dodge(preserve = "single")) +
  theme_minimal() +
  labs(title = "Client meeting location by education",subtitle = "Bar/Nightclubs/Entertainment place") +
  ylab("Proportion") +
  xlab("") +
  scale_fill_manual(values = c("lightsalmon",'lightblue')) +
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.title = element_blank(),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 25))
ggsave("images/meeting_place_bar_education.tiff",width = 500,height = 450,units = "mm",compression = "lzw")


ggplot(datasumm,aes(x = educ,y = nprop,fill = findonline)) +
  geom_bar(stat = "identity",position = position_dodge(preserve = "single")) +
  #geom_text(aes(label=round(nprop,1)),hjust=0.5,position = position_dodge(0.7),size=6) +
  theme_minimal() +
  labs(title = "Client meeting location by education",subtitle = "Telephone/WhatsApp/Internet") +
  ylab("Proportion") +
  xlab("") +
  scale_fill_manual(values = c("lightsalmon",'lightblue')) +
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.title = element_blank(),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 25))
ggsave("images/meeting_place_online_education.tiff",width = 500,height = 450,units = "mm",compression = "lzw")


ggplot(datasumm,aes(x = educ,y = nprop,fill = findbar)) +
  geom_bar(stat = "identity",position = position_dodge(preserve = "single")) +
  #geom_text(aes(label=round(nprop,1)),hjust=0.5,position = position_dodge(0.7),size=6) +
  theme_minimal() +
  labs(title = "Client meeting location by education",subtitle = "Bar") +
  ylab("Proportion") +
  xlab("") +
  scale_fill_manual(values = c("lightsalmon",'lightblue')) +
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.title = element_blank(),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 25))


# plotting function
plot_meeting_location <- function(data,meeting_place,meeting_place_title){
  ggplot(datasumm,aes(x = educ,y = nprop,fill = meeting_place)) +
    geom_bar(stat = "identity",position = position_dodge(preserve = "single")) +
    geom_text(aes(label = round(nprop,1)),hjust = 0.5,position = position_dodge(0.7),size = 6) +
    theme_minimal() +
    labs(title = "Client meeting location by education",subtitle = meeting_place_title) +
    ylab("Proportion") +
    scale_fill_manual(values = c("lightsalmon",'lightblue')) +
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
  pivot_longer(cols = c("findbar","findonline","mrktfind","lodgefind","trucksfind"),names_to = "loc_type",values_to = "location") %>%
  mutate(location_type = factor(case_when(
    loc_type=="findbar" ~ "Bar",
    loc_type=="findonline" ~ "Online",
    loc_type=="mrktfind" ~ "Market",
    loc_type=="lodgefind" ~ "Lodge",
    loc_type=="trucksfind" ~ "Trucks"
  )))


rdsLocation <- rdsLong %>%
  #filter(!is.na(educ))+
  group_by(location,location_type) %>%
  summarise(nres = n_distinct(pid)) %>%
  mutate(nprop = nres/sum(nres)*100)

ggplot(rdsLocation,aes(x = location_type,y = nprop)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylim(0,100) +
  ylab("Proportion") +
  #facet_wrap(~location_type)+
  scale_fill_manual(values = c("lightsalmon",'lightblue')) +
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.title = element_blank())

# hiv testing location
datatesting <- rds %>%
  filter(!is.na(wherehivTested)) %>%
  group_by(wherehivTested) %>%
  summarise(nres = n_distinct(pid)) %>%
  mutate(nprop = nres/sum(nres)*100)

ggplot(datatesting,aes(x = wherehivTested, y = nprop)) +
  geom_bar(stat = "identity",fill = "steelblue") +
  theme_minimal() +
  xlab("") +
  ylab("Proportion") +
  ylim(0,100) +
  coord_flip() +
  labs(title = "Where HIV test was done") +
  theme(axis.text.x = element_text(size = 38),
        axis.text.y = element_text(size = 38),
        axis.title.y = element_text(size = 38),
        axis.title.x = element_text(size = 38),
        plot.title = element_text(size = 40))
ggsave("images/last_hiv_test_place.tiff",width = 500,height = 450,units = "mm",compression = "lzw")


lastTest <- rds %>%
  filter(!is.na(whenhivtest)) %>%
  group_by(whenhivtest) %>%
  summarise(nres = n_distinct(pid)) %>%
  mutate(nprop = nres/sum(nres)*100)


h1 <- ggplot(lastTest,aes(x = whenhivtest,y = nprop)) +
  geom_bar(stat = "identity",fill = "salmon") +
  theme_minimal() +
  xlab("") +
  ylab("Proportion") +
  ylim(0,100) +
  labs(title = "When last HIV test was done") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 38),
        axis.text.y = element_text(size = 38),
        axis.title.y = element_text(size = 38),
        axis.title.x = element_text(size = 38),
        plot.title = element_text(size = 40))
ggsave("images/last_hiv_test.tiff",width = 500,height = 450,units = "mm",compression = "lzw")

# network sizes
agepositive <- datRDS %>%
  #filter(!is.na(hivres)) %>%
  group_by(hivstatus,agecat) %>%
  summarise(nres = n_distinct(pid)) %>%
  mutate(nprop = nres/sum(nres)*100)

ggplot(agepositive,aes(x = agecat,y = nprop,fill = hivstatus)) +
  geom_bar(stat = "identity",position = position_dodge(preserve = "single")) +
  theme_minimal() +
  xlab("") +
  ylab("Proportion") +
  ylim(0,100) +
  labs(title = "HIV status by age") +
  scale_fill_manual(values = c("salmon","steelblue","gold")) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = c(.87,.87),
        plot.title = element_text(size = 20))
ggsave("images/hiv_age.tiff",width = 500,height = 450,units = "mm",compression = "lzw")

# network by age
networkData <- rds %>%
  group_by(agecat) %>%
  summarise(nres = n_distinct(pid),
            nnsize = median(sexworkerKnow))
# characteristics by sex outcome

# break in sex work over the past 6 month
breakdata <- rds %>%
  filter(!is.na(sexbreak)) %>%
  filter(!is.na(hivres)) %>%
  group_by(sexbreak,hivres) %>%
  summarise(nres = n_distinct(pid)) %>%
  mutate(nprop = nres/sum(nres)*100)

ggplot(breakdata,aes(x = sexbreak,y = nprop,fill = hivres)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  theme_minimal() +
  xlab("") +
  ylab("Proportion") +
  ylim(0,100) +
  labs(title = "Break in sex work in the past 6 months by HIV status") +
  scale_fill_manual(values = c("salmon","gold","steelblue")) +
  theme(axis.text = element_text(size = 38),
        axis.title = element_text(size = 38),
        legend.title = element_blank(),
        legend.text = element_text(size = 33),
        legend.position = c(.87,.87),
        plot.title = element_text(size = 40))
ggsave("images/sex_work_break.tiff",width = 500,height = 450,units = "mm",compression = "lzw")

# future HIV risk
futurerisk <- rds %>%
  filter(!is.na(hivrisk)) %>%
  #filter(!is.na(hivres)) %>%
  group_by(hivrisk) %>%
  summarise(nres = n_distinct(pid)) %>%
  mutate(nprop = nres/sum(nres)*100)

h2 <- ggplot(futurerisk,aes(x = hivrisk,y = nprop)) +
  geom_bar(stat = "identity",fill = "salmon") +
  theme_minimal() +
  xlab("") +
  ylab("Proportion") +
  ylim(0,100) +
  labs(title = "HIV risk in the next 5 years") +
  #coord_flip()+
  theme(axis.text.x = element_text(size = 38),
        axis.text.y = element_text(size = 38),
        axis.title.y = element_text(size = 38),
        axis.title.x = element_text(size = 38),
        plot.title = element_text(size = 40))
ggsave("images/future_hiv_risk.tiff",width = 500,height = 450,units = "mm",compression = "lzw")

gridExtra::grid.arrange(h1,h2)
ggsave("images/testing_future_risk.tiff",height = 600,width = 500,units = "mm",compression = "lzw")

# --------------------------------------------------------------------------------------------------------------------------------------
# relationships between mental health, alcohol abuse and violence
mentalHealth <- rds %>%
  group_by(violence01,drink_freq,think_deep) %>%
  summarise(nres=n()) %>%
  mutate(nprop=nres/sum(nres)*100)

ggplot(mentalHealth,aes(x=drink_freq,y=nprop,fill=think_deep))+
  geom_bar(stat = "identity",fill = "salmon")+
  theme_bw() +
  xlab("") +
  ylab("Proportion") +
  ylim(0,100)+
  labs(title = "HIV risk in the next 5 years") +
  facet_wrap(~violence01) +
  theme(axis.text.x = element_text(size = 38),
        axis.text.y = element_text(size = 38),
        axis.title.y = element_text(size = 38),
        axis.title.x = element_text(size = 38),
        plot.title = element_text(size = 40))
#--------------------- diary survey sampling ------------------------------
set.seed(309)
diary_data_pos <- rdsfinal %>%
  filter(hivstatus == "Positive") %>%
  #group_by(hivstatus) %>%
  slice_sample(n=60)

set.seed(230)
diary_data_neg <- rdsfinal %>%
  filter(hivstatus == "Negative") %>%
  slice_sample(n = 40)

diary_data <- bind_rows(diary_data_neg,diary_data_pos)
write.csv(diary_data,"data/diary_data.csv",row.names = F)

# remaining participants
k <- which(rdsfinal$pid %in% setdiff(rdsfinal$pid,diary_data$pid))
dataRemain <- rdsfinal[k,]
set.seed(45)
dataRemainPos <- dataRemain %>%
  filter(hivstatus == "Positive") 

write.csv(dataRemainPos,"data/hivposData.csv",row.names = F)
dataRemainNeg <- dataRemain %>%
  filter(hivstatus == "Negative")
write.csv(dataRemainNeg,"data/hivnegData.csv",row.names = F)


# to do

# seeds characteristics table

# participants who found clients online
online_clients <-  finaldata %>%
  filter(onlinefind == 1)
write.csv(online_clients,file = "data/online_clients.csv",row.names = F)


# ------------------------------------------------------------------------------------------------
# STI country analysis
# ------------------------------------------------------------------------------------------------


sti_data2 <- datRDS %>%
  pivot_longer(cols = c(syphstatus, chlam_result, gono_result), names_to = "STI", values_to = "sti_status") %>%
  mutate(sti_status = gsub("Not Detected", "Negative", sti_status)) %>%
  mutate(sti_status = gsub("Detected", "Positive", sti_status)) %>%
  mutate(STI = factor(case_when(
    STI == "syphstatus" ~ "Syphilis",
    STI == "gono_result" ~ "Gonorrhea",
    STI == "chlam_result" ~ "Chlamydia"))) %>%
  group_by(sti_status) %>%
  summarise(num_pos = n_distinct(pid))


chlam_data <- datRDS %>%
  group_by(chlam_result, age) %>%
  summarise(num = n_distinct(pid)) %>%
  #ungroup() %>%
  mutate(perc = num/sum(num, na.rm = TRUE) * 100) %>%
  mutate(chlam_result = factor(case_when(
    chlam_result == "Detected" ~ "Positive",
    chlam_result == "Not Detected" ~ "Negative"))) %>%
  mutate(age = factor(ifelse(age == "Young", "18-25", "25+"))) %>%
  ungroup()
  

gono_data <- datRDS %>%
  group_by(gono_result, age) %>%
  summarise(num = n_distinct(pid)) %>%
  mutate(perc = num/sum(num, na.rm = TRUE) * 100) %>%
  mutate(gono_result = factor(case_when(
    gono_result == "Detected" ~ "Positive",
    gono_result == "Not Detected" ~ "Negative"))) %>%
  mutate(age = factor(ifelse(age == "Young", "18-25", "25+")))

syph_data <- datRDS %>%
  group_by(syphstatus, age) %>%
  summarise(num = n_distinct(pid)) %>%
  mutate(perc = num/sum(num, na.rm = TRUE) * 100) %>%
  mutate(age = factor(ifelse(age == "Young", "18-25", "25+")))

ggplot(chlam_data, aes(x = age, y = perc, fill = chlam_result)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("lightsalmon","steelblue","grey50")) +
  theme_minimal() +
  ylim(0, 100) +
  labs(x = "", y = "Proportion", fill = "", title = "Chlamydia prevalence") +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.position = "bottom",
        plot.title = element_text(size = 29))
ggsave("images/chlamydia_prevalence.png", width = 300, height = 300, units = "mm")


ggplot(gono_data, aes(x = age, y = perc, fill = gono_result)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("lightsalmon","steelblue","grey50")) +
  theme_minimal() +
  ylim(0, 100) +
  labs(x = "", y = "Proportion", fill = "", title = "Gonorrhoea prevalence") +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.position = "bottom",
        plot.title = element_text(size = 29))
ggsave("images/gonorrhoea_prevalence.png", width = 300, height = 300, units = "mm")

ggplot(syph_data, aes(x = age, y = perc, fill = syphstatus)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("lightsalmon","steelblue")) +
  theme_minimal() +
  ylim(0, 100) +
  labs(x = "", y = "Proportion", fill = "", title = "Syphilis prevalence") +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.position = "bottom",
        plot.title = element_text(size = 29))
ggsave("images/syphilis_prevalence.png", width = 300, height = 300, units = "mm")


# create baseline tables

s1 <- tableby(chlam_result ~ age, data = datRDS)
s2 <- tableby(syphstatus ~ age, data = datRDS)
s3 <- tableby(gono_result ~ age, data = datRDS)

# diagnoses vs treatment

trt1 <- tableby(sti_treat ~ factor(sti_symp), data = sti_data)

trt2 <- tableby(chlam_result ~ factor(sti_symp), data = sti_data)
trt3 <- tableby(gono_result ~ factor(sti_symp), data = sti_data)
trt4 <- tableby(syphstatus ~ factor(sti_symp), data = sti_data)


# sti infection and hiv 
gonorrhoea_hiv <- datRDS %>%
  filter(gono_result == "Detected") #%>%
  #filter(hivstatus == "Positive")

syphilis_hiv <- datRDS %>%
  filter(syphstatus == "Positive")

chlamydia_hiv <- datRDS %>%
  filter(chlam_result == "Detected")


# check proportions with hiv for each of these STI

rds_gono_hiv <- RDS.II.estimates(gonorrhoea_hiv, outcome.variable = "hivstatus")
rds_syph_hiv <- RDS.II.estimates(syphilis_hiv, outcome.variable = "hivstatus")
rds_chlam_hiv <- RDS.II.estimates(chlamydia_hiv, outcome.variable = "hivstatus")

# create variable for STI symptoms

sti_data <- datRDS %>%
  mutate(sti_symp = ifelse(c49 == 1 | c50 == 1 | c51 == 1 | c52 == 1, 1, 0)) %>%
  mutate(sti_treat = ifelse(c53 == 1, 1, 0)) 

# create a variable for any sti
sti_data$sti_any <- 0
sti_data$sti_any[sti_data$chlam_result == "Detected" | sti_data$gono_result == "Detected" | sti_data$syphstatus == "Positive"] <- 1


# add sti to the rds data
datRDS$sti_any <- 0
datRDS$sti_any[datRDS$chlam_result == "Detected" | datRDS$gono_result == "Detected" | datRDS$syphstatus == "Positive"] <- 1
datRDS$sti_any <- factor(datRDS$sti_any)

# add sti symptoms and treatment to the data

new_rds <- datRDS %>%
  mutate(sti_symp = factor(ifelse(c49 == 1 | c50 == 1 | c51 == 1 | c52 == 1, 1, 0))) %>%
  mutate(sti_treat = factor(ifelse(c53 == 1, 1, 0))) 

sti_data_hiv <- sti_data %>%
  filter(sti_any == 1)

rds_anysti <- RDS.II.estimates(sti_data[sti_data$sti_any == 1,], outcome.variable = "hivstatus")

# prevalence among women who recently reported HIV negative test
# variable c05

recent_test <- new_rds %>%
  filter(hivres == "HIV -ve")

recent_test_gono_hivneg <- RDS.II.estimates(recent_test[recent_test$hivstatus == "Negative",], outcome.variable = "gono_result")
recent_test_gono_hivpos <- RDS.II.estimates(recent_test[recent_test$hivstatus == "Positive",], outcome.variable = "gono_result")

recent_test_syph_hivneg <- RDS.II.estimates(recent_test[recent_test$hivstatus == "Negative",], outcome.variable = "syphstatus")
recent_test_syph_hivpos <- RDS.II.estimates(recent_test[recent_test$hivstatus == "Positive",], outcome.variable = "syphstatus")

recent_test_gono_hivneg <- RDS.II.estimates(recent_test[recent_test$hivstatus == "Negative",], outcome.variable = "chlam_result")
recent_test_gono_hivpos <- RDS.II.estimates(recent_test[recent_test$hivstatus == "Positive",], outcome.variable = "chlam_result")

recent_test_sti_hivneg <- RDS.II.estimates(recent_test[recent_test$hivstatus == "Negative",], outcome.variable = "sti_any")
recent_test_sti_hivpos <- RDS.II.estimates(recent_test[recent_test$hivstatus == "Positive",], outcome.variable = "sti_any")

# prevalence by age
chlam_prev_age_young <- RDS.II.estimates(datRDS[datRDS$age == "Young",], outcome.variable = "chlam_result")
chlam_prev_age_adult <- RDS.II.estimates(datRDS[datRDS$age == "Adult",], outcome.variable = "chlam_result")

syph_prev_age_young <- RDS.II.estimates(datRDS[datRDS$age == "Young",], outcome.variable = "syphstatus")
syph_prev_age_adult <- RDS.II.estimates(datRDS[datRDS$age == "Adult",], outcome.variable = "syphstatus")

gono_prev_age_young <- RDS.II.estimates(datRDS[datRDS$age == "Young",], outcome.variable = "gono_result")
gono_prev_age_adult <- RDS.II.estimates(datRDS[datRDS$age == "Adult",], outcome.variable = "gono_result")

sti_any_age_young <- RDS.II.estimates(datRDS[datRDS$age == "Young",], outcome = "sti_any")
sti_any_age_adult <- RDS.II.estimates(datRDS[datRDS$age == "Adult",], outcome = "sti_any")

# sti prevalence by HIV 

rds_gono_hivneg <- RDS.II.estimates(datRDS[datRDS$hivstatus == "Negative",], outcome.variable = "gono_result")
rds_gono_hivpos <- RDS.II.estimates(datRDS[datRDS$hivstatus == "Positive",], outcome.variable = "gono_result")

rds_syph_hivneg <- RDS.II.estimates(datRDS[datRDS$hivstatus == "Negative",], outcome.variable = "syphstatus")
rds_syph_hivpos <- RDS.II.estimates(datRDS[datRDS$hivstatus == "Positive",], outcome.variable = "syphstatus")

rds_chlam_hivneg <- RDS.II.estimates(datRDS[datRDS$hivstatus == "Negative",], outcome.variable = "chlam_result")
rds_chlam_hivpos <- RDS.II.estimates(datRDS[datRDS$hivstatus == "Positive",], outcome.variable = "chlam_result")

rds_sti_hivneg <- RDS.II.estimates(datRDS[datRDS$hivstatus == "Negative",], outcome.variable = "sti_any")
rds_sti_hivpos <- RDS.II.estimates(datRDS[datRDS$hivstatus == "Positive",], outcome.variable = "sti_any")

# sti diagnosis and treatment proportions and comparisons
rds_gono_symppos <- RDS.II.estimates(new_rds[new_rds$sti_symp], outcome.variable = "gono_result")
rds_gono_sympneg <- RDS.II.estimates(datRDS[datRDS$sti_symp == 0,], outcome.variable = "gono_result")

rds_syph_symppos <- RDS.II.estimates(datRDS[datRDS$sti_symp == 1,], outcome.variable = "syphstatus")
rds_syph_sympneg <- RDS.II.estimates(datRDS[datRDS$sti_symp == 0,], outcome.variable = "syphstatus")

rds_chlam_symppos <- RDS.II.estimates(datRDS[datRDS$sti_symp == 1,], outcome.variable = "chlam_result")
rds_chlam_sympneg <- RDS.II.estimates(datRDS[datRDS$sti_symp == 0,], outcome.variable = "chlam_result")

rds_sti_symppos <- RDS.II.estimates(datRDS[datRDS$sti_symp == 1,], outcome.variable = "sti_any")
rds_sti_sympneg <- RDS.II.estimates(datRDS[datRDS$sti_symp == 0,], outcome.variable = "sti_any")

# with symptoms
U <- new_rds[new_rds$sti_symp == 1,]
M <- new_rds[new_rds$sti_symp == 0,]
fisher.test(table(new_rds$syphstatus,new_rds$sti_symp))
fisher.test(table(new_rds$sti_any,new_rds$sti_symp))
fisher.test(table(new_rds$gono_result,new_rds$sti_symp))
fisher.test(table(new_rds$chlam_result,new_rds$sti_symp))

# participants data

piddata <- read_csv("data/Participants_Attribute.csv", skip = 1)

k = which(datRDS$pid %in% piddata$`PID #`)
U = datRDS[k,]

Udata <- U |>
  select(pid, chargefee, hhsize, syphstatus, gono_result, chlam_result)
write.csv(Udata, "data/participant_attribute_data.csv", row.names = FALSE)
