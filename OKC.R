# This script looks at the vote about publicly funding the arena in OKC

# Loading libraries

library(sf)
library(AER)
library(dplyr)
library(lmtest)
library(tigris)
library(ggrepel)
library(ggplot2)
library(leaflet)
library(sandwich)
library(patchwork)
library(geomander)
library(tinytiger)
library(stargazer)
library(tidycensus)
library(modelsummary)

# Project directory

direc <- 'D:/OKC/'

# Reading in the data

votes <- read.csv(paste0(direc, 'data/20231212_PrecinctResults.csv'))
ok <- read.csv(paste0(direc, 'data/20221108_PrecinctResults.csv'))
xcovars <- read.csv(paste0(direc, 'data/nhgis0001_ds258_2020_block.csv'))
precs <- read_sf(paste0(direc, 'data/Voter_Precincts_2020/Voter_Precincts_2020.shp'))

# Getting blocks shapefile for OK

blockos <- blocks(state = 'OK', year = 2022)

# Obtaining 2020 census block level data for covariates except income and educational attainment

canadian <- create_block_table(state = 'OK', county = '017', year = 2020)
cleveland <- create_block_table(state = 'OK', county = '027', year = 2020)
oklahoma <- create_block_table(state = 'OK', county = '109', year = 2020)
pottawatomie <- create_block_table(state = 'OK', county = '125', year = 2020)

# Joining the county level spatial data.frames

covars <- rbind(canadian, cleveland, oklahoma, pottawatomie)

# Matching the crs of the spatial data.frames

covars <- covars %>% st_transform(4326)

# Joining covars and xcovars

xcovars$GEOCODE <- as.character(xcovars$GEOCODE)
xcovars <- xcovars %>% filter(COUNTY %in% c('Canadian County', 'Cleveland County', 'Oklahoma County', 'Pottawatomie County'))
xcovars <- xcovars[,c('GEOCODE', 'U7O001', 'U8R001', 'U7S001', 'U7S002', 'U9X001', 'U9X003', 'VAL004', 'U9U001', 'U9U002', 'U9Y001', 'U9Y002', 'VAP001', 'VAP002', 'VAP003', 'VAP004', 'VAP005', 'VAP006', 'VAP007', 'VAP008')]

covars <- covars %>% st_drop_geometry()
covars <- covars[,c(1,6:23)]
covars <- covars %>% left_join(xcovars, by = c('GEOID' = 'GEOCODE'))

# Joining blockos to the above

blockos <- blockos %>% filter(COUNTYFP20 %in% c('017', '027', '109', '125'))
blockos <- blockos[,c(5,14,15,18)]
covars <- covars %>% left_join(blockos, by = c('GEOID' = 'GEOID20'))

# Spatial matching of votes and precs

precs$PCT_CEB <- as.integer(precs$PCT_CEB)
votes <- votes %>% left_join(precs, by = c('precinct' = 'PCT_CEB'))
votes <- votes %>% filter(COUNTY_NAM %in% c('CANADIAN', 'CLEVELAND', 'OKLAHOMA', 'POTTAWATOMIE'))

# Spatial matching of votes and ok

ok <- ok %>% filter(race_description == 'FOR UNITED STATES SENATOR')

tv <- c()

for (i in 1:nrow(ok)) {
  
  tmp <- ok %>% filter(precinct == ok$precinct[i])
  tv <- c(tv, sum(tmp$cand_tot_votes))
  
}

ok$total_votes <- tv

repub <- ok %>% filter(cand_party == 'REP')
demo <- ok %>% filter(cand_party == 'DEM')
liber <- ok %>% filter(cand_party == 'LIB')

repub <- repub[,c('precinct', 'cand_tot_votes', 'total_votes')]
demo <- demo[,c('precinct', 'cand_tot_votes', 'total_votes')]
liber <- liber[,c('precinct', 'cand_tot_votes', 'total_votes')]

repub$Republican <- repub$cand_tot_votes / repub$total_votes
demo$Democrat <- demo$cand_tot_votes / demo$total_votes
liber$Libertarian <- liber$cand_tot_votes / liber$total_votes

colnames(repub)[2] <- 'R_Votes'
colnames(demo)[2] <- 'D_Votes'
colnames(liber)[2] <- 'L_Votes'

repub$precinct <- as.integer(repub$precinct)
demo$precinct <- as.integer(demo$precinct)
liber$precinct <- as.integer(liber$precinct)

props <- c()
counts <- c()

for (i in 1:nrow(votes)) {
  
  tmp <- votes %>% filter(precinct == votes$precinct[i])
  num <- tmp[which(tmp$cand_name == 'FOR THE PROPOSITION - YES'),]$cand_tot_votes[1]
  denom <- sum(tmp$cand_tot_votes)
  props <- c(props, num/denom)
  counts <- c(counts, denom)
  
}

votes$Y <- props
votes$YES_Votes <- counts
votes <- votes %>% filter(cand_name == 'FOR THE PROPOSITION - YES')

votes <- votes %>% left_join(repub, by = c('precinct' = 'precinct'))
votes <- votes %>% left_join(demo, by = c('precinct' = 'precinct'))
votes <- votes %>% left_join(liber, by = c('precinct' = 'precinct'))

# Identifying which blocks in covars match to which precincts in votes

covars <- st_as_sf(covars)
votes <- st_as_sf(votes)

covars <- covars %>% st_set_crs(4326)
votes <- votes %>% st_set_crs(4326)

inter <- st_intersects(covars$geometry, votes$geometry)

# Completing the precinct-level data.frame

pop <- c()
vapw <- c()
vapb <- c()
vaph <- c()
vapa <- c()
age <- c()
mens <- c()
hounits <- c()
vaca.rate <- c()
pct.vaca.sale <- c()
pct.kiddos <- c()
mort <- c()

covars[is.na(covars)] <- 0

for (i in 1:nrow(votes)) {
  
  print(paste0('Checking precinct ', i, ' of ', nrow(votes), '.......'))
  
  these <- c()
  
  for (j in 1:nrow(covars)) {
    
    if (i %in% inter[[j]]) {
      
      these <- c(these, j)
      
    }
    
  }
  
  tmp <- covars[these,]
  
  pop <- c(pop, sum(tmp$U7O001))
  vapw <- c(vapw, sum(tmp$vap_white) / sum(tmp$vap))
  vapb <- c(vapb, sum(tmp$vap_black) / sum(tmp$vap))
  vaph <- c(vaph, sum(tmp$vap_hisp) / sum(tmp$vap))
  vapa <- c(vapa, sum(tmp$vap_asian) / sum(tmp$vap))
  age <- c(age, sum(tmp$U8R001*tmp$U7O001) / sum(tmp$U7O001))
  mens <- c(mens, sum(tmp$U7S002) / sum(tmp$U7S001))
  hounits <- c(hounits, sum(tmp$U9X001))
  vaca.rate <- c(vaca.rate, sum(tmp$U9X003) / sum(tmp$U9X001))
  pct.vaca.sale <- c(pct.vaca.sale, sum(tmp$VAL004*tmp$U9X001) / sum(tmp$U9X001))
  pct.kiddos <- c(pct.kiddos, sum(tmp$U9U002) / sum(tmp$U9U001))
  mort <- c(mort, sum(tmp$U9Y002) / sum(tmp$U9Y001))
  
}

votes$Population <- pop
votes$VAP_White <- vapw
votes$VAP_Black <- vapb
votes$VAP_Hispanic <- vaph
votes$VAP_Asian <- vapa
votes$Age <- age
votes$Men <- mens
votes$Housing_Units <- hounits
votes$Vacancy_Rate <- vaca.rate
votes$Pct_Vacant_for_Sale <- pct.vaca.sale
votes$Pct_Children <- pct.kiddos
votes$Mortgage <- mort

# Getting tract level data on income and educational attainment

ok.inc.ed <- get_acs(state = 'OK', geography = 'tract', year = 2021, variables = c('DP03_0062', 'DP02_0068P', 'DP03_0009P', 'DP02_0053', 'DP02_0053P', 'DP02_0083P', 'DP03_0019P', 'DP03_0033', 'DP03_0034', 'DP03_0035', 'DP03_0036', 'DP03_0037', 'DP03_0038', 'DP03_0039', 'DP03_0040', 'DP03_0041', 'DP03_0042', 'DP03_0043', 'DP03_0044', 'DP03_0045'))

# Getting tracts shapefile for OK

tractors <- tracts(state = 'OK', year = 2021)

# Joining ok.in.ed and tractors

tractors <- tractors[,c(4,13)]
ok.inc.ed <- ok.inc.ed %>% left_join(tractors, by = c('GEOID' = 'GEOID'))

# Add income and education, etc. data to votes

ok.inc.ed <- st_as_sf(ok.inc.ed)
votes <- st_as_sf(votes)

ok.inc.ed <- ok.inc.ed %>% st_set_crs(4326)
votes <- votes %>% st_set_crs(4326)

inter.milan <- st_intersects(ok.inc.ed$geometry, votes$geometry)

# Completing the precinct-level data.frame, for real this time

inc <- c()
ed <- c()
emp <- c()
kids.in.school <- c()
same.town.py <- c()
pct.commute.by.car <- c()
jobs.outdoors <- c()
jobs.construction <- c()
jobs.manufacuring <- c()
jobs.wholesale <- c()
jobs.retail <- c()
jobs.transpo <- c()
jobs.information <- c()
jobs.finance <- c()
jobs.professional <- c()
jobs.ed.health.social.ass <- c()
jobs.amenities <- c()
jobs.other <- c()
jobs.public.admin <- c()
jobs.all <- c()

for (i in 1:nrow(votes)) {
  
  print(paste0('Checking precinct ', i, ' of ', nrow(votes), '.......'))
  
  these <- c()
  
  for (j in 1:nrow(ok.inc.ed)) {
    
    if (i %in% inter.milan[[j]]) {
      
      these <- c(these, j)
      
    }
    
  }
  
  tmp <- ok.inc.ed[these,]
  tmp.inc <- tmp %>% filter(variable == 'DP03_0062')
  tmp.ed <- tmp %>% filter(variable == 'DP02_0068P')
  tmp.emp <- tmp %>% filter(variable == 'DP03_0009P')
  tmp.school <- tmp %>% filter(variable == 'DP02_0053')
  tmp.townies <- tmp %>% filter(variable == 'DP02_0083P')
  tmp.commute <- tmp %>% filter(variable == 'DP03_0019P')
  tmp.outdoors <- tmp %>% filter(variable == 'DP03_0033')
  tmp.constr <- tmp %>% filter(variable == 'DP03_0034')
  tmp.manuf <- tmp %>% filter(variable == 'DP03_0035')
  tmp.whole <- tmp %>% filter(variable == 'DP03_0036')
  tmp.retail <- tmp %>% filter(variable == 'DP03_0037')
  tmp.trans <- tmp %>% filter(variable == 'DP03_0038')
  tmp.info <- tmp %>% filter(variable == 'DP03_0039')
  tmp.fin <- tmp %>% filter(variable == 'DP03_0040')
  tmp.pro <- tmp %>% filter(variable == 'DP03_0041')
  tmp.soc.welf <- tmp %>% filter(variable == 'DP03_0042')
  tmp.amenit <- tmp %>% filter(variable == 'DP03_0043')
  tmp.other <- tmp %>% filter(variable == 'DP03_0044')
  tmp.pa <- tmp %>% filter(variable == 'DP03_0045')
  
  inc <- c(inc, mean(tmp.inc$estimate))
  ed <- c(ed, mean(tmp.ed$estimate))
  emp <- c(emp, mean(tmp.emp$estimate))
  kids.in.school <- c(kids.in.school, sum(tmp.school$estimate))
  same.town.py <- c(same.town.py, mean(tmp.townies$estimate))
  pct.commute.by.car <- c(pct.commute.by.car, mean(tmp.commute$estimate))
  jobs.outdoors <- c(jobs.outdoors, sum(tmp.outdoors$estimate))
  jobs.construction <- c(jobs.construction, sum(tmp.constr$estimate))
  jobs.manufacuring <- c(jobs.manufacuring, sum(tmp.manuf$estimate))
  jobs.wholesale <- c(jobs.wholesale, sum(tmp.whole$estimate))
  jobs.retail <- c(jobs.retail, sum(tmp.retail$estimate))
  jobs.transpo <- c(jobs.transpo, sum(tmp.trans$estimate))
  jobs.information <- c(jobs.information, sum(tmp.info$estimate))
  jobs.finance <- c(jobs.finance, sum(tmp.fin$estimate))
  jobs.professional <- c(jobs.professional, sum(tmp.pro$estimate))
  jobs.ed.health.social.ass <- c(jobs.ed.health.social.ass, sum(tmp.soc.welf$estimate))
  jobs.amenities <- c(jobs.amenities, sum(tmp.amenit$estimate))
  jobs.other <- c(jobs.other, sum(tmp.other$estimate))
  jobs.public.admin <- c(jobs.public.admin, sum(tmp.pa$estimate))
  
}

votes$Income <- inc
votes$Education <- ed
votes$Unemployment_Rate <- emp
votes$Kids_In_School <- kids.in.school
votes$Pct_Commute_By_Car <- pct.commute.by.car
votes$Townies <- same.town.py
votes$Jobs_Outdoors <- jobs.outdoors
votes$Jobs_Construction <- jobs.construction
votes$Jobs_Manufacturing <- jobs.manufacuring
votes$Jobs_Wholesale <- jobs.wholesale
votes$Jobs_Retail <- jobs.retail
votes$Jobs_Transportation <- jobs.transpo
votes$Jobs_Information <- jobs.information
votes$Jobs_Finance <- jobs.finance
votes$Jobs_Professional <- jobs.professional
votes$Jobs_Social_Work <- jobs.ed.health.social.ass
votes$Jobs_Amenities <- jobs.amenities
votes$Jobs_Other <- jobs.other
votes$Jobs_Public_Administration <- jobs.public.admin

votes$Jobs_All <- votes$Jobs_Outdoors + votes$Jobs_Construction + votes$Jobs_Manufacturing + votes$Jobs_Wholesale + votes$Jobs_Retail + votes$Jobs_Transportation + votes$Jobs_Information + votes$Jobs_Finance + votes$Jobs_Professional + votes$Jobs_Social_Work + votes$Jobs_Amenities + votes$Jobs_Other + votes$Jobs_Public_Administration

votes$Pct_Outdoors <- votes$Jobs_Outdoors / votes$Jobs_All
votes$Pct_Construction <- votes$Jobs_Construction / votes$Jobs_All
votes$Pct_Manufacturing <- votes$Jobs_Manufacturing / votes$Jobs_All
votes$Pct_Wholesale <- votes$Jobs_Wholesale / votes$Jobs_All
votes$Pct_Retail <- votes$Jobs_Retail / votes$Jobs_All
votes$Pct_Transportation <- votes$Jobs_Transportation / votes$Jobs_All
votes$Pct_Information <- votes$Jobs_Information / votes$Jobs_All
votes$Pct_Finance <- votes$Jobs_Finance / votes$Jobs_All
votes$Pct_Professional <- votes$Jobs_Professional / votes$Jobs_All
votes$Pct_Social_Work <- votes$Jobs_Social_Work / votes$Jobs_All
votes$Pct_Amenities <- votes$Jobs_Amenities / votes$Jobs_All
votes$Pct_Other <- votes$Jobs_Other / votes$Jobs_All
votes$Pct_Public_Administration <- votes$Jobs_Public_Administration / votes$Jobs_All

# Compute distance from each precinct (centroid) to paycom stadium

centres <- st_centroid(votes$geometry)
plon <- -97.51506015635016
plat <- 35.46340716716361
paycom <- as.data.frame(cbind(plon,plat))
paycom <- st_as_sf(paycom, coords = c(1,2))
paycom <- paycom %>% st_set_crs(4326)

proxy.dists <- c()

for (i in 1:nrow(votes)) {
  
  proxy.dists <- c(proxy.dists, st_distance(paycom, centres[i]))
  
}

votes$Distance <- proxy.dists

# Converting meters to kilometers

votes$Kilometers <- votes$Distance / 1000

# Concerting some other percentage-based variables for consistency

votes$Education <- votes$Education / 100
votes$Pct_Vacant_for_Sale <- votes$Pct_Vacant_for_Sale / 100
votes$Pct_Commute_By_Car <- votes$Pct_Commute_By_Car / 100
votes$Townies <- votes$Townies / 100

# Run 'YES' vote share regressions

model1 <- lm(Y ~ Republican + Democrat + Libertarian + log(Population+1) + VAP_Black + VAP_Asian
               + VAP_Hispanic + VAP_White + Age + Men + Pct_Children + Townies
               + Pct_Commute_By_Car + log(Income+1) + Education + Unemployment_Rate
               + log(Housing_Units+1) + Vacancy_Rate + Pct_Vacant_for_Sale + Mortgage + log(Kilometers+1)
               + Pct_Public_Administration + Pct_Construction + Pct_Manufacturing + Pct_Wholesale + Pct_Retail
               + Pct_Transportation + Pct_Information + Pct_Finance + Pct_Professional + Pct_Social_Work
               + Pct_Amenities + Pct_Other, data = votes)

model2 <- lm(Y ~ log(R_Votes+1) + log(D_Votes+1) + log(L_Votes+1) + log(Population+1)
             + VAP_Black + VAP_Asian + VAP_Hispanic + VAP_White + Age + Men + Pct_Children + Townies
             + Pct_Commute_By_Car + log(Income+1) + Education + Unemployment_Rate
             + log(Housing_Units+1) + Vacancy_Rate + Pct_Vacant_for_Sale + Mortgage + log(Kilometers+1)
             + Pct_Public_Administration + Pct_Construction + Pct_Manufacturing + Pct_Wholesale + Pct_Retail
             + Pct_Transportation + Pct_Information + Pct_Finance + Pct_Professional + Pct_Social_Work
             + Pct_Amenities + Pct_Other, data = votes)

# Calculating heteroskedasticity-robust standard errors

model1.robust <- coeftest(model1, vcov = vcovCL(model1, type = 'HC0'))
model2.robust <- coeftest(model2, vcov = vcovCL(model2, type = 'HC0'))

# Viewing the results

stargazer(model1.robust, model11.robust, type = 'text')

# Saving the results

write.csv(stargazer(model1.robust, model2.robust, type = 'text'), paste0(direc, 'results/text.txt'), row.names = FALSE)
write.csv(stargazer(model1.robust, model2.robust), paste0(direc, 'results/latex.txt'), row.names = FALSE)

# Creating a leaflet to show the results of the referendum

mapdat <- read.csv(paste0(direc, 'data/20231212_PrecinctResults.csv'))

props <- c()

for (i in 1:nrow(mapdat)) {
  
  tmp <- mapdat %>% filter(precinct == mapdat$precinct[i])
  num <- tmp[which(tmp$cand_name == 'FOR THE PROPOSITION - YES'),]$cand_tot_votes[1]
  denom <- sum(tmp$cand_tot_votes)
  props <- c(props, num/denom)
  
}

mapdat$Y <- props
mapdat <- mapdat %>% filter(cand_name == 'FOR THE PROPOSITION - YES')
mapdat <- mapdat %>% left_join(precs, by = c('precinct' = 'PCT_CEB'))
mapdat <- mapdat %>% filter(COUNTY_NAM %in% c('CANADIAN', 'CLEVELAND', 'OKLAHOMA', 'POTTAWATOMIE'))
mapdat <- mapdat %>% filter(is.na(Y) == FALSE)

pal <- colorNumeric(palette = 'Greys', domain = mapdat$Y)

grass <- leaflet(mapdat$geometry) %>% setView(lng = -97.51506015635016, lat = 35.46340716716361, zoom = 10) %>% addTiles() %>% 
                 addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = pal(mapdat$Y))

grass

# Of the groups that seemed to vote for the tax, what proportion of total jobs do they constitute?

prop <- (sum(votes$Jobs_Construction) + sum(votes$Jobs_Manufacturing) + sum(votes$Jobs_Wholesale) + sum(votes$Jobs_Retail) + sum(votes$Jobs_Transportation) + sum(votes$Jobs_Professional) + sum(votes$Jobs_Other)) / sum(votes$Jobs_All)
prop2 <- (sum(votes$Jobs_Amenities) + sum(votes$Jobs_Construction) + sum(votes$Jobs_Manufacturing) + sum(votes$Jobs_Wholesale) + sum(votes$Jobs_Retail) + sum(votes$Jobs_Transportation) + sum(votes$Jobs_Professional) + sum(votes$Jobs_Other)) / sum(votes$Jobs_All)

prop # 49.9\%
prop2 # 59.1\%

# Voter turnout relative to the 2022 US Senate vote

votes.2023 <- sum(votes$YES_Votes)
votes.2022 <- sum(votes$total_votes)
turnout.ratio <- votes.2023 / votes.2022

turnout.ratio # 29.12\%

# Adding a turnout column to votes

votes$Turnout_Ratio <- votes$YES_Votes / votes$total_votes

# Run voting turnout regressions (YES_Votes is the total number of votes, not the number of YES votes)

model3 <- lm(Turnout_Ratio ~ Y + Republican + Democrat + Libertarian +log(R_Votes+1) + log(D_Votes+1) + log(L_Votes+1) + log(Population+1)
             + VAP_Black + VAP_Asian + VAP_Hispanic + VAP_White + Age + Men + Pct_Children + Townies
             + Pct_Commute_By_Car + log(Income+1) + Education + Unemployment_Rate
             + log(Housing_Units+1) + Vacancy_Rate + Pct_Vacant_for_Sale + Mortgage + log(Kilometers+1)
             + log(Jobs_Public_Administration+1) + log(Jobs_Construction+1) + log(Jobs_Manufacturing+1) + log(Jobs_Wholesale+1) + log(Jobs_Retail+1)
             + log(Jobs_Transportation+1) + log(Jobs_Information+1) + log(Jobs_Finance+1) + log(Jobs_Professional+1) + log(Jobs_Social_Work+1)
             + log(Jobs_Amenities+1) + log(Jobs_Other+1), data = votes[which(is.na(votes$Republican) == FALSE),])

model4 <- lm(Turnout_Ratio ~ Y + log(R_Votes+1) + log(D_Votes+1) + log(L_Votes+1) + log(Population+1)
             + VAP_Black + VAP_Asian + VAP_Hispanic + VAP_White + Age + Men + Pct_Children + Townies
             + Pct_Commute_By_Car + log(Income+1) + Education + Unemployment_Rate
             + log(Housing_Units+1) + Vacancy_Rate + Pct_Vacant_for_Sale + Mortgage + log(Kilometers+1)
             + log(Jobs_Public_Administration+1) + log(Jobs_Construction+1) + log(Jobs_Manufacturing+1) + log(Jobs_Wholesale+1) + log(Jobs_Retail+1)
             + log(Jobs_Transportation+1) + log(Jobs_Information+1) + log(Jobs_Finance+1) + log(Jobs_Professional+1) + log(Jobs_Social_Work+1)
             + log(Jobs_Amenities+1) + log(Jobs_Other+1), data = votes[which(is.na(votes$Republican) == FALSE),])

# Calculating heteroskedasticity-robust standard errors

model3.robust <- coeftest(model3, vcov = vcovCL(model3, type = 'HC0'))
model4.robust <- coeftest(model4, vcov = vcovCL(model4, type = 'HC0'))

# Viewing the results

stargazer(model3.robust, model4.robust, type = 'text')

# Saving the results

write.csv(stargazer(model3.robust, model4.robust, type = 'text'), paste0(direc, 'results/text.txt'), row.names = FALSE)
write.csv(stargazer(model3.robust, model4.robust), paste0(direc, 'results/latex.txt'), row.names = FALSE)

# How does this vary across the nation (get data) - if so, this still doesn't HAVE TO become a full length article

# can i qualtrics these people and look not just at WTP, but also at if they voted YES or NO (or abstained) and why they did so ??

