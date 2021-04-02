#get info/help-----
#1. for pkgs- CRAN pkg docs & vignettes
#2. Google it
#3. search twitter #rstats


#load packages-----
library(tidyverse)
library(here)
library(janitor)
library(skimr)


#read in data----

#assign  new beaches object, use the tidyverse pkg to read csv file & here pkg to tell r where csv file is located w/i the project file
beaches <- read.csv(here("data", "sydneybeaches.csv"))

#explore the data----

#returns spreadsht,or click on beaches obj in enviro
view(beaches)

#returns shape of df
dim(beaches)

#returns type of data in each column
str(beaches)

#returns same info as str with better format (dbl=num)
glimpse(beaches)

#returns first 6 rows of data
head(beaches)

#returns last 6 rows of data
tail(beaches)

#returns summary stats of each var
summary(beaches)

#returns shape and summary stats, req. skimr pkg
skim(beaches)



#tidy columns----

#get column names
glimpse(beaches)

#select all columns and rename, using dyplr pkg (in tidyverse)
select_all(beaches, toupper) #all uppercase
select_all(beaches, tolower) #all lowercase

#clean names (e.g. add underscores), req. janitor pkg
clean_names(beaches)

#just return column names
names(beaches)

#in output names do not have formatting just applied
#like in python, you must assign new object to retain 
cleanbeaches  <- clean_names(beaches)

names(cleanbeaches)

#rename specified column, use new name = old name, overrides previous cleanbeaches
cleanbeaches <- rename(.data = cleanbeaches, beachbugs = enterococci_cfu_100ml)

names(cleanbeaches)

#select subset & re-order columns----

#select subset of columns
select(cleanbeaches, council, site, beachbugs)

#reorder columns
select(cleanbeaches, council, site, beachbugs, everything())

#pipe data----

#keyboard shortcut to add pipe command is ctrl+shift+m
#using pipe function from magrittr pkg enables you to string several functions together and perform them sequentially

cleanbeaches <- beaches %>%
  clean_names() %>%
  rename(beachbugs = enterococci_cfu_100ml)
#  select(site, council, beachbug)--removed this line to get entire df with clean names



#export data----

#export cleaned data to new .csv file
write_csv(cleanbeaches, "cleanbeaches.csv")
#how do i put or move output to data folder inside this project?



#arrange & filter data----

#which beaches have the worst bugs?

#sort data on beachbugs, descending order
worstbeaches <- cleanbeaches %>%
  arrange(desc(beachbugs)) #instead of desc(beaches), can use -beaches

#returns worst bugs for coogee beach only
worstcoogee <- cleanbeaches %>%
  filter(site == "Coogee Beach") %>%
  arrange(-beachbugs)
#how do i remove one object from my environment?

worstmalabar <- cleanbeaches %>%
  filter(site =="Malabar Beach") %>%
  arrange(-beachbugs)



#filter using 2 or more variables

#investigative question: does coogee or bondi beach have more extreme bacteria levels? to answer: first filter to get only coogee and bondi obs., then get summary stats on each beach

#returns just data in filter
coogee_bondi <- cleanbeaches %>%
  filter(site %in% c("Coogee Beach", "Bondi Beach")) %>%
  arrange(desc(beachbugs))



#summary stats----

#returns summary stats on coogee and bondi
sum_coogee_bondi <- cleanbeaches %>%
  filter(site %in% c("Coogee Beach", "Bondi Beach")) %>%
  group_by(site) %>% #w/o returns single row summary stats on combo
  summarise(maxbugs = max(beachbugs, na.rm = TRUE),
            meanbugs = mean(beachbugs, na.rm = TRUE),
            medianbugs = median(beachbugs, na.rm = TRUE),
            sdbugs = sd(beachbugs, na.rm = TRUE)) 
#summarise function doesn't work if there are missing values in the data, add 'na.rm = TRUE' to ignore missing values

#return summary stats on all beaches 
sum_all_beaches <- cleanbeaches %>%
  group_by(site) %>% 
  summarise(maxbugs = max(beachbugs, na.rm = TRUE),
            meanbugs = mean(beachbugs, na.rm = TRUE),
            medianbugs = median(beachbugs, na.rm = TRUE),
            sdbugs = sd(beachbugs, na.rm = TRUE)) %>%
  arrange(-meanbugs)


#more groupby----

# investigative question: which council does worst at keeping their beaches clean?

#summary stats for data grouped by site
cleanbeaches %>%
  group_by(council) %>%
  summarise(meanbugs = mean(beachbugs, na.rm = TRUE),
            medianbugs = median(beachbugs, na.rm = TRUE))

#summary stats for data grouped by site and council
cleanbeaches %>% 
  group_by(council, site) %>% 
  summarise(meanbugs = mean(beachbugs, na.rm = TRUE),
            medianbugs = median(beachbugs, na.rm = TRUE))



#new vars w/ separate & unite----

#create new var & keep existing var by separating values in date column
cleanbeaches %>% separate("date", c("day", "month", "year"), remove = FALSE)

#create new var to replace date var 
cleanbeaches %>% separate("date", c("day", "month", "year"))

#replace existing var with combo var
cleanbeaches %>% unite(council_site, council:site)

#create new combo var and keep existing
cleanbeaches %>% unite(council_site, council:site, remove = FALSE)



#NEW VARS W/ mutate----

#mutate is most common way to create new vars.

#use mutate to log transform beachbugs var--if vars have extreme values, good idea to log transform values so vars will have something appox. a normal distribution
cleanbeaches %>% mutate(logbeachbugs = log(beachbugs))


#use mutate to lag transform beachbugs var--lag function takes each value in selected var, subtracts the previous value from it--thus after lag is used the first entry is NA
cleanbeaches %>% mutate(beachbugsdiff = lag(beachbugs))
#what would use lag for?


#use mutate to compute a new logical var (buggier) that is TRUE if value is > mean, FALSE if value is < mean
cleanbeaches %>% mutate(buggier = beachbugs > mean(beachbugs, na.rm = TRUE)) #reminder must remove NAa


#to check that previous command did what we wanted, calculate mean of beachbugs, then look at values to see if lower or higher 
meanbugs = mean(cleanbeaches$beachbugs, na.rm = TRUE)

#use groupby and mutate to compute new log var that is true if value is > mean of beachbugs for that beach
cleanbeaches %>% 
  group_by(site) %>% 
  mutate(buggier_site = beachbugs > mean(beachbugs, na.rm = TRUE))

#to retain new vars, add new vars to new object at once using piping
cleanbeaches_new <- cleanbeaches %>% 
  separate("date", c("day", "month", "year")) %>%
  mutate(logbeachbugs = log(beachbugs)) %>% 
  mutate(beachbugsdiff = lag(beachbugs)) %>% 
  mutate(buggier_all = beachbugs > mean(beachbugs, na.rm = TRUE)) %>% 
  group_by(site) %>% 
  mutate(buggier_site = beachbugs > mean(beachbugs, na.rm = TRUE))


#export data to .csv----
write_csv(cleanbeaches_new, here("Data", "cleanbeaches_new.csv"))





























