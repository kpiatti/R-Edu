#load pkgs and data
library(tidyverse)
library(here)

bakers_wide <- read.csv(here("Data", "bakers_wide.csv"))


#use pivot_longer to make wide data long
bakers_long <- bakers_wide %>% 
  pivot_longer(names_to = "spice", values_to = "correct", cinnamon_1:nutmeg_3)



beachbugs_wide <- read.csv(here("Data", "beachbugs_wide.csv"))

#use pivot_longer to reshape wide dataset into long format
beachbugs_long <- beachbugs_wide %>% 
  pivot_longer(names_to = "beaches", values_to = "beachbugs", Bondi.Beach:Tamarama.Beach)

#use pivot_wider to reshape long data into wide data
beachbugs_long %>% 
  pivot_wider(names_from = beaches, values_from = beachbugs)



#reshaping more complex dataset from wide to long----

#read in csv
frames <- read.csv(here("Data", "frames_wide.csv"))

view(frames)

#return list of var names
names(frames)

#reshape data from wide to long
frames %>% pivot_longer(names_to = c("size", "item"),
                        values_to = "response",
                        large_item1:small_item7,
                        names_sep = "_") #tell R what symbol to separate 





