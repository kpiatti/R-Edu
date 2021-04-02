#load pkgs and data----

library(tidyverse)
library(here)
#ran in console--install.packages("ggbeeswarm")
library(ggbeeswarm)

#read in data
plotbeaches <- read.csv(here("Data", "cleanbeaches_new.csv"))

#to change default plot theme for entire script (default theme = theme_grey())
theme_set(theme_classic())

#to change plot colors using pallettes
library(RColorBrewer)

######## POINT/SCATTER PLOTS----

#INVESTIGATIVE QUESTION: what are the bug levels like from year to year?

#use ggplot to create point plot of beachbugs by year
plotbeaches %>% 
  ggplot(aes(x = year, y = beachbugs)) +
  geom_point()
# when x is cat var, plot will have bars of points for each cat


#get summary of how many obs there are per year
plotbeaches %>% 
  group_by(year) %>% 
  summarize(obs = n())



####### JITTER PLOT #######

# points with same value will be spread out horizontally

#create jitter plot of beachbugs by year
plotbeaches %>% 
  ggplot(aes
(x = year, y = beachbugs)) +
  geom_jitter()


############ QUASI-RANDOM PLOT #######

# beachbug data looks like upside down T--b/c most obs value near O

plotbeaches %>% 
  ggplot(aes(x = year, y = beachbugs)) +
  geom_quasirandom()



#plot buglevels by site
plotbeaches %>% 
  ggplot(aes(x = site, y = beachbugs)) +
  geom_jitter()


######## FLIP COORDINATES----

#use when labels on y-axis too crowded

#add command to above plot to flip x and y cordinates
plotbeaches %>% 
  ggplot(aes(x = site, y = beachbugs)) +
  geom_jitter() +
  coord_flip()


######## COLORIZE DATA POINTS (REP 3RD VAR)----

#INVESTIGATIVE QUESTION: from our last plots, doesn't look like any change in bug levels across all beaches from year to year. is there any change wrt particular beaches?

plotbeaches %>% 
  ggplot(aes(x = site, y = beachbugs, color = year)) +
  geom_jitter() +
  coord_flip()
#plot not look as expected b/c year is being treated as continuous var. 

#change dtype

#glimpse to check var dtypes
glimpse(plotbeaches)

#permanetly change year dtype from int to factor
plotbeaches$year <- as.factor(plotbeaches$year)

#glimpse to verify change
glimpse(plotbeaches)

#re-plot data, x=site, y=bugs, color=year
plotbeaches %>% 
  ggplot(aes(x = site, y = beachbugs, color = year)) +
  geom_jitter() +
  coord_flip()

#see council perform ovr time: x=site, y=bugs, color=year
plotbeaches %>% 
  ggplot(aes(x = year, y = beachbugs, color = council)) +
  geom_jitter() +
  coord_flip()


######## OMIT NAs----

#remove NAs to stop warning message about removing obs w/ NA

#re-plot council perform ovr time w/ month
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = month, y = beachbugs, color = council)) +
  geom_jitter() +
  coord_flip()



####### PLOT GRIDS w/ FACET_WRAP----

#use facet_wrap to create grids of multiple side-by-side plots to represent additional vars

#clk zoom in plot panel to see bigger window of plot grid


plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = year, y = beachbugs)) +
  geom_jitter() +
  facet_wrap(~ site)

#use color and facet_wrap to rep 3rd and 4th vars
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = year, y = beachbugs, color = month)) +
  geom_jitter() +
  facet_wrap(~site)

#change month dtype to factor
plotbeaches$month <- as.factor(plotbeaches$month)

#re-run plot
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = year, y = beachbugs, color = month)) +
  geom_jitter() +
  facet_wrap(~site)


######## FILTER DATA THEN PLOT----

#plot only obs if beachbugs is less than 1,000
plotbeaches %>% 
  na.omit() %>% 
  filter(beachbugs < 1000) %>% 
  ggplot(aes(x=year, y=beachbugs, color=site)) +
  geom_jitter() +
  facet_wrap(~site)

#plot obs for cogee and bondi where bugs < 1,000
plotbeaches %>% 
  na.omit() %>% 
  filter(beachbugs < 1000) %>% 
  filter(site %in% c("Coogee Beach", "Bondi Beach")) %>% 
  ggplot(aes(x=year, y=beachbugs, color=site)) +
  geom_jitter() +
  facet_wrap(~site)



########### SAVE PLOT----

#option 1: use export button in plots pane

#option 2: use ggsave and here
ggsave(here("output", "coogee_bondi.png"))



######## GGPLOT HISTOGRAMS----

#create hist of beach sites (categorical var)
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x=site))+
  geom_histogram(stat = "count")

#create histogram of # of obs per council
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x=council))+
  geom_histogram(stat="count")

#create histogram to compare # of obs that were buggier than avg for that site
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x=buggier_site))+
  geom_histogram(stat="count")

#create histogram to compare # of obs that were buggier than avg for all sites
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x=buggier_all))+
  geom_histogram(stat="count")


######## BASE GRAPHICS HIST #######

#can use for var  with numeric values 
hist(plotbeaches$beachbugs)



####### ALTER BINWIDTH #########
plotbeaches %>% 
  na.omit() %>% 
  filter(site == "Coogee Beach",
         year == "2018", 
         logbeachbugs > 0) %>% 
  ggplot(aes(x = logbeachbugs)) +
  geom_histogram(binwidth = 0.5)


######### BOX AND VIOLIN PLOTS----

#create box plot of bugs by year
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x=year, y=beachbugs))+
  geom_boxplot()
#can't see boxes b/c outliers/distribution not normal, fix using log transformed beachbugs var. we created in earlier lesson


#re-plot using log tranformed beachbugs var
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x=year, y=logbeachbugs))+
  geom_boxplot()

#re-plot as violin plot
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x=year, y=logbeachbugs))+
  geom_violin()

#re-plot filtering for obs that are greater than mean of beachbugs for that site (meanbugs is true)
plotbeaches %>% 
  na.omit() %>% 
  filter(buggier_site == "TRUE") %>% 
  ggplot(aes(x=year, y=logbeachbugs))+
  geom_violin()

#re-plot with facet for each site
plotbeaches %>% 
  na.omit() %>% 
  filter(buggier_site=="TRUE") %>% 
  ggplot(aes(x=year, y=logbeachbugs))+
  geom_violin()+
  facet_wrap(~site)



######## COLOR OPTIONS ######

#re-plot adding color to violin outlines
plotbeaches %>% 
  na.omit() %>% 
  filter(buggier_site == "TRUE") %>% 
  ggplot(aes(x=year, y=logbeachbugs, color=site))+
  geom_violin()+
  facet_wrap(~site)

#re-plot adding color to fill violins
plotbeaches %>% 
  na.omit() %>% 
  filter(buggier_site == "TRUE") %>% 
  ggplot(aes(x=year, y=logbeachbugs, color=site, fill=site))+
  geom_violin()+
  facet_wrap(~site)

#re-plot filling color by year
plotbeaches %>% 
  na.omit() %>% 
  filter(buggier_site == "TRUE") %>% 
  ggplot(aes(x=year, y=logbeachbugs, color=year, fill=year))+
  geom_violin()+
  facet_wrap(~site)



########## COMBO PLOTS #########

#it is also possible to layer different types of geom plots of top of one another to convey more information

#combo box and point of logbeachbugs by site, color =year
plotbeaches %>% 
  filter(buggier_site == "TRUE") %>% 
  ggplot(aes(x = site, y = logbeachbugs)) +
  geom_boxplot() +
  geom_point(aes(color = year)) +
  coord_flip()


# combo violin and quasi-random of logbeaches by year at Clovelly beach
plotbeaches %>% 
  filter(site == "Clovelly Beach") %>% 
  ggplot(aes(x= year, y = logbeachbugs)) +
  geom_violin() +
  geom_quasirandom(aes(color = buggier_site))


######## BAR % COLUMN PLOTS ########
 
#to plot the frequency/count of a single var use geom_bar--ggplot will assume you want counts if you just tell ggplot what var to use on the x-axis. To create bar chart using 2 vars use geom_col

#plot number of observation per year at each site
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = year)) + 
  geom_bar() +
  facet_wrap(~site)

#plot beachbugs by year
plotbeaches %>% 
  na.omit() %>%
  ggplot(aes(x = year, y = beachbugs)) +
  geom_col()

#check sum of beachbugs by year to check what values are being represented on y-axis in the above plot
plotbeaches %>% 
  na.omit() %>% 
  group_by(year) %>% 
  summarise(total_bugs = sum(beachbugs))

#to plot beachbugs each year for each site
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = year, y = beachbugs)) +
  geom_col() +
  facet_wrap(~site)



####### PLOT SUMMARY STATS #########

#to plot a summary stat along the y axis, calculate the summary stat then pipe it into ggplot

#create sub-plots of each site, showing the mean of beachbugs each year for that site
plotbeaches %>% 
  na.omit() %>%
  group_by(year, site) %>% 
  summarise(meanbugs = mean(beachbugs)) %>% 
  ggplot(aes(x = year, y = meanbugs)) +
  geom_col() + 
  facet_wrap(~ site)
#adding facet wrap didn't work 1st time b/c had grouped by year only, had to group by year and site to have meanbugs by year for each site before i could plot that info using facet_wrap. 



######## ADD ERROR BARS #######

# to add error bars to a plot showing summary stat (e.g. mean) you first do a summmarise to get the mean, std, count(n = n(), and standard error) then you pipe that into a geom_col plot and add a geom_error plot
plotbeaches %>% 
  na.omit() %>% 
  group_by(site) %>% 
  summarise(mean = mean(beachbugs),
            sd = sd(beachbugs),
            n = n(),
            stderr = sd/sqrt(n)) %>% 
  ggplot(aes(x = site, y = mean)) +
  geom_col() +  #to make the bars
  coord_flip() +   # to make x-axis labels readable
  geom_errorbar(aes(x = site, ymin = mean-stderr, ymax = mean+stderr)) 
#last line add the error bars to the top/end of each column


########## PLOT CORRELATIONS #######

# added weather data b/c not 2 vars that might be corelated in the beachbugs data, so add weather data to see if beachbug levels are correlated with weather
raintemp <- read.csv(here("Data", "rain_temp_beachbugs.csv"))

#to plot correlation b/t vars, create scatter plot (geom_point) and add a geom_smooth layer to show a best fit regeression line with shaded error area around line

#plot rain levels (x) and bug levels (y)
raintemp %>% 
  ggplot(aes(x = rain_mm, y = beachbugs)) +
  geom_point()


#add a best fit regression line through the data
raintemp %>%
  filter(beachbugs > 500) %>% 
  ggplot(aes(x=rain_mm, y = beachbugs)) +
  geom_point() +
  geom_smooth()
#filtered out obs. with beachbug values below 500 to make regression line through data easier to see--would not typically do that in a real project


#colorize points to represent temp var on the above plot
raintemp %>% 
  filter(beachbugs > 500) %>% 
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth()



########## CHANGE BACKGROUND ########

# the default bckgrd (light grey w/ grid lines) is theme_grey

#to change background of all plots in your script add theme_set(name_of_prefered_theme()) to the top of your script


#to change bckgrd of single plot add theme_set() line to ggplot code
raintemp %>% 
  filter(beachbugs > 500) %>% 
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth() +
  theme_set(theme_classic())




###################### CHANGE COLORS----

# 2 options: add another layer to scale_color_gradient, option 2: use palettes (pkg RColorBrewer) and scale_color_distiller. there are many more scale_color options available. 

#change data point colors so cold temps are blue and high temps are red using option 1
raintemp %>% 
  filter(beachbugs > 500) %>% 
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth() +
  theme_set(theme_classic()) +
  scale_color_gradient(low = "blue", high = "red")

#set color using option 2 (requires pkg RColorBrewer)
#to show names of available color palettes 
display.brewer.all()


raintemp %>% 
  filter(beachbugs > 500) %>% 
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth() +
  theme_set(theme_classic()) +
  scale_color_distiller(palette = "RdYlBu")

#when using color to represent discrete/categorical vars use scale_color_brewer
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = year, y = beachbugs, color = year)) +
  geom_col() +
  facet_wrap(~site) +
  scale_color_brewer(palette = "Set2")
#this plot required changing dtype of year var to factor before it would execute, using color to represent year doesn't make sense in this plot. 




########## ADD TITLES & LABELS ########

raintemp %>% 
  filter(beachbugs > 500) %>% 
  ggplot(aes(x = rain_mm, 
             y = beachbugs, 
             color = temp_airport)) +
  geom_point() +
  geom_smooth() +
  theme_set(theme_classic()) +
  scale_color_distiller(palette = "RdYlBu") +
  labs(title = "Plot Showing How Beachbug Levels Vary \n by  Rainfall and Temperature \nat Australian Beaches `2014-2018",
       subtitle = "eccocenti levels",
       caption = "https://rladiessydney.org/courses/ryouwithme/03-vizwhiz-4/",
       x  = "Amount of Rainfall (mm)",
       y= "Bug Level (count)")



############## ADD A GIF ###########

#see instructionis in vizwiz 5 from RLadies Sydney
# https://rladiessydney.org/courses/ryouwithme/03-vizwhiz-5/




























