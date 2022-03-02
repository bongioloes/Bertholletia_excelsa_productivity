<div id="top"></div>

<div align="center">
  <h1 align="center">Brazil nut productivity estimates in secondary forest</h1>

  <p align="center">
    Master thesis presented at the University of Florida - School of Forest Resources and Conservation
    <br />
    <a href="http://dx.doi.org/10.1016/j.foreco.2020.118019"><strong>Check out the full paper </strong></a>
    <br />
   </p>
</div>


<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#introduction">Introduction</a>
    </li>
    <li><a href="#data">Data</a></li>
    <ul>
        <li><a href="#secondary">Secondary</a></li>
        <li><a href="#primary">Primary</a></li>
    </ul>
    <li><a href="#individual-based-model">Individual-based Model</a></li>
    <li><a href="#pos-model-data-management-and-plotting">Pos-model data management and plotting</a></li>
    <ul>
        <li><a href="#basal-area">Basal Area</a></li>
        <li><a href="#fruit-prodcutivity">Fruit prodcutivity</a></li>
        <li><a href="#mapping">Mapping</a></li>
    </ul>
  </ol>
</details>

## Introduction

  <p> Bellow is a summary of all the programming that was done to my Master Thesis. 
      I can't provide the data and go into more details in methods and results due to the paper that was written about this project.
  </p>
  <p> What I will be presenting here is the code used to produce the results and the images and graphs gerated.
  This include some graphs that were used in the paper and presentations, the individual-based model and the data management around this.
  </p>
  <p align="right">(<a href="#top">back to top</a>)</p>

## Data

### Secondary
```R
# Original R file is "BN_price.R"


## created 19/11/2018 to ilustrate brazil nut price change over time.
# prodcution and average price of the north region of Brazil was obtain at https://sidra.ibge.gov.br/Tabela/289 
# from 1994 to 2017 in tonnes
# to canculate price in dolars historical currency exchange rate were obtain at https://www.oanda.com/currency/average

dat <- read.csv("data/price_change/prod_price_currency.csv",as.is = T)
library(ggplot2)
library(dplyr)

#### ---- Price per fruit ----####
dat_fruits <-dat %>%
  select(Ano,rs_fruit, us_fruit) %>% 
  reshape2::melt(id.vars = "Ano")
dat_fruits_2017 <-dat_fruits %>% 
  filter(Ano == 2017) %>% 
  mutate(value = round(value, digits = 3))

# values per fruit
bnut_fruit_price_title <- expression(paste("Average price per fruit of ", italic("B. excelsa"), " nuts before drying"))

bnut_fruit_price <- ggplot(dat_fruits, aes(x = Ano))+
  geom_line(aes(y= value,col=variable))+
  geom_point(aes( y= value, col = variable))+
  labs(y="Average price per fruit", 
       color=NULL) +  # title and caption
  scale_x_continuous(name = "Year",
                   breaks = c(1994,1998,2002,2006,2010,2014,2017),
                   label = c(1994,1998,2002,2006,2010,2014,2017))+
  scale_color_manual(name = "Currency",
                     values = (wesanderson::wes_palette(n=4, name= "GrandBudapest1"))[3:4],
                     labels = c("R$","U$"))+
  geom_label(data = dat_fruits_2017,
             aes(y=value,label = value),
             label.r = unit(0.15, "lines"),
             nudge_x = -0.65,
             nudge_y = 0.005)+
  ggtitle(bnut_fruit_price_title)+
  theme(axis.text.x = element_text(vjust=0.5, size = 9),
        plot.title = element_text(family="Comic Sans MS", hjust = 0.5, size = 15))

# ggsave(filename ="output/bnut_fruit_price.png",plot = bnut_fruit_price,dpi = 300, width = 25, height = 12, units = c("cm"))
bnut_fruit_price
```

<br />
<p align="center">
<img src="https://user-images.githubusercontent.com/38635706/156423538-ab776f2a-a1bd-4638-8ed8-f1ba6a2bf18b.png" alt="Logo" height="450">
</p>
<br />

```R
#### ---- Price per lata ----####
dat_lata <-dat %>%
  select(Ano,rs_can, us_can) %>% 
  reshape2::melt(id.vars = "Ano")
dat_lata_2017 <-dat_lata %>% 
  filter(Ano == 2017) %>% 
  mutate(value = round(value, digits = 2))

# values per fruit

bnut_lata_price_title <- expression(paste("Average price for a can of unshelled ", italic("B. excelsa"), " nuts before drying"))

bnut_lata_price <- ggplot(dat_lata, aes(x = Ano))+
  geom_line(aes(y= value,col=variable))+
  geom_point(aes( y= value, col = variable))+
  labs(y="Average price per lata (~11kg)", color=NULL) +  # title and caption
  scale_x_continuous(name = "Year",
                     breaks = c(1994,1998,2002,2006,2010,2014,2017),
                     label = c(1994,1998,2002,2006,2010,2014,2017))+
  scale_color_manual(name = "Currency",
                     values = (wesanderson::wes_palette(n=4, name= "GrandBudapest1"))[3:4],
                     labels = c("R$","U$"))+
  geom_label(data = dat_lata_2017,
             aes(y=value,label = value),
             label.r = unit(0.15, "lines"),
             nudge_x = -0.65,
             nudge_y = 0.005)+
  ggtitle(bnut_lata_price_title)+
  theme(axis.text.x = element_text(vjust=0.5, size = 9),
        plot.title = element_text(family="TT Arial", hjust = 0.5, size = 15))

# ggsave(filename ="output/bnut_lata_price.png",plot = bnut_lata_price,dpi = 300, width = 25, height = 12, units = c("cm"))
bnut_lata_price
```

<br />
<p align="center">
<img src="https://user-images.githubusercontent.com/38635706/156423447-78d28633-4ba4-41e6-8b7e-d2faeda196e0.png" alt="Logo" height="450">
</p>
<br />

### Primary
```R
# Original R file is "Field Sheet.R"


# Looking into the data inventoried and summaring into fallows

dat = read.csv('data/Field_sheet.csv',as.is = T)
library(ggplot2)
library(dplyr)
library(wesanderson)
library(ggmosaic)
# this is the fallows code in age order, from youngest to older.
dat$Cap<-factor(dat$Cap, levels=c('J','G','L','C','E','M','A','Q','B','D','I','P','K','F','S','R','O','N'))
Crown_Position<-factor(dat$Crown_position, levels=c("Dominant","Co-Dominant","Intermediate","Suppressed"))

####------Grouping data into fallows------####
# calculate sapling density
sapling_density_df <- dat %>% 
  filter(DBH <10) %>% 
  group_by(Cap) %>%
  summarize(Density_saplings = mean(n()/Fallow_Area))

# summarize info of fallows
dat_fallow <- dat %>%
  group_by(Cap) %>% 
  summarize(Area = mean(Fallow_Area),
            Age = mean(Fallow_Age),
            Age_Category = Age_Category[1],
            Density_overall = mean(Fallow_density),
            Count_tree = n()) %>% 
  mutate(Density_saplings = sapling_density_df$Density_saplings)

# calculate mean and sd of fallows
means_and_sd <-as.data.frame(sapply(dat_fallow, function(cl) list(means=mean(cl,na.rm=TRUE), sds=sd(cl,na.rm=TRUE))))


#### --------------- Fallow Age --------------- ####

wilcox_Age_DBH<- wilcox.test(dat$Fallow_Age,dat$DBH)
#wilcox_Age_DBH

##  Wilcoxon rank sum test with continuity correction
## 
## data:  dat$Fallow_Age and dat$DBH
## W = 57433, p-value < 2.2e-16
## alternative hypothesis: true location shift is not equal to 0


DBH_dis <- ggplot(data = dat,mapping= aes(x = Cap,y = dat$DBH, fill = dat$Age_Category))+
  geom_boxplot()+
  ylab("DBH (cm)")+
  xlab("Fallow in age order, from youngest (12 yrs) to oldest (60 yrs)")+
  scale_fill_manual(values=c('#396ab1',"#da7c30","#3e9651","#922428"),
                    #cbPalette[1:5],
                    # wes_palette(n = 4, name = "GrandBudapest1"),
                    #c('#396ab1',"#da7c30","#3e9651","#922428"),
                    name="Fallow Age\nClass (years)", #HOW TO centralize titles and legend titles
                    labels= c("12 - 17","20 - 22","26 - 29","48 - 60"))+
  ggtitle("DBH distribution per fallow arranged in fallow age order")+
  ggplot2::theme(legend.text.align=1,
                 legend.title.align = 0.5,
                 axis.text.x = element_text(size = 11,
                                            color = c(rep('#396ab1',6),
                                                      rep("#da7c30",6),
                                                      rep("#3e9651",4),
                                                      rep("#922428",2))),
                 plot.title = element_text(family="TT Arial", hjust = 0.5, size = 15))

# ggsave(filename ="output/DBH_dis.png",plot = DBH_dis,dpi = 300, width = 20, height = 12, units = c("cm"))
DBH_dis
```

<br />
<p align="center">
<img src="https://user-images.githubusercontent.com/38635706/156423387-5c50c468-d5ec-4b0e-b080-b13d71ac7de3.png" alt="Logo" height="450">
</p>
<br />

```R
### --------------- Crown Position Graph --------------- ####

crown_pos_df <-dat %>% 
  select(Fallow_Age, DBH, Crown_position) %>%
  mutate(Age_Category = ifelse(Fallow_Age <=17, "12-17 (yrs)",
                               ifelse(Fallow_Age <=22, "20-22 (yrs)",
                                      ifelse(Fallow_Age <=29, "26-29 (yrs)",
                                             ifelse(Fallow_Age >=48, "48-60 (yrs)", NA)))),
         DBH_class = ifelse(DBH <10, "0-10",
                            ifelse(DBH <20, "10-20",
                                   ifelse(DBH <30, "20-30",
                                          ifelse(DBH <40, "30-40",
                                                 ifelse(DBH >=40, ">40",NA)))))) %>% 
  select(Age_Category,DBH_class, Crown_position)
# crown_pos_df$Age_Category<-as.factor(crown_pos_df$Age_Category)
# crown_pos_df$DBH_class <-as.factor(crown_pos_df$DBH_class)                                            
# crown_pos_df$Crown_position <- as.factor(crown_pos_df$Crown_position)

# percent <- c("0%", "25%", "50%", "75%", "100%")

crown_pos_graph <- ggplot(data = crown_pos_df,aes(x = factor(DBH_class, 
                                                             levels = c("0-10","10-20","20-30","30-40",">40")), 
                                                  fill = factor(Crown_position, 
                                                                levels = c("Dominant","Co-Dominant","Intermediate","Suppressed"))))+
  geom_bar(position = "fill", stat = "count")+
  scale_y_continuous(name = element_blank(),
                     na.value = element_blank(),
                     labels= c("0%", "25%", "50%", "75%", "100%"))+
  scale_x_discrete(name = element_blank())+#DBH classes")+
  scale_fill_manual(values = wes_palette("Moonrise2",4 ,type = "discrete"),#Darjeeling2 Moonrise2
                    name = "Crown Position",
                    breaks=c("Suppressed","Intermediate","Co-Dominant","Dominant"))+
  facet_grid(~Age_Category,switch = "x")+#,nrow = 1, ncol = 4)
  ggtitle("Crown position (suppressed, intermediate, co-dominant and dominant) \n of Brazil nut by DBH class (cm) in the four fallow age classes.")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        # legend.text = element_text(debug=T),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(family="TT Arial", hjust = 0.5, size = 13))



# ggsave(filename ="output/crown_pos_graph.png",
#        plot = crown_pos_graph,dpi = 300,
#        width = 20,
#        height = 12,
#        units = c("cm"))
crown_pos_graph
```

<br />
<p align="center">
<img src="https://user-images.githubusercontent.com/38635706/156423306-ccd0309c-b796-4718-89c4-5f0691419b46.png" alt="Logo" height="450">
</p>
<br />

```R
#### --------------- Number of competitors --------------- ####
# ----- Spearman - DBH ~ Number of competitors ----- #
spearman_cor_DBH_comp <- cor.test(dat$DBH,dat$Number_Comp,method = "spearman")
#spearman_cor_DBH_comp

## 
##  Spearman's rank correlation rho
## 
## data:  dat$DBH and dat$Number_Comp
## S = 2978100, p-value = 0.02316
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## -0.1435918


#point plot of DBH(cm) vs numbers of competitors using Fallow agesfor colors
# Same as the first graph, but uses Fallow ages classes to colored points
Age = as.factor(dat$Age_Category)
plot_DBH_N_comp <- ggplot(data = dat)+
  geom_point(mapping= aes(x = DBH, y = Number_Comp, color=Age), size = 3) +
  geom_smooth(se=TRUE,aes(x = DBH, y = Number_Comp, linetype = ""),color = "black", method=lm, fullrange=TRUE) +
  ylab("Number of competitors") +
  xlab("DBH (cm)") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  scale_color_manual(name = "Fallow Age\nClass (years)",
                     values = c('#396ab1',"#da7c30","#3e9651","#922428"),
                     labels= c("12 - 17","20 - 22","26 - 29","48 - 60"))+
  scale_linetype_manual(values = 1,
                        name = "Trendline")+
  ggtitle("DBH and number of competitors to every tree in the data set,\n colored by fallow age class.")+
  theme(legend.text.align=1,
        legend.title.align = 0.5,
        plot.title = element_text(family="TT Arial", hjust = 0.5, size = 12))

# ggsave(filename ="output/plot_DBH_N_comp.png",plot = plot_DBH_N_comp,dpi = 200,width = 20 ,height = 12, units = c("cm"))
plot_DBH_N_comp
```

<br />
<p align="center">
<img src="https://user-images.githubusercontent.com/38635706/156423262-b1243619-0ad9-4f33-b438-6a17847219fd.png" alt="Logo" height="450">
</p>
<br />

```R
# ----- kruskal.test - Crown Position~ Number of competitors ----- #
stast_test_for_comp_vs_crown_pos <- kruskal.test(Number_Comp ~Crown_Position, data=dat)
stast_test_for_comp_vs_crown_pos

# this is a way to graphically represent the Crown position vs Number of competitiors
b = Crown_Position
a = dat$Number_Comp
mytable <- table(a,b)
crown_position_vs_competition_df<-mosaicplot(mytable,
            main = "",
            ylab = "Crown Position",
            xlab = "Number of competitors",
            las =1,
            border = NA,
            color = wesanderson::wes_palette(n = 4, "GrandBudapest1"))#c('#396ab1',"#da7c30","#3e9651","#922428"))
# ggsave(filename ="output/comp_N_comp_Crown_position.png",plot = crown_position_vs_competition_df,dpi = 300, width = 20, height = 16, units = c("cm"))
 
crown_position_vs_competition_df <- as.data.frame(Crown_Position) %>% 
   mutate(Number_comp = dat$Number_Comp)
ggplot()+
  geom_mosaic(aes(crown_position_vs_competition_df))
```

<p align="right">(<a href="#top">back to top</a>)</p>
<!-- ============================================================================================================================================ -->

<br />

## Individual-based Model
```Python
# Original Python file is located in ".\IBM_Model\IBM_FINAL_MODEL.py"
#
# In python, change model to create 4 files, one to each time period I want. But start the dataframe with some information already on it
# Like the firsts columns of the input csv file
#  ---------------- Importing Libraries ------------------  #
import random
random.seed()
import math
#import matplotlib.pyplot as plt
#from matplotlib.font_manager import FontProperties
import csv
#from collections import defaultdict
import pandas as pd
import copy
#  ---------------- Opening csv file and creating dictionary ------------------  #
pop_base = {} # create empy dictionary
with open('data/field_sheet_for_python.csv', mode='r') as infile: #open csv with data
    reader = csv.reader(infile)
    next(reader)#this ignore the headers
    for rows in reader: # select columns of the cvs file that are necessary for the model
        pop_base.update({int(rows[0]):   #    rows[0] = tree index     list-index
                        [rows[1],        #    rows[1] = tree code      0
                         float(rows[2]), #    rows[2] = DBH (cm)       1
                         int(rows[3]),   #    rows[3] = f              2
                         int(rows[4]),   #    rows[4] = cfgp           3
                         int(rows[5]),   #    rows[5] = cftp           4
                         int(rows[6]),   #    rows[6] = f1             5
                         int(rows[7]),   #    rows[7] = f2             6
                         int(rows[8]),   #    rows[8] = p              7
                         int(rows[9]),   #    rows[9] = v              8
                         str("Alive"),   #    set all trees to alive   9
                         float(0.0)]})   #    cumulative production    10
#  ---------------- Functions ------------------  #
def DBH(r,d,f,cfgp): #growth - for trees <10cm DBH Staudhammer et al (2013) and for trees >= 10cm DBH Bertwell et al (2017)
    if d<10:
        Basal_Area_Increment = -20.508 + 0.023*r + 0.702*d -8.873*f + 4.532*cfgp# -1.2939*cftp
        batc = 4/math.pi # batc = Basal area transformation constant
        try: # for some dbh+rain conditions the Bassal_area_increment would return negative causing an error when transforming BA to DBH below.
            NewDBH_10cm = math.sqrt(d**2+batc*Basal_Area_Increment) #transform BA in DBH
        except:
            NewDBH_10cm = d # in case of a error above, the DBH for the tree will continue the same for the following year
        if NewDBH_10cm >=10: # to avoid small trees (<10 cm DBH) to growth faster than larger tree, once the tree pass the 10 cm threshold it will have 10 cm in the following year.
            NewDBH = 10
        else:
            NewDBH = NewDBH_10cm
    else:
        NewDBH = (0.3841 + 0.000261*r -0.0000643*d**2 + 0.000000256*d**3)+d
    return NewDBH
def survival (d,f1,f2,p,v): #Survival (Bertwell et al, 2017)
    logitPFor = 3.713 - 0.010*d - 2.636*f1 - 1.086*f2 - 1.487*p + 0.607*v
    #this function was developed to calculate the survival over 14years, to calculate survival chance in one year we need to extract this info from the formula
    logitP = math.exp(logitPFor)
    p = logitP / (1.0+logitP)
    P = p**(1.0/14.0)
    return P
def production(DBH): #Prodcution (Bertwell et al, 2017)
    if DBH > 40:
        Annual_fruit_production = -192.1 + 5.799*DBH - 0.018*DBH**2
    else:
        Annual_fruit_production = 0
    return Annual_fruit_production
#  ---------------- Model ------------------  #
#  ---------------- Variables ------------------  #
#rain historical data
r = [1531.3,1975,1641.3,1795.4,1634.3,1859.4,1831.3,2103.5,1296.7,1114.8,933.2,1761.7,1721.3,2050.9,2126.1,2442.5,1711.6,2340.1,2156.2,2040.3,2012.4,2156.9,1687.8,1902.5,2180.4,2296.1,2182.2,1703.5,1926.9,1909.9,1797.1,2090.4,1751.7,2054.6,1766.2,1841.4,2253.1,1766.4,1619.4,2689.4,2046.3,2400.3,2012.4,1681.7,2244.5]
final_time = 40 # number of years we want to go into de future
runs = 1 #number of runs that the model will do
trees = pop_base.keys() #collect keys of the dictionary to be used as the columns names in the dataframes, corresponde to the tree's number
xyear = random.choice(range(len(r))) #randomlly select a value of the rain historical data to start
# this is the first step to create the dataframe that will store the results
keys_list_str = ['Run_time']
for tree in trees:
    keys_list_str.append(str(tree))
#  ---------------- Dataframes ------------------  #
#creates the dataframe (df) that will store data generated from the models
prod_cum_df = pd.DataFrame(columns = keys_list_str) #Cummulative production
dbh_df = pd.DataFrame(columns = keys_list_str) #growth
prod_df = pd.DataFrame(columns = keys_list_str) #production
for run in range(runs):
#    print (run) #so you can know were the model is
    pop = copy.deepcopy(pop_base) #set data base to initial values before each run
    pop_DBH = [] # set to empy before each run
    for t in range(final_time):
        pop_DBH = [] # set to empy before each run
        pop_DBH.append("DBH-{0}-{1}".format(run,t)) # this will be the header of the df once the list is transfered to there
        pro_cum = [] #cummulative production set to empy before each time
        pro_cum.append("Cum_P-{0}-{1}".format(run,t)) # this will be the header of the df once the list is transfered to there
        pro = [] #cummulative production set to empy before each time
        pro.append("Prod-{0}-{1}".format(run,t)) # this will be the header of the df once the list is transfered to there
        rain_amount = r[xyear%len(r)] #choose the r value base on the random year selected by xyear
        for tree in trees: # loop over each tree to check for survival and growth
            if pop[tree][9] != "Dead": #check if tree is still alive.
                if random.random() >= survival(pop[tree][1],pop[tree][5],pop[tree][6],pop[tree][7],pop[tree][8]): #o index number aqui é o do dic e não o do csv. Por isso q f1 está como 5 e não como 6.
                    pop[tree][9] = "Dead" #Change "Alive" to "Dead"
                    New_DBH = 0 #set DBH to zero
#                        del pop[tree]
                else:
                    if pop[tree][1]>40:
                        pop[tree][2] = 1 #when a tree is bigger than 40cm in DBH I am assuming that it will start to produce
                    New_DBH = DBH(rain_amount,pop[tree][1],pop[tree][2],pop[tree][3]) #calculate growth
                pop[tree][1] = New_DBH #save new DBH value
        xyear = xyear+1 # add one year, so the next rain in the list will be selected to the next interaction
        for tree in trees: #calculating production
            prod = production(pop[tree][1]) #calculate production for every tree
            cum_prod = prod+pop[tree][10] # sum fruit production with previous production to save cummulative production
            pop[tree][10] = cum_prod #save cummulative production for the next year
            pro_cum.append(cum_prod) #store cummulative production for the df
            pro.append(prod) #store production for the df
        if t == 9 or t == 19 or t == 29 or t == 39: #in order to minimaze the size of the data frame, only for years were recorded to the df (10,20,30 and 40)
#            print (t,run)
            for tree in trees: # saving DBH every 10 years to be written in the csv file
                Save_last_DBH = pop[tree][1]
                pop_DBH.append(Save_last_DBH) #store DBH for the df
            pop_DBH_df2 = pd.DataFrame([pop_DBH], columns = keys_list_str) #create df for this year
            dbh_df = dbh_df.append(pop_DBH_df2,ignore_index=True) # integrate previous df with the one that was created above.
            prod_cum_df2 = pd.DataFrame([pro_cum], columns = keys_list_str)
            prod_cum_df = prod_cum_df.append(prod_cum_df2,ignore_index=True)
            prod_df2 = pd.DataFrame([pro], columns = keys_list_str)
            prod_df = prod_df.append(prod_df2,ignore_index=True)
            
## ---- save the df to csv files
#dbh_df.to_csv('output/model_output_dbh_50r_10cmDBHlimits.csv', index = False)
#prod_cum_df.to_csv('output/model_output_cumulative_prod_50r_10cmDBHlimits.csv',index = False)
#prod_df.to_csv('output/model_output_prod_50r_10cmDBHlimits.csv',index = False)
## I transfered the files to a R project, as they were. Transformation were performed at the Rstudio environment.
```


<p align="right">(<a href="#top">back to top</a>)</p>
<!-- ============================================================================================================================================ -->
<br />

## Pos-model data management and plotting
### Basal Area
```R
# Original R file is "FINAL_growth.R"

dbh_dat = read.csv('data/model_output_dbh_50r_10cmDBHlimits.csv',as.is = T)
library(ggplot2)
# library(plyr)
# library(Rmisc) 
library(dplyr)
library(wesanderson)
library(reshape2)


####------Functions------####
#Function to subset the data in four
subset_data_by_time <- function(data, nSubsets, nSkip){
  outList <- vector("list", length = nSubsets)
  totRow <- nrow(data)

  for (i in seq_len(nSubsets)) {
    rowsToGrab <- seq(i, totRow, nSkip)
    outList[[i]] <- data[rowsToGrab ,] 
  }
  return(outList)
}

#Function to count NA's
Count.na <- function(x){
  sum(is.na(x))
}

Basal_area <- function (dbh){
  ba = pi * (dbh**2) / 40000 #for DBH in cm the output will be in sqrt meters
} 


####------Subseting data------####
#subset and create dataframes in the four times (10,20,30,40 years)
dbh_dat_subsets<-subset_data_by_time(dbh_dat,4,4)
dbh_dat_10 <- as.data.frame(dbh_dat_subsets[[1]])
dbh_dat_20 <- as.data.frame(dbh_dat_subsets[[2]])
dbh_dat_30 <- as.data.frame(dbh_dat_subsets[[3]])
dbh_dat_40 <- as.data.frame(dbh_dat_subsets[[4]])

number_variables = nrow(dbh_dat_10)

####------Managing data------####
##For all the four df transpose them. Set zero values as NA's and calculate mean, sample sd and se
dbh_dat_10_col_names <- dbh_dat_10$Run_time #extrac first row from df to use as columns names after transposing
dbh_dat_10 <- as.data.frame(t(dbh_dat_10[,-1])) #transpose dataframe
colnames(dbh_dat_10) <- dbh_dat_10_col_names #set columns name
dbh_dat_10[dbh_dat_10 == 0] <- NA #set zeros as NA
# dbh_dat_10<-dbh_dat_10 %>%        #create new columns in the df with:
#   mutate(mean_dbh = rowMeans(dbh_dat_10,na.rm = T))%>%              #Mean
#   mutate(stand_dev = apply(dbh_dat_10, 1, sd,na.rm=T)) %>%           #standard deviation
#   mutate(count_na = apply(dbh_dat_10, 1, Count.na)) %>%              #number of NA's
#   mutate(stand_err = stand_dev/sqrt(number_variables-count_na))       #standard error

dbh_dat_20_col_names <- dbh_dat_20$Run_time
dbh_dat_20 <- as.data.frame(t(dbh_dat_20[,-1]))
colnames(dbh_dat_20) <- dbh_dat_20_col_names
dbh_dat_20[dbh_dat_20 == 0] <- NA
# dbh_dat_20<-dbh_dat_20 %>%
#   mutate(mean_dbh = rowMeans(dbh_dat_20,na.rm = T))%>% 
#   mutate(stand_dev = apply(dbh_dat_20, 1, sd,na.rm=T)) %>%
#   mutate(count_na = apply(dbh_dat_20, 1, Count.na)) %>% 
#   mutate(stand_err = stand_dev/sqrt(number_variables-count_na))

dbh_dat_30_col_names <- dbh_dat_30$Run_time
dbh_dat_30 <- as.data.frame(t(dbh_dat_30[,-1]))
colnames(dbh_dat_30) <- dbh_dat_30_col_names
dbh_dat_30[dbh_dat_30 == 0] <- NA
# dbh_dat_30<-dbh_dat_30 %>%
#   mutate(mean_dbh = rowMeans(dbh_dat_30,na.rm = T))%>% 
#   mutate(stand_dev = apply(dbh_dat_30, 1, sd,na.rm=T)) %>%
#   mutate(count_na = apply(dbh_dat_30, 1, Count.na)) %>% 
#   mutate(stand_err = stand_dev/sqrt(number_variables-count_na))

dbh_dat_40_col_names <- dbh_dat_40$Run_time
dbh_dat_40 <- as.data.frame(t(dbh_dat_40[,-1]))
colnames(dbh_dat_40) <- dbh_dat_40_col_names
dbh_dat_40[dbh_dat_40 == 0] <- NA
# dbh_dat_40<-dbh_dat_40 %>%
#   mutate(mean_dbh = rowMeans(dbh_dat_40,na.rm = T))%>% 
#   mutate(stand_dev = apply(dbh_dat_40, 1, sd,na.rm=T)) %>%
#   mutate(count_na = apply(dbh_dat_40, 1, Count.na)) %>% 
#   mutate(stand_err = stand_dev/sqrt(number_variables-count_na))

####------Summaring data------####
#integrate values from all the times df's to one single df
summary_df <- read.csv('data/Field_sheet.csv',as.is = T) #call a csv file which contains other data from trees
# summary_df_dbh<- summary_df %>% #use select() to extract just the columns we need.
#   select(one_of(c("Cap","Fallow_Area","Fallow_Age","Age_Category","Tree_Number_Cont","Tree_code", "DBH"))) %>%
#   mutate(mean_dbh_10 = dbh_dat_10$mean_dbh, #create new columns to all means
#          mean_dbh_20 = dbh_dat_20$mean_dbh,
#          mean_dbh_30 = dbh_dat_30$mean_dbh,
#          mean_dbh_40 = dbh_dat_40$mean_dbh) %>%
#   # mutate(stand_dev_dbh_10 = dbh_dat_10$stand_dev, #create new columns to all sd
#   #        stand_dev_dbh_20 = dbh_dat_20$stand_dev,
#   #        stand_dev_dbh_30 = dbh_dat_30$stand_dev,
#   #        stand_dev_dbh_40 = dbh_dat_40$stand_dev) %>%
#   mutate(count_na_dbh_10 = dbh_dat_10$count_na, #create new columns to all count of NA
#          count_na_dbh_20 = dbh_dat_20$count_na,
#          count_na_dbh_30 = dbh_dat_30$count_na,
#          count_na_dbh_40 = dbh_dat_40$count_na) %>%
#   # mutate(stand_err_dbh_10 = dbh_dat_10$stand_err, #create new columns to all se
#   #        stand_err_dbh_20 = dbh_dat_20$stand_err,
#   #        stand_err_dbh_30 = dbh_dat_30$stand_err,
#   #        stand_err_dbh_40 = dbh_dat_40$stand_err) %>%
#   mutate(basal_area_dbh_in = Basal_area(DBH),
#          basal_area_dbh_10 = Basal_area(mean_dbh_10),
#          basal_area_dbh_20 = Basal_area(mean_dbh_20),
#          basal_area_dbh_30 = Basal_area(mean_dbh_30),
#          basal_area_dbh_40 = Basal_area(mean_dbh_40))

# write.csv(summary_df_dbh, file = "output/summary_df_dbh.csv")

#### ---- test another way to calculate ba so survivalship is accounted for ----####
# replicate that to all years now
# calculating BA of fallows
df_ba_10 <- as.data.frame(sapply(dbh_dat_10,Basal_area))
df_ba_10 <- df_ba_10%>% 
  mutate(Cap = summary_df$Cap) %>% 
  group_by(Cap) %>% 
  summarise_all(funs(sum(.,na.rm = TRUE))) %>% 
  mutate(mean_f_ba_10 = rowMeans(.[,-1])) %>% 
  select(Cap,mean_f_ba_10)

df_ba_20 <- as.data.frame(sapply(dbh_dat_20,Basal_area))
df_ba_20 <- df_ba_20%>% 
  mutate(Cap = summary_df$Cap) %>% 
  group_by(Cap) %>% 
  summarise_all(funs(sum(.,na.rm = TRUE))) %>% 
  mutate(mean_f_ba_20 = rowMeans(.[,-1])) %>% 
  select(Cap,mean_f_ba_20)


df_ba_30 <- as.data.frame(sapply(dbh_dat_30,Basal_area))
df_ba_30 <- df_ba_30%>% 
  mutate(Cap = summary_df$Cap) %>% 
  group_by(Cap) %>% 
  summarise_all(funs(sum(.,na.rm = TRUE))) %>% 
  mutate(mean_f_ba_30 = rowMeans(.[,-1])) %>% 
  select(Cap,mean_f_ba_30)

df_ba_40 <- as.data.frame(sapply(dbh_dat_40,Basal_area))
df_ba_40 <- df_ba_40%>% 
  mutate(Cap = summary_df$Cap) %>% 
  group_by(Cap) %>% 
  summarise_all(funs(sum(.,na.rm = TRUE))) %>% 
  mutate(mean_f_ba_40 = rowMeans(.[,-1])) %>% 
  select(Cap,mean_f_ba_40)

# agregating values to one single df
summary_df <- read.csv('data/Field_sheet.csv',as.is = T) #call a csv file which contains other data from trees
summary_df_ba<- summary_df %>% #use select() to extract just the columns we need.
  select("Cap","Fallow_Area","Fallow_Age","Age_Category","DBH") %>%
  mutate(ba_in_t = Basal_area(DBH)) %>% 
  group_by(Cap) %>% 
  mutate(sum_f_ba_in = sum(ba_in_t)) %>%
  ungroup() %>% 
  select(one_of(c("Cap","Fallow_Area","Fallow_Age","Age_Category","sum_f_ba_in"))) %>% 
  group_by(Cap) %>% 
  summarise(Fallow_Area = mean(Fallow_Area),
            Fallow_Age = mean(Fallow_Age),
            Age_Category = first(Age_Category),
            sum_f_ba_in = mean(sum_f_ba_in)) %>% 
  mutate(ba_ha_in = sum_f_ba_in/Fallow_Area,
         ba_ha_10 = df_ba_10$mean_f_ba_10/Fallow_Area,
         ba_ha_20 = df_ba_20$mean_f_ba_20/Fallow_Area,
         ba_ha_30 = df_ba_30$mean_f_ba_30/Fallow_Area,
         ba_ha_40 = df_ba_40$mean_f_ba_40/Fallow_Area) %>% 
  select(Cap,Fallow_Age,Age_Category,ba_ha_in,ba_ha_10,ba_ha_20,ba_ha_30,ba_ha_40)

summary_df_ba_FinalThesis <- summary_df_ba # this is to send the df to another script without interfering with the rest of the code.

####------Plotting------####
#organizing data to be plotted
summary_df_ba_g <- melt(summary_df_ba,
                         id.vars = c("Cap","Fallow_Age","Age_Category"),
                         measure.vars = c("ba_ha_in","ba_ha_10","ba_ha_20","ba_ha_30","ba_ha_40")) %>% 
  mutate(Age = ifelse(variable == "ba_ha_in", Fallow_Age,
                      ifelse(variable == "ba_ha_10", Fallow_Age + 10,
                      ifelse(variable == "ba_ha_20", Fallow_Age + 20,
                      ifelse(variable == "ba_ha_30", Fallow_Age + 30,
                      ifelse(variable == "ba_ha_40", Fallow_Age + 40, NA)))))) %>% 
  select(Cap,Age,Age_Category,value)

# plotting
plot_growth_ba <- ggplot(data = summary_df_ba_g)+
  geom_line(aes(x = Age, y = value, group = Cap, color = Age_Category))+#linetype = Age_Category, color = Cap))+
  scale_linetype_manual(values = c(1,2,3,4))+
  scale_color_manual(values = c('#396ab1',"#da7c30","#3e9651","#922428"),
                     name = "Fallow Age\nClass (years)",
                     labels= c("12 - 17","20 - 22","26 - 29","48 - 60"))+
  geom_point(aes(x = Age, y = value, color = Age_Category))+
  xlab("Age (years)")+
  ylab(bquote("Projected basal area ("*m^2~ha^-1*")"))+
  scale_x_continuous(breaks = c(20,40,60,80,100))+
  scale_y_continuous(breaks = c(0,1,2,3,3.5))+
  ggtitle("Average sum of basal area of B. excelsa \n per hectare of each fallow, colored by age class.")+
  theme(legend.text.align=1,
        legend.title.align = 0.5,
        plot.title = element_text(family="TT Arial", hjust = 0.5, size = 15))

# ggsave(filename ="output/plot_growth_ba_50r.png",plot = plot_growth_ba,dpi = 600,width = 25, height = 12, units = c("cm"))

plot_growth_ba
```

<br />
<p align="center">
<img src="https://user-images.githubusercontent.com/38635706/156422619-80f24771-1c4c-4eef-a502-dfb927233e5d.png" alt="Logo" height="450">
</p>
<br />

<!-- ============================================================================================================================================ -->

### Fruit prodcutivity
```R
# Original R file is "FINAL_prod(non-cum).R"

prod_dat = read.csv('data/model_output_prod_50r_10cmDBHlimits.csv',as.is = T)
library(ggplot2)
# library(plyr)
# library(Rmisc) 
library(dplyr)
library(wesanderson)
library(reshape2)

####------Functions------####
#Function to subset the data in four
subset_data_by_time <- function(data, nSubsets, nSkip){
  outList <- vector("list", length = nSubsets)
  totRow <- nrow(data)
  
  for (i in seq_len(nSubsets)) {
    rowsToGrab <- seq(i, totRow, nSkip)
    outList[[i]] <- data[rowsToGrab ,] 
  }
  return(outList)
}

#Function to count NA's
Count.na <- function(x){
  sum(is.na(x))
}


####------Subseting data------####
#subset and create dataframes in the four times (10,20,30,40 years)
prod_dat_subsets<-subset_data_by_time(prod_dat,4,4)
prod_dat_10 <- as.data.frame(prod_dat_subsets[[1]])
prod_dat_20 <- as.data.frame(prod_dat_subsets[[2]])
prod_dat_30 <- as.data.frame(prod_dat_subsets[[3]])
prod_dat_40 <- as.data.frame(prod_dat_subsets[[4]])

number_variables = nrow(prod_dat_10)

###------Managing data------####
#For all the four df transpose them. Set zero values as NA's and calculate mean, sample sd and se
prod_dat_10_col_names <- prod_dat_10$Run_time #extrac first row from df to use as columns names after transposing
prod_dat_10 <- as.data.frame(t(prod_dat_10[,-1])) #transpose dataframe
colnames(prod_dat_10) <- prod_dat_10_col_names #set columns name
prod_dat_10[prod_dat_10 == 0] <- NA #set zeros as NA
prod_dat_10<-prod_dat_10 %>%        #create new columns in the df with:
  mutate(mean_prod = rowMeans(prod_dat_10,na.rm = T))%>%              #Mean
  mutate(stand_dev = apply(prod_dat_10, 1, sd,na.rm=T)) %>%           #standard deviation
  mutate(count_na = apply(prod_dat_10, 1, Count.na)) %>%              #number of NA's
  mutate(stand_err = stand_dev/sqrt(number_variables-count_na))       #standard error

prod_dat_20_col_names <- prod_dat_20$Run_time
prod_dat_20 <- as.data.frame(t(prod_dat_20[,-1]))
colnames(prod_dat_20) <- prod_dat_20_col_names
prod_dat_20[prod_dat_20 == 0] <- NA
prod_dat_20<-prod_dat_20 %>%
  mutate(mean_prod = rowMeans(prod_dat_20,na.rm = T))%>% 
  mutate(stand_dev = apply(prod_dat_20, 1, sd,na.rm=T)) %>%
  mutate(count_na = apply(prod_dat_20, 1, Count.na)) %>% 
  mutate(stand_err = stand_dev/sqrt(number_variables-count_na))
  
prod_dat_30_col_names <- prod_dat_30$Run_time
prod_dat_30 <- as.data.frame(t(prod_dat_30[,-1]))
colnames(prod_dat_30) <- prod_dat_30_col_names
prod_dat_30[prod_dat_30 == 0] <- NA
prod_dat_30<-prod_dat_30 %>%
  mutate(mean_prod = rowMeans(prod_dat_30,na.rm = T))%>% 
  mutate(stand_dev = apply(prod_dat_30, 1, sd,na.rm=T)) %>%
  mutate(count_na = apply(prod_dat_30, 1, Count.na)) %>% 
  mutate(stand_err = stand_dev/sqrt(number_variables-count_na))

prod_dat_40_col_names <- prod_dat_40$Run_time
prod_dat_40 <- as.data.frame(t(prod_dat_40[,-1]))
colnames(prod_dat_40) <- prod_dat_40_col_names
prod_dat_40[prod_dat_40 == 0] <- NA
prod_dat_40<-prod_dat_40 %>%
  mutate(mean_prod = rowMeans(prod_dat_40,na.rm = T))%>% 
  mutate(stand_dev = apply(prod_dat_40, 1, sd,na.rm=T)) %>%
  mutate(count_na = apply(prod_dat_40, 1, Count.na)) %>% 
  mutate(stand_err = stand_dev/sqrt(number_variables-count_na))

####------Summaring data------####
#integrate values from all the times df's to one single df
summary_df <- read.csv('data/Field_sheet.csv',as.is = T) #call a csv file which contains other data from trees
summary_df_prod<- summary_df %>% #use select() to extract just the columns we need.
  select(one_of(c("Cap","Fallow_Area","Fallow_Age","Age_Category","Tree_Number_Cont","Tree_code","Col"))) %>% 
  mutate(mean_prod_10 = prod_dat_10$mean_prod, #create new columns to all means
         mean_prod_20 = prod_dat_20$mean_prod,
         mean_prod_30 = prod_dat_30$mean_prod,
         mean_prod_40 = prod_dat_40$mean_prod) %>% 
  mutate(stand_dev_prod_10 = prod_dat_10$stand_dev, #create new columns to all sd
         stand_dev_prod_20 = prod_dat_20$stand_dev,
         stand_dev_prod_30 = prod_dat_30$stand_dev,
         stand_dev_prod_40 = prod_dat_40$stand_dev) %>% 
  mutate(count_na_prod_10 = prod_dat_10$count_na, #create new columns to all count of NA
         count_na_prod_20 = prod_dat_20$count_na,
         count_na_prod_30 = prod_dat_30$count_na,
         count_na_prod_40 = prod_dat_40$count_na) %>% 
  mutate(stand_err_prod_10 = prod_dat_10$stand_err, #create new columns to all se
         stand_err_prod_20 = prod_dat_20$stand_err,
         stand_err_prod_30 = prod_dat_30$stand_err,
         stand_err_prod_40 = prod_dat_40$stand_err)

# write.csv(summary_df, file = "output/summary_df.csv")


####------Summaring data into fallows------####
summary_df_prod_caps <- summary_df_prod %>%
  group_by(Cap, Fallow_Age, Fallow_Area, Col) %>% 
  summarise(prod_10 = sum(mean_prod_10, na.rm = T)/mean(Fallow_Area),
            prod_20 = sum(mean_prod_20, na.rm = T)/mean(Fallow_Area),
            prod_30 = sum(mean_prod_30, na.rm = T)/mean(Fallow_Area),
            prod_40 = sum(mean_prod_40, na.rm = T)/mean(Fallow_Area)) %>% 
  arrange(Fallow_Age)%>%
#sg = stack graph, here I start to prepare the df to plot the stack graph
# need to adjust data. Because they are already a sum of each other, I cannot add them again, and that is what a sg does.
# therefor I adjust them so it will only show the increment from every decade.
  mutate(prod_10_sg = prod_10,
         prod_20_sg = prod_20-prod_10_sg,
         prod_30_sg = prod_30-prod_20_sg-prod_10_sg,
         prod_40_sg = prod_40-prod_30_sg-prod_20_sg-prod_10_sg)%>%
  ungroup() %>%
  select(Cap,prod_10_sg,prod_20_sg,prod_30_sg,prod_40_sg,Col)

####------Plotting------####
summary_df_prod_f_sg <- summary_df_prod_caps %>% 
  select(Cap,prod_10_sg,prod_20_sg,prod_30_sg,prod_40_sg) %>% 
  melt(id.vars = "Cap")


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Cap_order<-factor(summary_df_f_sg$Cap, levels=c('J','G','L','C','E','M','A','Q','B','D','I','P','K','F','S','R','O','N'))
plot_prod_bar<- ggplot(data=summary_df_prod_f_sg, aes(x=factor(Cap, levels=c('J','G','L','C','E','M','A','Q','B','D','I','P','K','F','S','R','O','N')), y=value, fill=factor(variable, levels=c("prod_40_sg","prod_30_sg","prod_20_sg","prod_10_sg"))))+
  geom_bar(position="stack", stat="identity")+
  scale_x_discrete(name = "Fallows in age order, from the youngest (12 yrs) to the oldest (60 years)")+
  scale_y_continuous(name = bquote("Projected annual fruit productivity (fruits  "*ha^-1*")"),
                     breaks = c(100,250,500,1000))+
  scale_fill_manual(values = wes_palette(name="GrandBudapest1",n=4),
                    # breaks = c(prod_10_sg, prod_20_sg, prod_30_sg,prod_40_sg),
                    name = "Years",
                    labels = c("40","30","20","10"))+
  ggtitle("Projected annual Brazil nut fruit productivity for \n each fallow at the end of 10, 20, 30 and 40 years, \n ordered by landowner fallow age estimates")+
  theme(axis.text.x = element_text(size = 11,
                                   color = c(rep('#396ab1',6),
                                             rep("#da7c30",6),
                                             rep("#3e9651",4),
                                             rep("#922428",2))),
        plot.title = element_text(family="TT Arial", hjust = 0.5, size = 15))

# ggsave(filename ="output/plot_prod_50r.png",plot = plot_prod_bar,dpi = 600,width = 20, height = 12, units = c("cm"))
plot_prod_bar
```

<br />
<p align="center">
<img src="https://user-images.githubusercontent.com/38635706/156422420-f2aa2fd5-99e4-4bf1-a61f-134784591bc0.png" alt="Logo" height="450">
</p>
<br />

```R
####------Collecting data to use in the thesis------####

sum(summary_df_prod_caps$prod_10_sg)
(sum(summary_df_prod_caps$prod_10_sg)-summary_df_prod_caps$prod_10_sg[2]-summary_df_prod_caps$prod_10_sg[5])*100/2307.55
summary_df_prod_caps$prod_30_sg # look for min value other than zero
max(summary_df_prod_caps$prod_30_sg, na.rm = T)
mean(summary_df_prod_caps$prod_40_sg)
sd(summary_df_prod_caps$prod_40_sg)

df_10yrs_producing_trees <- summary_df_prod %>% 
   filter(mean_prod_10 > 0)

mean(df_10yrs_producing_trees$mean_prod_10) #68.58298
plotrix::std.error(df_10yrs_producing_trees$mean_prod_10) #21.54941
```

<!-- ============================================================================================================================================ -->

### Mapping

```R
# Original R file is "production_map.R"

# Brazil nut productivity map.
# Represent spatial distribution of the fallows studied in this project and
# Brazil nut production in most of the householdings per ha at 
#the Projeto de Assentamento Agro-Extrativista Chico Mendes (PAECM), Xapuri, Acre, Brazil.
# Created by Eduardo S. Bongiolo as part of his Master thesis
# Data 07 Nov, 2018

# Open libraries ############
library(sf) #sf = simple feature
library(ggplot2)
library(raster)
# library(rgdal)
library(dplyr)

# Loading data ############
#Projeto de assentamento agroextrativista Chico Mendes (PAECM) delimitation
#upload perimeter of study area, properties within study area and house properties coordinates
PAECM_boundary <- st_read("data/google_earth/PAECM_boundary.shp") # perimeter of the study area
```
```R
## Reading layer `PAECM_boundary' from data source `C:\Users\eduar\OneDrive - University of Florida\r-to_thesis\data\google_earth\PAECM_boundary.shp' using driver `ESRI Shapefile'
## Simple feature collection with 1 feature and 11 fields
## geometry type:  POLYGON
## dimension:      XYZ
## bbox:           xmin: -68.43256 ymin: -11.04071 xmax: -68.22755 ymax: -10.80547
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```
```R
# PAECM_properties_boundaries<- st_read("data/map/Shape files Cachoeira Tecman/propriedade_amppaecm_POA2015_UPA09.shp") #perimeter of properties, does not contains all
PAECM_properties_points <-read.csv("data/google_earth/properties.csv") #Coordinates of the house from most of all properties in a map from INCRA 2005
PAECM_fallows <-read.csv("data/google_earth/spatial_dist_fallows.csv") #Coordinates to each of the fallows used in this study
brazil_bolivia_border <-st_read("data/google_earth/brazil_bolivia_border.shp")
```
```R
## Reading layer `brazil_bolivia_border' from data source `C:\Users\eduar\OneDrive - University of Florida\r-to_thesis\data\google_earth\brazil_bolivia_border.shp' using driver `ESRI Shapefile'
## Simple feature collection with 2 features and 10 fields
## geometry type:  LINESTRING
## dimension:      XYZ
## bbox:           xmin: -68.48731 ymin: -11.0526 xmax: -68.17611 ymax: -10.84693
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```

```R
# these two zoom squares are to zoom in at the map where the points are to close to be properly seen
zoom_square_1<- rbind(c(-68.403542,-10.821079),
                      c(-68.367060,-10.821079),
                      c(-68.367060,-10.862918),
                      c(-68.403542,-10.862918),
                      c(-68.403542,-10.821079)) #add the first point again to it wrap up.
zoom_square_1_pol <- st_polygon(list(zoom_square_1))

zoom_square_2<- rbind(c(-68.370381,-10.903649),
                      c(-68.362954,-10.903649),
                      c(-68.362954,-10.910137),
                      c(-68.370381,-10.910137),
                      c(-68.370381,-10.903649)) #add the first point again to it wrap up.
zoom_square_2_pol <- st_polygon(list(zoom_square_2))

# CRS ############
#set Coordinates reference system (CRS) to data imported from .csv
map_CRS <- st_crs(PAECM_boundary)
PAECM_properties_points_sp <- st_as_sf(PAECM_properties_points,coords = c("X", "Y"),crs = map_CRS)
# st_crs(PAECM_properties_points_sp) #look at the crs

PAECM_fallows_sp <- st_as_sf(PAECM_fallows,coords = c("X", "Y"),crs = map_CRS)
# st_crs(PAECM_fallows_sp)#look at the crs

zoom_square_1_pol_CRS<- st_sfc(x = zoom_square_1_pol,
                               crs = map_CRS)
zoom_square_2_pol_CRS<- st_sfc(x = zoom_square_2_pol,
                               crs = map_CRS)

# Inset map ############

Latin_America <- map_data("world", regions = c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador",
                                               "French Guiana","Guyana","Paraguay","Peru","Suriname","Uruguay",
                                               "Venezuela", "Panama")) #Latin America countries + Panama
Latin_America_sp <- st_as_sf(Latin_America,coords = c("long", "lat"),crs = map_CRS)
# st_crs(Latin_America_sp)
inset <-ggplot()+
  geom_sf(data = Latin_America_sp, lwd = 0.3)+
  annotate("text",label = "Brazil", x = -65, y = -5, colour = "grey28", size = 4)+
  geom_sf(data = PAECM_boundary, size = 3, color = "red")+
  ggplot2::theme_minimal()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_rect(color="grey10", size=0.5, linetype="solid",fill=NA),
        panel.grid=element_line(colour = "white"))+
  coord_sf(xlim = c(-80,-50), ylim = c(10,-20))
# inset #it takes a while

# Prodcutivity in Fruits Map ####
## Data Managing fruit ====
#Categorizing prod in categories
custom_bins_fruit = c(0,60,120,180,240,1400)
# custom_bins = c(10,333,666,1100)
PAECM_properties_points_fruit <- PAECM_properties_points_sp %>% 
  mutate(prod_cat = cut(fruit_productivity,breaks = custom_bins_fruit))


PAECM_fallows_fruit <- PAECM_fallows_sp %>% 
  mutate(prod_cat_fallow = cut(prod_40, breaks= custom_bins_fruit),
         age_cat_fallow = cut(age, breaks = c(11,17,22,29,60)))


## Plotting ====
cbPalette<- c("#feebe2","#fbb4b9","#f768a1","#c51b8a","#7a0177")
cbPalette_f_legend<- c(NA,"#feebe2","#fbb4b9","#f768a1","#c51b8a","#7a0177")
# age_colors <- c('#396ab1',"#da7c30","#3e9651","#922428")

### main map ----
prod_map_PAECM_fruit<-ggplot()+
  #Call the limit of the study site + brazil/bolivia border
  geom_sf(data = brazil_bolivia_border)+
  geom_sf(data = PAECM_boundary)+
  # call the production data of all properties
  geom_sf(data = PAECM_properties_points_fruit,aes(fill = prod_cat),size = 3.75, shape = 21,show.legend = "point")+
  # call the gps coordenates of the inventoried fallows
  geom_sf(data = PAECM_fallows_fruit,aes(size = prod_cat_fallow), shape = 18, show.legend = "point")+
  # call the zoom in squares
  geom_sf(data = zoom_square_1_pol_CRS,color = "black", fill = NA)+
  geom_sf(data = zoom_square_2_pol_CRS,color = "red", fill = NA)+
  # age # not being used right now
  # scale_color_manual(values = age_colors,
  #                    name= "Fallows Age\n(years)",#(cans/ha)
  #                    breaks= c("(11,17]","(17,22]","(22,29]","(29,60]"),
  #                    labels= c("12 - 17","20 - 22","26 - 29","48 - 60"),
  #                    guide = guide_legend(override.aes = list(linetype = "blank", shape = 18, size = 5)))+
  # manually edit legend to the prodcutivity data
  scale_fill_manual(values=cbPalette,
                    name = expression(atop("",
                                           atop(textstyle("Individual"),
                                                atop(textstyle("landholding"),
                                                     atop(textstyle("fruit productivity"),
                                                          atop(textstyle("estimates for"),
                                                               atop(textstyle("2017-2018"),
                                                                    atop(textstyle("(fruits ha"^-1*")"))))))))),
                    breaks=c(NA,"(0,60]","(60,120]","(120,180]","(180,240]","(240,1.4e+03]"),
                    labels= c("NA","\u2264 60","60 - 120","120 - 180","180 - 240","> 240"),
                    guide = guide_legend(override.aes = list(linetype = 0, shape = 21, size = 5, fill = cbPalette_f_legend)))+
  #manually edit legend to the inventoried fallows
  # production in 40 years
  scale_size_manual(values= c(2,3,4,5,6),
                    name = expression(atop("", #needs to add this line, because atop() creates a extra space between line one and two
                                           atop(textstyle("Projected fruit"), #textsyle() is to plot all text with the same size
                                                atop(textstyle("productivity in"),
                                                     atop(textstyle("fallows in 40 yrs"),
                                                          atop(textstyle("(fruits ha"^-1*")"))))))),
                    breaks= c(NA,"(0,60]","(60,120]","(120,180]","(180,240]","(240,1.4e+03]"),
                    labels= c("NA","\u2264 60","60 - 120","120 - 180","180 - 240","> 240"),
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = 18, fill = NA)))+
  ggplot2::theme_minimal()+ # I like this theme, but edited some aspects below.
  ggplot2::theme(legend.text.align = 0.5,
                 legend.title.align = 0.5,
                 plot.background = element_blank(),
                 panel.grid = element_line(colour = "white"),
                 panel.background = element_rect(fill = "grey87", color = "white"))+#,
  # plot a scalebar
  ggsn::scalebar(dist = 5,
                 dd2km = F,
                 location = "bottomright",
                 model = 'WGS84', 
                 st.bottom = F,
                 height = 0.01,
                 st.dist = 0.01,
                 x.min = -68.355, x.max = -68.20, y.min= -10, y.max= -11.05)+
  #plot the north arrow
  ggsn::north(symbol = 1, #see ggsn::northSymbols() for a list of all 18 symbols
              scale = 0.8,
              x.min = -68.25, x.max = -68.2, y.min= -10.85, y.max= -10.8)+
  annotate("text",label = "Brazil", x = -68.3, y = -10.85, colour = "grey28", size = 4)+ # add text to the map
  annotate("text",label = "Bolivia", x = -68.35, y = -11.05, colour = "grey28", size = 4)+ # add text to the map
  # guides(shape = FALSE)+
  coord_sf(xlim = c(-68.45,-68.2), ylim = c(-11.05,-10.8)) #reduce the map a little bit.
# prod_map_PAECM_fruit

  ### No legend Map ----
#same plot as above but without the legend it will be needed bellow.
prod_map_PAECM_fruit_no_leg<-prod_map_PAECM_fruit+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  guides(size=FALSE,color = FALSE, fill = FALSE)
# prod_map_PAECM_fruit_no_leg

### Zoom polygons ----
prod_map_PAECM_fruit_zoom_1 <- prod_map_PAECM_fruit+
  theme(title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_blank(),
        panel.border= element_rect(fill = NA, color = "black"))+
  guides(size=FALSE,color = FALSE, fill = FALSE)+
  coord_sf(xlim = c(-68.401, -68.3695) , ylim = c(-10.86, -10.825)) #it will return a warning saying that the map already has a crs
# prod_map_PAECM_fruit_zoom_1

prod_map_PAECM_fruit_zoom_2 <- prod_map_PAECM_fruit+
  theme(title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_blank(),
        panel.border= element_rect(fill = NA, color = "red"))+
  guides(size=FALSE,color = FALSE, fill = FALSE)+
  coord_sf(xlim = c(-68.37, -68.3635) , ylim = c(-10.9043, -10.9098)) #it will return a warning saying that the map already has a crs
# prod_map_PAECM_fruit_zoom_2

# legend 1
# plot only the geom_sf that has a legend, this way is easy to choose the order of the legends for the final plot
prod_map_PAECM_fruit_legend_1<-ggplot()+
  # This is a simplified version of the main map only to create a legend.
  geom_sf(data = PAECM_properties_points_fruit,aes(fill = prod_cat),size = 3.75, shape = 21,show.legend = "point")+
  # manually edit legend to the prodcutivity data
  scale_fill_manual(values=cbPalette,
                    name = expression(atop("",
                                           atop(textstyle("Individual"),
                                                atop(textstyle("landholding"),
                                                     atop(textstyle("fruit productivity"),
                                                          atop(textstyle("estimates for"),
                                                               atop(textstyle("2017-2018"),
                                                                    atop(textstyle("(fruits ha"^-1*")"))))))))),
                    breaks=c(NA,"(0,60]","(60,120]","(120,180]","(180,240]","(240,1.4e+03]"),
                    labels= c("NA","\u2264 60","60 - 120","120 - 180","180 - 240","> 240"),
                    guide = guide_legend(override.aes = list(linetype = 0, shape = 21, size = 5, fill = cbPalette_f_legend)))+
  ggplot2::theme_minimal()+ # I like this theme, but edited some aspects below.
  ggplot2::theme(legend.text.align = 0.5,
                 legend.title.align = 0.5,
                 plot.background = element_blank(),
                 panel.grid = element_line(colour = "white"),
                 panel.background = element_rect(fill = "grey87", color = "white"))+#,
  coord_sf()
# prod_map_PAECM_fruit_legend_1

#legend 2
# plot only the geom_sf that has a legend, this way is easy to choose the order of the legends for the final plot
prod_map_PAECM_fruit_legend_2<-ggplot()+
  geom_sf(data = PAECM_fallows_fruit,aes(size = prod_cat_fallow), shape = 18, show.legend = "point")+
  scale_size_manual(values= c(2,3,4,5,6),
                    name = expression(atop("", #needs to add this line, because atop() creates a extra space between line one and two
                                           atop(textstyle("Projected fruit"), #textsyle() is to plot all text with the same size
                                                atop(textstyle("productivity in"),
                                                     atop(textstyle("fallows in 40 yrs"),
                                                          atop(textstyle("(fruits ha"^-1*")"))))))),
                    breaks= c(NA,"(0,60]","(60,120]","(120,180]","(180,240]","(240,1.4e+03]"),
                    labels= c("NA","\u2264 60","60 - 120","120 - 180","180 - 240","> 240"),
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = 18, fill = NA)))+
  ggplot2::theme_minimal()+ # I like this theme, but edited some aspects below.
  ggplot2::theme(legend.text.align = 0.5,
                 legend.title.align = 0.5,
                 plot.background = element_blank(),
                 panel.grid = element_line(colour = "white"),
                 panel.background = element_rect(fill = "grey87", color = "white"))+#,
  coord_sf()
# prod_map_PAECM_fruit_legend_2

## Saving to a file ####

# plot everything we want into a matrix. extrac legend from the prod_map and used the 
# prod_map_PAECM_no_leg to plot
legend_1 <- ggpubr::get_legend(prod_map_PAECM_fruit_legend_1) # extract the legend from the map
legend_2 <- ggpubr::get_legend(prod_map_PAECM_fruit_legend_2) # extract the legend from the map
gf <- gridExtra::arrangeGrob(prod_map_PAECM_fruit_no_leg, #1
                             legend_1, #2
                             legend_2,#3
                             inset, #4
                             prod_map_PAECM_fruit_zoom_1, #5
                             prod_map_PAECM_fruit_zoom_2, #6
                             layout_matrix = matrix(c(5, 1, 1, 1, 1, 3,
                                                      5, 1, 1, 1, 1, 3,
                                                      6, 1, 1, 1, 1, 2,
                                                      6, 1, 1, 1, 1, 2,
                                                      6, 1, 1, 1, 1, 4),
                                                    ncol = 6, # it has to be equal to the number of columns you want
                                                    byrow = T))

# ggsave(filename ="output/prod_mapv20_fruit.png",plot = gf,dpi = 300, width = 30, height = 20, units = c("cm"))
# plot(gf) # This was used in R, but in the Markdown is not plotting correctly. So I used the code bellow to show the map.

plot_prod_map_PAECM_fruit <- png::readPNG("output/prod_mapv20_fruit.png")
grid::grid.raster(plot_prod_map_PAECM_fruit)
```

<br />
<p align="center">
<img src="https://user-images.githubusercontent.com/38635706/156422160-029ab0bf-0489-4b31-8915-2b52a27b7d36.png" alt="Logo" height="450">
</p>
<br />

<p align="right">(<a href="#top">back to top</a>)</p>


