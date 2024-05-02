Final Project - Final Report
================
Srikar Lanka
4/28/2024

# Initial Setup

## Important Packages

``` r
library(tidyverse)
library(dcData)
library(janitor)
library(devtools)

library(esquisse)
library(mosaic)
library(mosaicData)
library(ggplot2)
```

## Isolate Datasets

``` r
evData <- read.csv("Electric_Vehicle_Population_Data.csv")
demographicData <- ZipDemography %>%
  mutate(ZIP = as.integer(ZIP))   
```

## Learn More About the Data

``` r
glimpse(evData)
```

    ## Rows: 177,866
    ## Columns: 17
    ## $ VIN..1.10.                                        <chr> "5YJYGDEE1L", "7SAYG…
    ## $ County                                            <chr> "King", "Snohomish",…
    ## $ City                                              <chr> "Seattle", "Bothell"…
    ## $ State                                             <chr> "WA", "WA", "WA", "W…
    ## $ Postal.Code                                       <int> 98122, 98021, 98109,…
    ## $ Model.Year                                        <int> 2020, 2023, 2019, 20…
    ## $ Make                                              <chr> "TESLA", "TESLA", "T…
    ## $ Model                                             <chr> "MODEL Y", "MODEL Y"…
    ## $ Electric.Vehicle.Type                             <chr> "Battery Electric Ve…
    ## $ Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility <chr> "Clean Alternative F…
    ## $ Electric.Range                                    <int> 291, 0, 270, 210, 0,…
    ## $ Base.MSRP                                         <int> 0, 0, 0, 0, 0, 0, 0,…
    ## $ Legislative.District                              <int> 37, 1, 36, 5, 23, 2,…
    ## $ DOL.Vehicle.ID                                    <int> 125701579, 244285107…
    ## $ Vehicle.Location                                  <chr> "POINT (-122.30839 4…
    ## $ Electric.Utility                                  <chr> "CITY OF SEATTLE - (…
    ## $ X2020.Census.Tract                                <dbl> 53033007800, 5306105…

``` r
print("---")
```

    ## [1] "---"

``` r
glimpse(demographicData)
```

    ## Rows: 42,741
    ## Columns: 44
    ## $ Totalpopulation                                        <dbl> NA, NA, NA, NA,…
    ## $ Male                                                   <dbl> NA, NA, NA, NA,…
    ## $ Female                                                 <dbl> NA, NA, NA, NA,…
    ## $ MedianAge                                              <dbl> NA, NA, NA, NA,…
    ## $ Under5years                                            <dbl> NA, NA, NA, NA,…
    ## $ X18yearsandover                                        <dbl> NA, NA, NA, NA,…
    ## $ X65yearsandover                                        <dbl> NA, NA, NA, NA,…
    ## $ Onerace                                                <dbl> NA, NA, NA, NA,…
    ## $ White                                                  <dbl> NA, NA, NA, NA,…
    ## $ BlackorAfricanAmerican                                 <dbl> NA, NA, NA, NA,…
    ## $ AmericanIndianandAlaskaNative                          <dbl> NA, NA, NA, NA,…
    ## $ Asian                                                  <dbl> NA, NA, NA, NA,…
    ## $ NativeHawaiianandOtherPacificIslander                  <dbl> NA, NA, NA, NA,…
    ## $ Someotherrace                                          <dbl> NA, NA, NA, NA,…
    ## $ Twoormoreraces                                         <dbl> NA, NA, NA, NA,…
    ## $ HispanicorLatinoofanyrace                              <dbl> NA, NA, NA, NA,…
    ## $ AverageHouseholdSize                                   <dbl> NA, NA, NA, NA,…
    ## $ Averagefamilysize                                      <dbl> NA, NA, NA, NA,…
    ## $ Totalhousingunits                                      <dbl> NA, NA, NA, NA,…
    ## $ Occupiedhousingunits                                   <dbl> NA, NA, NA, NA,…
    ## $ Owneroccupiedhousingunits                              <dbl> NA, NA, NA, NA,…
    ## $ Renteroccupiedhousingunits                             <dbl> NA, NA, NA, NA,…
    ## $ Vacanthousingunits                                     <dbl> NA, NA, NA, NA,…
    ## $ Population25yearsandover                               <dbl> NA, NA, NA, NA,…
    ## $ Highschoolgraduateorhigher                             <dbl> NA, NA, NA, NA,…
    ## $ Bachelorsdegreeorhigher                                <dbl> NA, NA, NA, NA,…
    ## $ Civilianveterans                                       <dbl> NA, NA, NA, NA,…
    ## $ Disabilitystatuspopulation21to64years                  <dbl> NA, NA, NA, NA,…
    ## $ Foreignborn                                            <dbl> NA, NA, NA, NA,…
    ## $ Nowmarriedpopulation15yearsandover                     <dbl> NA, NA, NA, NA,…
    ## $ SpeakalanguageotherthanEnglishathome5yearsandover      <dbl> NA, NA, NA, NA,…
    ## $ Inlaborforcepopulation16yearsandover                   <dbl> NA, NA, NA, NA,…
    ## $ Meantraveltimetoworkinminutespopulation16yearsandolder <dbl> NA, NA, NA, NA,…
    ## $ Medianhouseholdincomedollars                           <dbl> NA, NA, NA, NA,…
    ## $ Medianfamilyincomedollars                              <dbl> NA, NA, NA, NA,…
    ## $ Percapitaincomedollars                                 <dbl> NA, NA, NA, NA,…
    ## $ Familiesbelowpovertylevel                              <dbl> NA, NA, NA, NA,…
    ## $ Individualsbelowpovertylevel                           <dbl> NA, NA, NA, NA,…
    ## $ Singlefamilyowneroccupiedhomes                         <dbl> NA, NA, NA, NA,…
    ## $ Medianvaluedollars                                     <dbl> NA, NA, NA, NA,…
    ## $ Medianofselectedmonthlyownercosts                      <dbl> NA, NA, NA, NA,…
    ## $ WithaMortgage                                          <dbl> NA, NA, NA, NA,…
    ## $ Notmortgaged                                           <dbl> NA, NA, NA, NA,…
    ## $ ZIP                                                    <int> 501, 544, 601, …

``` r
print("---")
```

    ## [1] "---"

``` r
names(demographicData)
```

    ##  [1] "Totalpopulation"                                       
    ##  [2] "Male"                                                  
    ##  [3] "Female"                                                
    ##  [4] "MedianAge"                                             
    ##  [5] "Under5years"                                           
    ##  [6] "X18yearsandover"                                       
    ##  [7] "X65yearsandover"                                       
    ##  [8] "Onerace"                                               
    ##  [9] "White"                                                 
    ## [10] "BlackorAfricanAmerican"                                
    ## [11] "AmericanIndianandAlaskaNative"                         
    ## [12] "Asian"                                                 
    ## [13] "NativeHawaiianandOtherPacificIslander"                 
    ## [14] "Someotherrace"                                         
    ## [15] "Twoormoreraces"                                        
    ## [16] "HispanicorLatinoofanyrace"                             
    ## [17] "AverageHouseholdSize"                                  
    ## [18] "Averagefamilysize"                                     
    ## [19] "Totalhousingunits"                                     
    ## [20] "Occupiedhousingunits"                                  
    ## [21] "Owneroccupiedhousingunits"                             
    ## [22] "Renteroccupiedhousingunits"                            
    ## [23] "Vacanthousingunits"                                    
    ## [24] "Population25yearsandover"                              
    ## [25] "Highschoolgraduateorhigher"                            
    ## [26] "Bachelorsdegreeorhigher"                               
    ## [27] "Civilianveterans"                                      
    ## [28] "Disabilitystatuspopulation21to64years"                 
    ## [29] "Foreignborn"                                           
    ## [30] "Nowmarriedpopulation15yearsandover"                    
    ## [31] "SpeakalanguageotherthanEnglishathome5yearsandover"     
    ## [32] "Inlaborforcepopulation16yearsandover"                  
    ## [33] "Meantraveltimetoworkinminutespopulation16yearsandolder"
    ## [34] "Medianhouseholdincomedollars"                          
    ## [35] "Medianfamilyincomedollars"                             
    ## [36] "Percapitaincomedollars"                                
    ## [37] "Familiesbelowpovertylevel"                             
    ## [38] "Individualsbelowpovertylevel"                          
    ## [39] "Singlefamilyowneroccupiedhomes"                        
    ## [40] "Medianvaluedollars"                                    
    ## [41] "Medianofselectedmonthlyownercosts"                     
    ## [42] "WithaMortgage"                                         
    ## [43] "Notmortgaged"                                          
    ## [44] "ZIP"

## Select Rows of Interest and Clean-Up Data

``` r
evData <-
  evData %>%
    select("VIN..1.10.", County, City, State, Postal.Code) %>% #Select columns of interest from evData
    filter(State == "WA") %>% # Filter for cars registered in the State of Washington
    rename(VIN = "VIN..1.10.") # Rename VIN to a more usable name
```

``` r
demographicData <-
  demographicData %>%
    select(Totalpopulation, MedianAge, X65yearsandover, Asian, Bachelorsdegreeorhigher, Meantraveltimetoworkinminutespopulation16yearsandolder, Medianhouseholdincomedollars, ZIP) %>% #Select Data of Interest
    rename(Population = Totalpopulation) %>% #Rename Long/Redudndant Names to more usable names
    rename("SixtyFivePlus" = X65yearsandover) %>% 
    rename("BachelorsDegree" = Bachelorsdegreeorhigher) %>% 
    rename("MeanTravelTime" = Meantraveltimetoworkinminutespopulation16yearsandolder) %>% 
    rename("MedianHouseholdIncome" = Medianhouseholdincomedollars) %>% 
    drop_na() #Drop Empty Values
```

# Background Information

## Guiding Research Question:

In this project, my ultimate goal is to find what demographics are
critical indicators of high EV ownership on a county level. I hope to
accomplish this task by comparing the rates of EV ownership vs the
general demographic makeups of various counties in the State of
Washington.

## Why is this Topic Interesting:

This topic is certainly a very interesting one to investigate because of
the sheer rate at which we have seen electric cars grow in the United
States. They have been exponentially becoming more popular as technology
has improved, and it can be interesting to take a step back and try to
gain a greater understanding of whether any demographics are driving
this increase in adoption.

The implications of a broader study on these topics are twofold.
Companies could better market their cars to demographics that are not
currently interested in them while also focusing on counties where there
is a demographic of interest for their cars. Governments could use this
data to see where they may need to focus on regulatory efforts.

This project also represents a chance to investigate something
fascinating. I hope to learn more about this topic in general.

# Analysis

## Analysis Set-Up (Join and Clean-Up)

### Note:

[“Referenced
Website”](https://www.geeksforgeeks.org/sum-across-multiple-rows-and-columns-using-dplyr-package-in-r/)

``` r
JoinData <- 
  evData %>% 
    group_by(County, Postal.Code) %>%
    summarise(totalCar = n()) %>% #Group_By County and Postal Code and then find the number of cars in each combination
    inner_join(demographicData, by=c("Postal.Code"="ZIP")) %>% #Join with the demographic data on Zip-Codes
    select(-c(Postal.Code)) %>% # Drop the Zip Code Column
    group_by(County) %>%  
    mutate(totalCar = sum(totalCar), Population = sum(Population), MedianAge = median(MedianAge), SixtyFivePlus = sum(SixtyFivePlus), Asian = sum(Asian), BachelorsDegree = sum(BachelorsDegree), MeanTravelTime = mean(MeanTravelTime), MedianHouseholdIncome = median(MedianHouseholdIncome)) %>% # Group by County and combine all the county values into one
  distinct() #Due to grouping, rows were repeating and only distinct rows are kept
```

## Demographics Overview

``` r
longerTable <-
  JoinData %>% 
    pivot_longer(cols=c("totalCar", "Population", "MedianAge", "SixtyFivePlus", "Asian", "BachelorsDegree", "MeanTravelTime", "MedianHouseholdIncome"),
                      names_to='Demographic',
                      values_to='Value') # Pivot into a longer format where each chosen deomgraphic is in a list

longerTable %>% 
  pivot_wider(names_from = County, values_from = Value) # Pivot back to a different table that shows the demographics in a county by county basis
```

    ## # A tibble: 8 × 40
    ##   Demographic         Adams Asotin Benton Chelan Clallam  Clark Columbia Cowlitz
    ##   <chr>               <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>    <dbl>   <dbl>
    ## 1 totalCar           4.8 e1 7.3 e1 1.86e3 1.05e3  1058   1.04e4     16     897  
    ## 2 Population         1.77e4 2.03e4 1.55e5 6.58e4 65005   3.55e5   3806   94419  
    ## 3 MedianAge          4   e1 3.94e1 3.38e1 3.49e1    40   3.56e1     42.5    39.4
    ## 4 SixtyFivePlus      1.83e3 3.33e3 1.58e4 9.16e3 13819   3.38e4    716   12350  
    ## 5 Asian              1   e2 1.05e2 3.23e3 4.47e2   737   1.11e4     16    1221  
    ## 6 BachelorsDegree    1.22e3 2.43e3 2.40e4 9.23e3  9524   4.88e4    455    8160  
    ## 7 MeanTravelTime     2.00e1 1.98e1 2.1 e1 2.00e1    22.6 2.65e1     18.7    32.0
    ## 8 MedianHouseholdIn… 3.80e4 3.52e4 4.27e4 3.69e4 34082   4.92e4  32130   40398  
    ## # ℹ 31 more variables: Douglas <dbl>, Ferry <dbl>, Franklin <dbl>,
    ## #   Garfield <dbl>, Grant <dbl>, `Grays Harbor` <dbl>, Island <dbl>,
    ## #   Jefferson <dbl>, King <dbl>, Kitsap <dbl>, Kittitas <dbl>, Klickitat <dbl>,
    ## #   Lewis <dbl>, Lincoln <dbl>, Mason <dbl>, Okanogan <dbl>, Pacific <dbl>,
    ## #   `Pend Oreille` <dbl>, Pierce <dbl>, `San Juan` <dbl>, Skagit <dbl>,
    ## #   Skamania <dbl>, Snohomish <dbl>, Spokane <dbl>, Stevens <dbl>,
    ## #   Thurston <dbl>, Wahkiakum <dbl>, `Walla Walla` <dbl>, Whatcom <dbl>, …

## Cars per Ten Thousand People

``` r
JoinData <-
  JoinData %>%
    mutate(carsPerThousand = (totalCar/Population) * 1000) %>%
    mutate(carsPerThousand = round(carsPerThousand)) #Create a metric called carsPerThousand which is the cars per thousand residents
  print(JoinData)
```

    ## # A tibble: 39 × 10
    ## # Groups:   County [39]
    ##    County   totalCar Population MedianAge SixtyFivePlus Asian BachelorsDegree
    ##    <chr>       <int>      <dbl>     <dbl>         <dbl> <dbl>           <dbl>
    ##  1 Adams          48      17677      40            1835   100            1217
    ##  2 Asotin         73      20330      39.4          3331   105            2428
    ##  3 Benton       1860     154790      33.8         15816  3225           24027
    ##  4 Chelan       1047      65784      34.9          9161   447            9228
    ##  5 Clallam      1058      65005      40           13819   737            9524
    ##  6 Clark       10409     354729      35.6         33773 11139           48770
    ##  7 Columbia       16       3806      42.5           716    16             455
    ##  8 Cowlitz       897      94419      39.4         12350  1221            8160
    ##  9 Douglas       364      40839      40.6          5368   214            3939
    ## 10 Ferry          31      11036      38            1447    30             910
    ## # ℹ 29 more rows
    ## # ℹ 3 more variables: MeanTravelTime <dbl>, MedianHouseholdIncome <dbl>,
    ## #   carsPerThousand <dbl>

``` r
JoinData %>%  
  ggplot(aes(x = carsPerThousand, y = County)) +  # CarsPerThousand vs. County
  geom_point(aes(size = carsPerThousand), alpha = 0.7) + 
  ggtitle("Electric Cars Per Thousand People in Each County") +
  xlab("Cars Per Thousand People") + 
  theme_minimal()
```

<img src="FinalProject_SrikarLanka_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />
This is a fairly introductory graph, yet it is very critical for what I
am trying to do. It is effectively the Cars per Thousand People in each
county, with the size of the point also representing how many cars there
are per person. So, why is this graph so critical? It is what will allow
me to isolate the counties that have a high EV population and a low EV
population to see the demographcs that influence those changes. This is
why this graph is very critical.

<figure>
<img src="PointsofInterest.png" alt="Image of Counties of Interest" />
<figcaption aria-hidden="true">Image of Counties of
Interest</figcaption>
</figure>

### Filter for Conties of Interest

``` r
filteredData <-
  JoinData %>%
    filter(carsPerThousand >= 20 | carsPerThousand <= 5) # Filter for counties of interest

filteredData %>%
  ggplot(aes(x = carsPerThousand, y = County)) + 
  geom_point(aes(size = carsPerThousand), alpha = 0.7) + 
  ggtitle("Electric Cars Per Thousand People in Each County") +
  xlab("Cars Per Thousand People") + 
  theme_minimal() #Replot with only the new counties of interest
```

<img src="FinalProject_SrikarLanka_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />
Using the knowledge provided to us in the first graph, we are now able
to cut down the counties to simply the ones we are interested in. We
have 7 counties where there are more then 20 cars per thousand people
and 9 with less then 5 cars per thousand people. These are the counties
that are the most “extreme” and the hope is we can extract the
demographic makeup that matters from each one.

## Analysis of Age as an Indicator

``` r
filteredData %>%
  ggplot(aes(x = County)) + 
  geom_point(aes(y = MedianAge)) + #Plotting MedianAge by Points 
  geom_boxplot(aes(y = carsPerThousand, color=SixtyFivePlus)) + #Colored Boxplots to represent the carsPerThousand and the number who are 65+ in said county
  ggtitle("Mean Age per County Compared to Cars Per Thousand People", subtitle = "With additional consideration for the population above 65 years") +
  ylab("Number of Value") +
  theme_minimal()
```

<img src="FinalProject_SrikarLanka_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />
There is certainly a lot to see in this graph, but overall it is very
simple yet a great indicator of wether age is a factor when it comes to
Electric Car ownership. Effectively, this is a graph of the Counties,
which each dot representing the median age in the county and each line
representing the cars per thousand people in that county. The lines are
additonally colored to represent the number of people who are 65+ in
each if these counties. Overall, we are able to see that there seems to
be no real correlation in terms of age and EV ownerships. Some of these
counties below 10 cars per thousand people have fairly young populations
yet also low ammount of electric vehicles. Although one could argue
there is a weak correlation, I disagree with this assessment because the
median age in the State of Washington is fairly consistent.

``` r
filteredData %>%
  mutate(percentOverSixtyFive = (SixtyFivePlus/Population)*100) %>% #Adjusting previous visualizaiton for the percentage of population 65+
  ggplot(aes(x = County)) + 
  geom_point(aes(y = MedianAge)) +
  geom_boxplot(aes(y = carsPerThousand, color=percentOverSixtyFive)) +
  ggtitle("Mean Age per County Compared to Cars Per Thousand People", subtitle = "With additional consideration for the percent of the population above 65 years") +
  labs(caption="Point is Mean Age, Line is Cars per Thousand People, Color is 65+ Representing by the Key Per County.") +
  ylab("Number of Value") +
  theme_minimal()
```

<img src="FinalProject_SrikarLanka_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />
This is simply a slight modication to the graph above with the
percentages of the population who are over 65+. This graph definetly
reaffirms my previous assertion related to age being a releative
nonfactor as there are counties that are very young with also low
ammounts of ev ownership.

## Analysis of Mean Travel Time

``` r
filteredData %>%
  ggplot(aes(x = MeanTravelTime)) +
  geom_point(aes(y = carsPerThousand, color = County), shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() + #Plotting the Mean Travel Time vs. the Cars Per Thousand Residents in each county with an added linear line
  geom_smooth(aes(y = carsPerThousand), method=lm) +
  ggtitle("Mean Travel Time Per Thousand People in Each County") +
  xlab("Mean Travel Time (Minutes)") + 
  ylab("Electric Vehicles per Thousand Residents")
```

<img src="FinalProject_SrikarLanka_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />
This is a graph that showcases the Man Travel Time vs the EV per
thousand people rate with each point representing the counties. There is
a linear line added overtop that tries to find the general trend.
Overall, we find a weak positive correlation between the travel time and
the ammount of EVs a county may have. Although this culd be driven by
outliers, I feel this could be a good indicator as the more a county
drives the more they want to save on gas, and thus this could be one
indicator.

## Asian Demographic per County

``` r
filteredData %>%
  mutate(percentAsian = (Asian/Population) *100) %>% #Calculate Asian Percentage per County
  ggplot() +
  aes(x = percentAsian, y = carsPerThousand) + 
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(County)) # Graph of the percentage of asian population vs Cars per Thousand Residents faceted by County
```

<img src="FinalProject_SrikarLanka_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />
This is a graph of the percent of a county that is Asian vs. the Cars
per Thousand people, with each grid representing a specific county.
Overall, I believe there is a correlation between the Asian Population
and the Cars a county has. Outside of San Juan county, every other
county with a asian population of greater then 3% has a fairly high EV
population.

## Education Level

``` r
filteredData %>%
  mutate(percentCollegeGrad = (BachelorsDegree/Population) *100) %>%
  ggplot() +
  aes(
    x = percentCollegeGrad,
    y = County,
    colour = carsPerThousand
  ) +
  geom_boxplot(fill = "#112446") +
  scale_color_gradient() +
  theme_minimal() #Graph showcasing the percentage of a county that is a college graduate with attention to the cars per thousand 
```

<img src="FinalProject_SrikarLanka_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />
This is a graph of the percentage of a county that has a Bachelors
degree or higher, with the lines representing the percentage and its
color representing the cars per thousand people. Overall, I believe
there is a link between the ammount of college educated populace and the
cars, as we see that the lines get lighter the more educated a county
is.

## Income

``` r
filteredData %>%
  mutate(twentyorGreaterCars = ifelse(carsPerThousand >= 20, 20, 0)) %>%
  ggplot() +
  aes(x = MedianHouseholdIncome, fill = County) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_grid(vars(twentyorGreaterCars)) # Graph showcasing the MedianHouseHoldIncome per county with respect to the number of Cars per Thousand Residents 
```

<img src="FinalProject_SrikarLanka_files/figure-gfm/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />
This is a graph of the Median Household Income per county, with the
colors representing the various counties. This graph is split into two
sections, the top graph being counties with less then 20 cars per
thousand people and the bottom being more. We are able to see that
income is certainly an additonal factor considering that the counties
with a income of greater then 40,000 also have more then 20 cars per
thousand people.

# Conclusion

## Challenge Encountered

The project went well, and I am very proud of my work. That being said,
there was one challenge that, although I did eventually overcome in the
end, eventually haunted me from the definitely the project. This
challenge was the nature of the data.

Firstly, due to the data’s complexity (especially that of
zipDemography), the initial joins I tried to do led to N/As when I tried
a join with evData since there were a lot of zip codes that did not
match up as well as I thought. I then had the issue where there were
multiple duplicate rows, each with its own data, though that was
overcome through grouping by county, combining like rows, and only
keeping distinct rows.

However, the true challenge came during graphs. Due to the nature of the
data being simple points rather than a series of data, a lot of graphs I
wanted to do and got accustomed to in the class were not at my disposal.
I had to create a variety of graph combinations that were definitely new
to me, but they provided me with great new insights.

## Final Conclusions

Based on the analysis above, we are able to conclude that there is
likely three key demographics that influence the ammount of electric
vehicles in a county: Income, Education, and the Asian Population. There
is additionally an argument to be made for the Mean Travel Time.
Overall, these findings are not solid, but rather they are able to serve
as a basis for a future quantative or qualitative study. However, for
the sake of my work, I feel confident that these demographics certainly
do potentially play a part in indicators for electric vehicles.

# Code Appendix

``` r
# This template file is based off of a template created by Alex Hayes
# https://github.com/alexpghayes/rmarkdown_homework_template

# Setting Document Options
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.align = "center"
)
library(tidyverse)
library(dcData)
library(janitor)
library(devtools)

library(esquisse)
library(mosaic)
library(mosaicData)
library(ggplot2)
evData <- read.csv("Electric_Vehicle_Population_Data.csv")
demographicData <- ZipDemography %>%
  mutate(ZIP = as.integer(ZIP))   
glimpse(evData)
print("---")
glimpse(demographicData)
print("---")
names(demographicData)
evData <-
  evData %>%
    select("VIN..1.10.", County, City, State, Postal.Code) %>% #Select columns of interest from evData
    filter(State == "WA") %>% # Filter for cars registered in the State of Washington
    rename(VIN = "VIN..1.10.") # Rename VIN to a more usable name
demographicData <-
  demographicData %>%
    select(Totalpopulation, MedianAge, X65yearsandover, Asian, Bachelorsdegreeorhigher, Meantraveltimetoworkinminutespopulation16yearsandolder, Medianhouseholdincomedollars, ZIP) %>% #Select Data of Interest
    rename(Population = Totalpopulation) %>% #Rename Long/Redudndant Names to more usable names
    rename("SixtyFivePlus" = X65yearsandover) %>% 
    rename("BachelorsDegree" = Bachelorsdegreeorhigher) %>% 
    rename("MeanTravelTime" = Meantraveltimetoworkinminutespopulation16yearsandolder) %>% 
    rename("MedianHouseholdIncome" = Medianhouseholdincomedollars) %>% 
    drop_na() #Drop Empty Values
JoinData <- 
  evData %>% 
    group_by(County, Postal.Code) %>%
    summarise(totalCar = n()) %>% #Group_By County and Postal Code and then find the number of cars in each combination
    inner_join(demographicData, by=c("Postal.Code"="ZIP")) %>% #Join with the demographic data on Zip-Codes
    select(-c(Postal.Code)) %>% # Drop the Zip Code Column
    group_by(County) %>%  
    mutate(totalCar = sum(totalCar), Population = sum(Population), MedianAge = median(MedianAge), SixtyFivePlus = sum(SixtyFivePlus), Asian = sum(Asian), BachelorsDegree = sum(BachelorsDegree), MeanTravelTime = mean(MeanTravelTime), MedianHouseholdIncome = median(MedianHouseholdIncome)) %>% # Group by County and combine all the county values into one
  distinct() #Due to grouping, rows were repeating and only distinct rows are kept
longerTable <-
  JoinData %>% 
    pivot_longer(cols=c("totalCar", "Population", "MedianAge", "SixtyFivePlus", "Asian", "BachelorsDegree", "MeanTravelTime", "MedianHouseholdIncome"),
                      names_to='Demographic',
                      values_to='Value') # Pivot into a longer format where each chosen deomgraphic is in a list

longerTable %>% 
  pivot_wider(names_from = County, values_from = Value) # Pivot back to a different table that shows the demographics in a county by county basis
JoinData <-
  JoinData %>%
    mutate(carsPerThousand = (totalCar/Population) * 1000) %>%
    mutate(carsPerThousand = round(carsPerThousand)) #Create a metric called carsPerThousand which is the cars per thousand residents
  print(JoinData)
  
JoinData %>%  
  ggplot(aes(x = carsPerThousand, y = County)) +  # CarsPerThousand vs. County
  geom_point(aes(size = carsPerThousand), alpha = 0.7) + 
  ggtitle("Electric Cars Per Thousand People in Each County") +
  xlab("Cars Per Thousand People") + 
  theme_minimal()
filteredData <-
  JoinData %>%
    filter(carsPerThousand >= 20 | carsPerThousand <= 5) # Filter for counties of interest

filteredData %>%
  ggplot(aes(x = carsPerThousand, y = County)) + 
  geom_point(aes(size = carsPerThousand), alpha = 0.7) + 
  ggtitle("Electric Cars Per Thousand People in Each County") +
  xlab("Cars Per Thousand People") + 
  theme_minimal() #Replot with only the new counties of interest
filteredData %>%
  ggplot(aes(x = County)) + 
  geom_point(aes(y = MedianAge)) + #Plotting MedianAge by Points 
  geom_boxplot(aes(y = carsPerThousand, color=SixtyFivePlus)) + #Colored Boxplots to represent the carsPerThousand and the number who are 65+ in said county
  ggtitle("Mean Age per County Compared to Cars Per Thousand People", subtitle = "With additional consideration for the population above 65 years") +
  ylab("Number of Value") +
  theme_minimal()
filteredData %>%
  mutate(percentOverSixtyFive = (SixtyFivePlus/Population)*100) %>% #Adjusting previous visualizaiton for the percentage of population 65+
  ggplot(aes(x = County)) + 
  geom_point(aes(y = MedianAge)) +
  geom_boxplot(aes(y = carsPerThousand, color=percentOverSixtyFive)) +
  ggtitle("Mean Age per County Compared to Cars Per Thousand People", subtitle = "With additional consideration for the percent of the population above 65 years") +
  labs(caption="Point is Mean Age, Line is Cars per Thousand People, Color is 65+ Representing by the Key Per County.") +
  ylab("Number of Value") +
  theme_minimal()
filteredData %>%
  ggplot(aes(x = MeanTravelTime)) +
  geom_point(aes(y = carsPerThousand, color = County), shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() + #Plotting the Mean Travel Time vs. the Cars Per Thousand Residents in each county with an added linear line
  geom_smooth(aes(y = carsPerThousand), method=lm) +
  ggtitle("Mean Travel Time Per Thousand People in Each County") +
  xlab("Mean Travel Time (Minutes)") + 
  ylab("Electric Vehicles per Thousand Residents")
filteredData %>%
  mutate(percentAsian = (Asian/Population) *100) %>% #Calculate Asian Percentage per County
  ggplot() +
  aes(x = percentAsian, y = carsPerThousand) + 
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(County)) # Graph of the percentage of asian population vs Cars per Thousand Residents faceted by County
filteredData %>%
  mutate(percentCollegeGrad = (BachelorsDegree/Population) *100) %>%
  ggplot() +
  aes(
    x = percentCollegeGrad,
    y = County,
    colour = carsPerThousand
  ) +
  geom_boxplot(fill = "#112446") +
  scale_color_gradient() +
  theme_minimal() #Graph showcasing the percentage of a county that is a college graduate with attention to the cars per thousand 
filteredData %>%
  mutate(twentyorGreaterCars = ifelse(carsPerThousand >= 20, 20, 0)) %>%
  ggplot() +
  aes(x = MedianHouseholdIncome, fill = County) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_grid(vars(twentyorGreaterCars)) # Graph showcasing the MedianHouseHoldIncome per county with respect to the number of Cars per Thousand Residents 
```
