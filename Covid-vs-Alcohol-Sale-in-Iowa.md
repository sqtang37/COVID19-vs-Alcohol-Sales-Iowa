Covid’s effect on Alcohol Sale in Iowa
================
Shuqin Tang
6/8/2022

``` r
library(readr)
library(ggplot2)
library(gridExtra)
library(prettydoc)
library(DT)
library(knitr)
library(kableExtra)
library(pander)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

# 

## 1. Data Preparation & Exploration

``` r
load("final_data.RData")
load("Store.RData")
load("Item.RData")
```

### 1A

``` r
##combine three datasets into one
total <- rbind(yr2016, yr2018, yr2020)

##check missing value
lapply(names(total), function(i) round(prop.table(table(is.na(total[i]), dnn = i)), digit = 2))
```

    ## [[1]]
    ## Invoice/Item Number
    ## FALSE 
    ##     1 
    ## 
    ## [[2]]
    ## Date
    ## FALSE 
    ##     1 
    ## 
    ## [[3]]
    ## Store Number
    ## FALSE 
    ##     1 
    ## 
    ## [[4]]
    ## Category
    ## FALSE  TRUE 
    ##     1     0 
    ## 
    ## [[5]]
    ## Vendor Number
    ## FALSE 
    ##     1 
    ## 
    ## [[6]]
    ## Item Number
    ## FALSE 
    ##     1 
    ## 
    ## [[7]]
    ## State Bottle Retail
    ## FALSE 
    ##     1 
    ## 
    ## [[8]]
    ## Bottles Sold
    ## FALSE 
    ##     1 
    ## 
    ## [[9]]
    ## Sale (Dollars)
    ## FALSE 
    ##     1 
    ## 
    ## [[10]]
    ## Volume Sold (Liters)
    ## FALSE 
    ##     1 
    ## 
    ## [[11]]
    ## Volume Sold (Gallons)
    ## FALSE 
    ##     1

Based on the result, we see that only the column named “category” has NA
value, which is not relevant to what data we need to analyze. Thus, this
column is ignored, and no data are needed to be dropped.

``` r
##change column name
names(total) <- gsub(" ", "_", names(total))

##make columns 7:11 numeric
total$`Sale_(Dollars)` <- gsub("[\\$|,]", "", total$`Sale_(Dollars)`)
total$State_Bottle_Retail <- gsub("\\$", "", total$State_Bottle_Retail)
total[,c(7:11)] <- sapply(total[,c(7:11)], as.numeric)
```

Because the columns from 7th to 11th are counts that can be used in
calculation, it is better to transform their types from char to num.

``` r
##check repeated rows
total[duplicated(total),]
```

    ##  [1] Invoice/Item_Number   Date                  Store_Number         
    ##  [4] Category              Vendor_Number         Item_Number          
    ##  [7] State_Bottle_Retail   Bottles_Sold          Sale_(Dollars)       
    ## [10] Volume_Sold_(Liters)  Volume_Sold_(Gallons)
    ## <0 rows> (or 0-length row.names)

There is no repeated row needed to be clean in this dataset.

``` r
##count numbers of outliers for each year
total$year <- format(total$Date, "%Y")

outlier16 <- total[which(total$year == "2016"),]$`Sale_(Dollars)`
nrow(data.frame(boxplot.stats(outlier16)$out))
```

    ## [1] 241

``` r
outlier18 <- total[which(total$year == "2018"),]$`Sale_(Dollars)`
nrow(data.frame(boxplot.stats(outlier18)$out))
```

    ## [1] 307

``` r
outlier20 <- total[which(total$year == "2020"),]$`Sale_(Dollars)`
nrow(data.frame(boxplot.stats(outlier20)$out))
```

    ## [1] 200

Although there are multiple outliers for sales each year, they are not
dropped because all sales need to be considered as important data in
this dataset.

``` r
##Total_Sales (Dollars)
sum2016 <- sum(total$`Sale_(Dollars)`[which(total$year == "2016")])
sum2018 <- sum(total$`Sale_(Dollars)`[which(total$year == "2018")])
sum2020 <- sum(total$`Sale_(Dollars)`[which(total$year == "2020")])
TS <- data.frame(sale = c(sum2016, sum2018, sum2020))
row.names(TS) <- c(2016, 2018, 2020)
colnames(TS)[1] <- "Total_Sales (Dollars)"
TS[,1] <- format(round(TS[,1], 9), nsmall = 2)
head(TS)
```

    ##      Total_Sales (Dollars)
    ## 2016             565769.81
    ## 2018             669415.37
    ## 2020             476914.54

### 1B

``` r
names(store) <- gsub(" ", "_", names(store))
totalwZip <- merge(total, store, by = "Store_Number")

table2016 <- totalwZip[which(totalwZip$year == 2016), ]
table2018 <- totalwZip[which(totalwZip$year == 2018), ]
table2020 <- totalwZip[which(totalwZip$year == 2020), ]
```

#### (1) Transaction

``` r
##transaction 2016
trans16 <- table(table2016$Zip_Code)
head(names(sort(trans16, decreasing = TRUE)), 10)
```

    ##  [1] "52402" "50010" "52240" "50613" "50401" "51501" "52001" "52807" "50317"
    ## [10] "52404"

``` r
##transaction 2018
trans18 <- table(table2018$Zip_Code)
head(names(sort(trans18, decreasing = TRUE)), 10)
```

    ##  [1] "50010" "52240" "52402" "50265" "52001" "50613" "51501" "52404" "52804"
    ## [10] "52241"

``` r
##transaction 2020
trans20 <- table(table2020$Zip_Code)
head(names(sort(trans20, decreasing = TRUE)), 10)
```

    ##  [1] "50010" "52402" "52001" "50322" "52240" "50702" "52241" "52404" "50265"
    ## [10] "50613"

#### (2) Total Sale

``` r
##total sale 2016
totSale16 <- tapply(table2016$`Sale_(Dollars)`, table2016$Zip_Code, sum)
head(names(sort(totSale16, decreasing = TRUE)), 10)
```

    ##  [1] "50266" "51501" "50320" "52402" "50010" "52240" "52807" "52001" "52241"
    ## [10] "50314"

``` r
##total sale 2018
totSale18 <- tapply(table2018$`Sale_(Dollars)`, table2018$Zip_Code, sum)
head(names(sort(totSale18, decreasing = TRUE)), 10)
```

    ##  [1] "50320" "52240" "50613" "50314" "51501" "52807" "52001" "52402" "52722"
    ## [10] "50010"

``` r
##total sale 2020
totSale20 <- tapply(table2020$`Sale_(Dollars)`, table2020$Zip_Code, sum)
head(names(sort(totSale20, decreasing = TRUE)), 10)
```

    ##  [1] "50322" "52402" "50314" "52722" "52807" "52001" "50320" "50010" "52314"
    ## [10] "52002"

#### (3) Median Sale

``` r
##median sale 2016
medSale16 <- tapply(table2016$`Sale_(Dollars)`, table2016$Zip_Code, median)
head(names(sort(medSale16, decreasing = TRUE)), 10)
```

    ##  [1] "50109" "50628" "50530" "52057" "50435" "50514" "52036" "52571" "52333"
    ## [10] "50421"

``` r
##median sale 2018
medSale18 <- tapply(table2018$`Sale_(Dollars)`, table2018$Zip_Code, median)
head(names(sort(medSale18, decreasing = TRUE)), 10)
```

    ##  [1] "50169" "50435" "50061" "51109" "50261" "50207" "52224" "50048" "50071"
    ## [10] "52227"

``` r
##median sale 2020
medSale20 <- tapply(table2020$`Sale_(Dollars)`, table2020$Zip_Code, median)
head(names(sort(medSale20, decreasing = TRUE)), 10)
```

    ##  [1] "50124" "52031" "50212" "50543" "51453" "52158" "52324" "50129" "51109"
    ## [10] "50854"

### 1C

#### Total Transaction

``` r
tenTotTran16 <- data.frame(head(sort(trans16, decreasing = TRUE), 10))
colnames(tenTotTran16)[2] <- "Total_Transection"
tenTotTran16$year <- rep("2016")

tenTotTran18 <- data.frame(head(sort(trans18, decreasing = TRUE), 10))
colnames(tenTotTran18)[2] <- "Total_Transection"
tenTotTran18$year <- rep("2018")

tenTotTran20 <- data.frame(head(sort(trans20, decreasing = TRUE), 10))
colnames(tenTotTran20)[2] <- "Total_Transection"
tenTotTran20$year <- rep("2020")

totalTran <- rbind(tenTotTran16, tenTotTran18, tenTotTran20)
str(totalTran)
```

    ## 'data.frame':    30 obs. of  3 variables:
    ##  $ Var1             : Factor w/ 15 levels "52402","50010",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Total_Transection: int  125 100 81 80 65 63 63 59 58 56 ...
    ##  $ year             : chr  "2016" "2016" "2016" "2016" ...

``` r
ggplot(totalTran, aes(x = year, y = Total_Transection, fill = year)) + geom_boxplot() + xlab("Year") + ylab("Transection") + ggtitle("Top 10 Transaction by Year")
```

![](Covid-vs-Alcohol-Sale-in-Iowa_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

From the boxplot “Top 10 Transaction by Year”, we can see that there is
no overall trend of sales, as the total sale increases from 2016 to
2018, and decreases from 2018 to 2020.

#### Total Sale

``` r
## Total Sale
tenTotSale16 <- data.frame(head(sort(totSale16, decreasing = TRUE), 10))
colnames(tenTotSale16)[1] <- "Total_Sale"
tenTotSale16$year <- rep("2016")

tenTotSale18 <- data.frame(head(sort(totSale18, decreasing = TRUE), 10))
colnames(tenTotSale18)[1] <- "Total_Sale"
tenTotSale18$year <- rep("2018")

tenTotSale20 <- data.frame(head(sort(totSale20, decreasing = TRUE), 10))
colnames(tenTotSale20)[1] <- "Total_Sale"
tenTotSale20$year <- rep("2020")

totalSale <- rbind(tenTotSale16, tenTotSale18, tenTotSale20)

ggplot(totalSale, aes(x = year, y = Total_Sale, fill = year)) + geom_boxplot() + xlab("Year") + ylab("Total Sales (Dollars)") + ggtitle("Top 10 Total Sales by Year")
```

![](Covid-vs-Alcohol-Sale-in-Iowa_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

From the boxplot “Top 10 Total Sales by Year”, we can see that the
overall trend of sales is the trend of decreasing from 2016 to 2020, as
the largest total sale and tzhe median sale both decrease every two
years.

#### Median Sale

``` r
tenMedSale16 <- data.frame(head(sort(medSale16, decreasing = TRUE), 10))
colnames(tenMedSale16)[1] <- "Median_Sale"
tenMedSale16$year <- rep("2016")

tenMedSale18 <- data.frame(head(sort(medSale18, decreasing = TRUE), 10))
colnames(tenMedSale18)[1] <- "Median_Sale"
tenMedSale18$year <- rep("2018")

tenMedSale20 <- data.frame(head(sort(medSale20, decreasing = TRUE), 10))
colnames(tenMedSale20)[1] <- "Median_Sale"
tenMedSale20$year <- rep("2020")

medianSale <- rbind(tenMedSale16, tenMedSale18, tenMedSale20)

ggplot(medianSale, aes(x = year, y = Median_Sale, fill = year)) + geom_boxplot() + xlab("Year") + ylab("Median Sales (Dollars)") + ggtitle("Top 10 Median Sales by Year")
```

![](Covid-vs-Alcohol-Sale-in-Iowa_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

From the boxplot “Top 10 Median Sale by Year”, we can see that there is
no overall trend of sales, as the total sale decreases from 2016 to
2018, and increases from 2018 to 2020.

### 1D

``` r
vd <- data.frame(sort(table(total$Vendor_Number), decreasing = TRUE))
influentialVD <- item[which(item$`Vendor Number` == vd[1,1]),4]
influentialVD[1]
```

    ## [1] "DIAGEO AMERICAS"

My definition of “influential” is the one that produces the most
invoices (which means it sold the most time, and most people bought it).

### 1E

``` r
## In question 1B, I used tapply to find the top 10 zipcodes with highest total sale and top 10 with highest median sale. They are in line 95-116.
```

## 2. Data Visualization

### 2A

``` r
set.seed(605814491)
Sample <- total[sample(nrow(total), size = 605, replace = FALSE), ]
Sample$month <- format(Sample$Date, "%m")

sample16 <- data.frame(tapply(Sample[which(Sample$year == "2016"),]$`Volume_Sold_(Liters)`, Sample[which(Sample$year == "2016"),]$month, sum))
colnames(sample16)[1] <- "Sum_Volume_Sold"

sample18 <- data.frame(tapply(Sample[which(Sample$year == "2018"),]$`Volume_Sold_(Liters)`, Sample[which(Sample$year == "2018"),]$month, sum))
colnames(sample18)[1] <- "Sum_Volume_Sold"

sample20 <- data.frame(tapply(Sample[which(Sample$year == "2020"),]$`Volume_Sold_(Liters)`, Sample[which(Sample$year == "2020"),]$month, sum))
colnames(sample20)[1] <- "Sum_Volume_Sold"

plot(sample16$Sum_Volume_Sold ~ rownames(sample16), type = "l", xlim = c(1,12), main = "Total Volume Sold (Liters) monthly for each year", xlab = "Month", ylab = "Total Volume Sold (Liters)")
lines(sample18$Sum_Volume_Sold ~ rownames(sample18), col = "blue", lty = 2)
lines(sample20$Sum_Volume_Sold ~ rownames(sample20), col = "red", lty = 3)
legend("topleft", legend = c("2016", "2018", "2020"), lty = 1:3, col=c("black", "blue", "red"))
```

![](Covid-vs-Alcohol-Sale-in-Iowa_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## 3. Data Relations

### 3. Data Relations

To evaluate how Covid-19 effected alcohol sales in Iowa, I randomly
extracted a decent amount of data and compared the total volume sold in
liter each month in 2016, 2018, and 2020 from these data. Comparing data
from these three years was because they represented pre-pandemic and
post-pandemic era. I used total volume sold instead of sale price in
dollar because the alcohol prices might increase during the Covid era,
which could not represent the effect of Covid-19 on alcohol sales. From
the graphics in 2A, we see that the overall trend of volume sold
decreases in 2020. Most of the sum of the volume sold for each month in
2020 are lower than other two years. There is also no obvious peak in
2020, while there is a few in 2016 and 2018.

The lower-than-usual alcohol sales in 2020 is mainly due to the effect
of Covid-19. Because of Covid-19, the country declared national
emergency. Many people had lower income than before, or even lost their
income sources. The economy of the country became worse. Along with
restricted alcohol import from other countries because of Covid-19, the
price of alcohols increased. These made them unable to afford as many as
alcohols than before. Another possible reasons for low alcohol sales is
that people were locked down (quarantine) in their houses. They were
unable to go out and buy alcohols. Some liquor shops were not able to
open at that time. Thus, the effect of Covid-19 on the alcohol sales
were in a great extent.
