# Load packages ----
    library(tidyr)
    library(stringr)
    library(plyr)
    library(dplyr)

# Checkpoint 1:----
# 1.
    companies <- read.delim("companies.txt",sep = "\t",header = TRUE)
    View(companies)

    rounds2 <- read.csv("rounds2.csv",header = TRUE)
    View(rounds2)


# 2.
# Changing the companies name to lower case
    
    rounds2$company_permalink <- tolower(rounds2$company_permalink)
    companies$permalink <- tolower(companies$permalink)

# How many unique companies are present in rounds2?
    
    length(unique(rounds2$company_permalink))
    
    
# How many unique companies are present in companies?
    
    length(unique(companies$permalink))
    

# Are there any companies in the rounds2 file which are not present in companies? 
    
    length(which(!rounds2$company_permalink %in% companies$permalink))

    
# Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. 
# How many observations are present in master_frame?
    
    master_frame <- merge(x=rounds2,y=companies,by.x ="company_permalink", by.y ="permalink")
    View(master_frame)
    

# Checkpoint 2:----

# Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity) 
    
    avg_funding_type <- as.data.frame(summarise(group_by(subset(master_frame,funding_round_type %in% c("venture","angel","seed","private_equity")),funding_round_type),mean=mean(raised_amount_usd, na.rm = TRUE)))
    avg_funding_type
    
# which investment type is the most suitable for them?
    Based on the number of investor and avergae funding "Venture" is the most suitable one
    
    no_of_investment <- as.data.frame(summarise(group_by(subset(master_frame,funding_round_type %in% c("venture","angel","seed","private_equity") & !(is.na(master_frame$raised_amount_usd) & !(master_frame$country_code == ""))),funding_round_type),count=length(funding_round_type)))
    no_of_investment
    
    
  #Checkpoint 3:----

# Spark Funds wants to see the top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)
# For the chosen investment type, make a data frame named top9 with the top nine countries (based on the total investment amount each country has received)

    Countries_funding <- aggregate(raised_amount_usd~country_code,subset(master_frame,funding_round_type == "venture" & !(country_code == "")),sum)
    top9 <- as.vector(Countries_funding$country_code[order(Countries_funding$raised_amount_usd, decreasing=TRUE)][1:9])
    top9

#Checkpoint 4:----

# Extract the primary sector of each category list from the category_list column
    
    master_frame <- separate(master_frame,category_list,c("primary_sector"),"\\|",remove = FALSE)
    View(master_frame)

# Load the mapping file in to data frame
    mapping <- read.csv("mapping.csv",header = TRUE)
    View(mapping)

# Convert "mapping data" frame from Wide to Long Format
    colnames(mapping) <- c("category_list","Automotive & Sports","Blanks","Cleantech / Semiconductors","Entertainment","Health","Manufacturing","News, Search and Messaging","Others","Social, Finance, Analytics, Advertising")
    mapping <- gather(mapping,main_sector,sector_val,2:10)

    mapping <- filter(mapping,!(sector_val ==0))
    mapping <- mapping[,-3]

# Correct the category_list values in "mapping" data frame

    mapping$category_list <- str_replace_all(mapping$category_list,"0","na")
    mapping$category_list <- str_replace(mapping$category_list,"nanotechnology","Nanotechnology")
    mapping$category_list <- str_replace(mapping$category_list,"natural Language Processing","Natural Language Processing")
    mapping$category_list <- str_replace(mapping$category_list,"natural Resources","Natural Resources")
    mapping$category_list <- str_replace(mapping$category_list,"navigation","Navigation")


# Merge "mapping" Data frame with "master_frame" data frame
    
    master_frame <- merge(x=master_frame,y=mapping,by.x ="primary_sector", by.y ="category_list")
    master_frame <- subset(master_frame,!(primary_sector == ""))
    View(master_frame)


#Checkpoint 5:----

# Three data frames D1, D2 and D3 

#D1
    
  pre_D1 <- subset(master_frame,master_frame$country_code=="USA" & master_frame$funding_round_type == "venture" & between(master_frame$raised_amount_usd,5000000,15000000))
    View(pre_D1)

# Total number of Investments (count)
    sum(table(pre_D1$main_sector))
    
# Number of investment sectorwise    
    sector_count1 <- as.data.frame(summarise(group_by(pre_D1,main_sector),D1_count=length(main_sector)))
    sector_count1
    
# Total amount of investment (USD)
   
    sum(aggregate(raised_amount_usd~main_sector,pre_D1,sum)$raised_amount_usd)
    

# For point 3 (top sector count-wise), which company received the highest investment?

    pre_D1_subset1 <- aggregate(raised_amount_usd~name,subset(pre_D1,pre_D1$main_sector == "Others"),sum)
    D1_company_name <- as.vector(pre_D1_subset1$name[which.max(pre_D1_subset1$raised_amount_usd)])
    D1_company_name

# For point 4 (second best sector count-wise), which company received the highest investment?
    
    pre_D1_subset2 <- aggregate(raised_amount_usd~name,subset(pre_D1,pre_D1$main_sector == "Social, Finance, Analytics, Advertising"),sum)
    D1_company_name2 <- as.vector(pre_D1_subset2$name[which.max(pre_D1_subset2$raised_amount_usd)])
    D1_company_name2

# D1 Data frame
    
    D1 <- join_all(list(master_frame,sector_count1,sector_raised_amount1), by ="main_sector",type = "inner")
    View(D1)

    
#D2

    pre_D2 <- subset(master_frame,master_frame$country_code=="GBR" & master_frame$funding_round_type == "venture" & between(master_frame$raised_amount_usd,5000000,15000000))
    View(pre_D2)
    
# Total number of Investments (count)
    
    sum(table(pre_D2$main_sector))
    
# Number of investment sectorwise 
    
    sector_count2 <- as.data.frame(summarise(group_by(pre_D2,main_sector),D2_count=length(main_sector)))
    sector_count2
    
# Total amount of investment (USD)
    
    sum(aggregate(raised_amount_usd~main_sector,pre_D2,sum)$raised_amount_usd)


# For point 3 (top sector count-wise), which company received the highest investment?
    
    pre_D2_subset1 <- aggregate(raised_amount_usd~name,subset(pre_D2,pre_D2$main_sector == "Others"),sum)
    D2_company_name <- as.vector(pre_D2_subset1$name[which.max(pre_D2_subset1$raised_amount_usd)])
    D2_company_name
    
# For point 4 (second best sector count-wise), which company received the highest investment?
    
    pre_D2_subset2 <- aggregate(raised_amount_usd~name,subset(pre_D2,pre_D2$main_sector == "Social, Finance, Analytics, Advertising"),sum)
    D2_company_name2 <- as.vector(pre_D2_subset2$name[which.max(pre_D2_subset2$raised_amount_usd)])
    D2_company_name2
    
# D2 Data frame    
    
    D2 <- join_all(list(master_frame,sector_count2,sector_raised_amount2), by ="main_sector",type = "inner")
    View(D2)


#D3

    pre_D3 <- subset(master_frame,master_frame$country_code=="IND" & master_frame$funding_round_type == "venture" & between(master_frame$raised_amount_usd,5000000,15000000))
    View(pre_D3)

# Total number of Investments (count)
    
    sum(table(pre_D3$main_sector))
    
# Number of investment sectorwise
    
    sector_count3 <- as.data.frame(summarise(group_by(pre_D3,main_sector),D3_count=length(main_sector)))
    sector_count3
    
# Total amount of investment (USD)   
    
    sum(aggregate(raised_amount_usd~main_sector,pre_D3,sum)$raised_amount_usd)


# For point 3 (top sector count-wise), which company received the highest investment?
    
    pre_D3_subset1 <- aggregate(raised_amount_usd~name,subset(pre_D3,pre_D3$main_sector == "Others"),sum)
    D3_company_name <- as.vector(pre_D3_subset1$name[which.max(pre_D3_subset1$raised_amount_usd)])
    D3_company_name
    
# For point 4 (second best sector count-wise), which company received the highest investment?
    
    pre_D3_subset2 <- aggregate(raised_amount_usd~name,subset(pre_D3,pre_D3$main_sector == "Social, Finance, Analytics, Advertising"),sum)
    D3_company_name2 <- as.vector(pre_D3_subset2$name[which.max(pre_D3_subset2$raised_amount_usd)])
    D3_company_name2
    
# D3 Data frame    
    D3 <- join_all(list(master_frame,sector_count3,sector_raised_amount3), by ="main_sector",type = "inner")
    View(D3)
    

