## Project Brief:
Spark Funds, an asset management company wants to make investments in a few companies. Spark Funds wants to understand the global trends in investments so that they can take the investment decisions effectively.

## Business and Data Understanding:
Spark Funds has two minor constraints for investments:

1. It wants to invest between 5 to 15 million USD per round of investment
2. It wants to invest only in English-speaking countries because of the ease of communication with the companies it would invest in
3. For your analysis, consider a country to be English speaking only if English is one of the official languages in that country
4. You may use this list: Click here for a list of countries where English is an official language.


### What is the strategy?

Spark Funds wants to invest where most other investors are investing. This pattern is often observed among early stage startup investors.
 
### What is Spark Funds’ business objective?

The business objectives and goals of data analysis are pretty straightforward.

* Business objective: The objective is to identify the best sectors, countries, and a suitable investment type for making investments. The overall strategy is to invest where others are investing, implying that the best sectors and countries are the ones where most investments are happening.

* Goals of data analysis:
	- Investment type analysis: Understanding investments in venture, seed/angel, private equity categories, etc. so Spark Funds can decide which type is best suited for its strategy.
	- Country analysis: Understanding which countries have had the most investments in the past. These will be Spark Funds’ favourites as well.
	- Sector analysis: Understanding the distribution of investments across the eight main sectors. (Note that we are interested in the eight main sectors provided in the mapping file. The two files — companies and rounds2 — have numerous sub-sector names; hence, you will need to map each sub-sector to its main sector.)
 
## Data:

1. **Company details**
companies: A table with basic data of companies

- Permalink:		Unique ID of company
- name: 		    Company name
- homepage_url: Website URL
- category_list:Category/categories to which a company belongs
- status: 		  Operational status
- country_code: Country Code 
- state_code:		State

2. **Funding round details**

rounds2: The most important parameters are explained below:

- company_permalink:	    Unique ID of company
- funding_round_permalink:Unique ID of funding round
- funding_round_type:	    Type of funding – venture, angel, private equity etc.
- funding_round_code:	    Round of venture funding (round A, B etc.)
- funded_at:              Date of funding
- raised_amount_usd:	    Money raised in funding (USD)

3. **Sector Classification**
mapping.csv: This file maps the numerous category names in the companies table (such 3D printing, aerospace, agriculture, etc.) to eight broad sector names. The purpose is to simplify the analysis into eight sector buckets, rather than trying to analyse hundreds of them.


### Checkpoint 1: Data Cleaning

- How many unique companies are present in rounds2?
- How many unique companies are present in companies?
- In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
- Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
- Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame?

JOin all the tables

### Checkpoint 2: Funding Type Analysis


