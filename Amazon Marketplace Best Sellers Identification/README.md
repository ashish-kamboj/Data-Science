## Description
Dataset of publicly available info on ecommerce sellers in the Garden category in the Amazon marketplace.

## Objective
To sanitize and analyze the data to profile the sellers present, and develop selection criteria to identify the best or most promising sellers in this dataset, in order to reach out to them.

## Data Description

## Data Preparation
 - **sellerproductcount** - gives you the count of products in the form '1-16 of over 100,000 results' , parse out the product count 100,000 .
 - **sellerratings** - Parse the % and count of positive ratings (e.g. 88% positive in the last 12 months (118 ratings))
 - **sellerdetails** - Parse out phone numbers, and email IDs of merchants, where available
 - **businessaddress** - To get the business locations of the sellers. Parse them to identify if a seller is registered in the US , Germany (DE), or China (CN). Not considering Chinese sellers at this point, so you excluded those sellers from analysis.
 - **Hero Product 1 #ratings and Hero Product 2 #ratings** - these 2 columns provides the number of ratings of the 2 'hero products' or best selling products of this seller.
