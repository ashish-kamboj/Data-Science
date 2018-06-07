## Business understanding:

“Global Mart” is an online store super giant having worldwide operations. It takes orders and delivers across the globe and deals with all the major product categories - consumer, corporate & home office.

## Business Problem:

Now they wanted to finalise the plan for the next 6 months i.e. Need sales and demand forecast for the next 6 months, that would help them to manage the revenue and inventory accordingly.
The store caters to 7 different market segments and in 3 major categories and wanted to forecast at this granular level, so the data had to be subseted into 21 (7*3) buckets before analysing the data.
But not all of these 21 market buckets were of importance from the store’s point of view. So they wanted to found out the 2 most profitable (and consistent) segment from these 21 and forecast the sales and demand for these segments.

## Data preparation:

1. Segmented the whole dataset into the 21 subsets based on the market and the customer segment level. 
2. Convert the transaction-level data into a time series. Aggregate the 3 attributes  - Sales, Quantity & Profit, over the Order Date to arrive at monthly    values for these attributes.Then I got the three time-series
3. Found the 2 most profitable and consistently profitable segments. Used the coefficient of variation for finding out the profitable segments among 21    market segments.
4. Smoothen the data (require for classical decomposition).

## Model building:

1.Separate out the last 6 months values from the dataset
2. Build the model using-
  - Classical decomposition
    - Fitted a multiplicative or additive model with trend and seasonality to the data, to find the global value of series
    - Subtracted the global values from the series
    - Modelled the residual series using the ARMA model, to get the local predictible series
    - Subracted the local predictible part from the residual series, we should be left with the noise
    - To check for the white noise performed ADF and KPSS tests

  - Auto ARIMA methodfor forecasting
    - Build a ARIMA model
    - Performed ADF and KPSS tests for checking white nois

## Model evaluation:
1. Forecasted the sales and quantity for the next 6 months using the above build models using MAPE.
