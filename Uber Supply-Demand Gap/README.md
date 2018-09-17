# Business Understanding
While travelling to and from the airport (incase you'd used Uber or any other cab services), People face the problem of cancellation by the driver or non-availability of cars. Well, if these are the problems faced by customers, these very issues also impact the business of Uber. If drivers cancel the request of riders or if cars are unavailable, Uber loses out on its revenue.

# Business Objectives
The aim of analysis is to identify the root cause of the problem (i.e. cancellation and non-availability of cars) and recommend ways to improve the situation. Also needs to find out the root cause(s) and possible hypotheses of the problem(s) and recommend ways to improve them.

# Data Understanding

There are six attributes associated with each request made by a customer:

  - **Request id:** A unique identifier of the request
  - **Time of request:** The date and time at which the customer made the trip request
  - **Drop-off time:** The drop-off date and time, in case the trip was completed 
  - **Pick-up point:** The point from which the request was made
  - **Driver id:** The unique identification number of the driver
  - **Status of the request:** The final status of the trip, that can be either completed, cancelled by the driver or no cars available
  
  Note: only the trips to and from the airport have been considered.
  
 # Exploratory Data Analysis
 
 * Visually identifed the most pressing problems for Uber. 
  - Created plots to visualise the frequency of requests that get cancelled or show 'no cars available'; identified the most problematic types of requests (city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) using plots
  
 * Find out the gap between supply and demand and show the same using plots.
  - Find the time slots when the highest gap exists
  - Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
  
  
  
