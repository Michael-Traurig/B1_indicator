# B1_indicator
Functions for the proposed B1 headline indicator in the GBF

Functions are seperated into 3 sections.
1. Data wrangling which assists in tranforming raw accounts data into the format required by the indicator function
2. Calculation of the index, with options depending on the level of disaggregation of data provided
3. A plotting function to display the indicator with its sub indices.

**Data wrangling**

Functions have been provided to ensure the accounts data is in the correct format to produce the indicator.

First, accounts data must be in Long format with the following Columns:
  Year, which indicates the year of the data point
  ES, Which indicates the name of the ecosystem service
  ES_category, which indicates if the service is "Provisioning", "Regulating", or "Cultural"
  Ecosystem, which indicates the ecosystem type (this is only applicable where indvidual ecosystem contributions are disaggregated
  Value, which is the raw value of the ecosystem service
Second, although the chain method allows aggregation of services with differing time series lengths, data gaps must not be present within a time series. 

The following functions are provided to assist in preparing the data

Add_ES_categories: This will create a new categorical variable which assigns, based on user input, services to their respective broad ES categories (Provisoning, Regulating, Cultural)

Long_pivot_years: Commonly accounts have each year of data seperated by columns, this function will adjust the data to long format

Long_pivot_ecosystems: In cases where accounts have been produced with ecosystem contributions as seperate columns, this function will adjust the data to long format

Long_pivot_ecoservices: In cases where ecosystem services are seperated by columns, this function will adjust the data to long format

Uniform_variables: This function will ensure the variable names in the data match what is requried of the indicator function.

clean_up_datagap: This function will interpolate missing data point within a time series. Note that filling missing data comes with assumptions, longer time gaps where multiple concecutive years are missing carries with it increasing uncertainty in the validity of interpolated data.

**Example of cleaning data**

cleaned_data <- long_pivot_years(data, '2000', '2023') %>% # Years run from 2000 - 2023 and are converted from columns to long format
                Add_ES_categories(ES_type = 'ES', # ES is the name of the variable with ecosystem service names in the data
                Prov_types = c("Food and feed",   # Provisioning services
                               "Timber",
                               "Water provisioning"),
                Reg_types  = c("Carbon sequestration", # Regulating services
                               "Water quality regulation"),
                Cult_types = c("Nature recreation", #Cultural services
                               "Aesthetic value")) %>%
                Uniform_variables(Year = 'year',
                                  Ecosystem = 'Land type',
                                  ES = 'service',
                                  ES_category = 'ES type',
                                  Value = 'amount') %>%
                Clean_up_datagap(Ecosystem_disag = TRUE)

**Use of the Indicator function**

The indicator function has several options. By default it is assumed that the combined index will be the aggregation of sub indices, however an option has been provided to create the combined index by aggregating all individual ES services. This can be adjusted by adding the argument **Aggregate_sub = FALSE**. Similarly when Ecosystem contributions are disaggregated in the data, by default the index will aggregate ecosystem contributions to specific services first, and then aggregate services. An option has been left to treat each service from each ecosystem as an individual service which can be adjusted by adding the argument **Aggregate_eco = FALSE**.

**Example use of function**

B1_chain_analysis(data = accounts_data, #Data set
                  ecosystem_disag = TRUE, #Data is disaggregated by ecosystem type
                  Aggregate_sub = TRUE, #Sub indices will be aggregated to make full index
                  Aggregate_eco = TRUE) #Ecosystem contributions will be aggregated through a geometric mean before aggregating services

This will produce a dataframe with Year, index (which is the index value) and Category (which describes which sub index the data referes to. Combined index is denoted as "All ES"

**Plotting the data**

A Function has been provided to plot the indices. The function works directly with the accounts data and will both calculate the index and produce the plot. In addition to the arguments used in the B1_chain_analysis function, the plot_chain_analysis function also allows the user to choose which colours the indices will be displayed (in order: combined index, provisioning, regulating, cultural), the y limits for the graph, and a title.

**Example use of the function**

plot_chain_analysis(data = accounts_data,
                    ecosystem_disag = TRUE,
                    Aggregate_sub = TRUE,
                    Aggregate_eco = TRUE,
                    title = "test", #Title of the plot
                    palette = c("Black", "red", "blue", "darkgreen"), #Colour palette for the different indices
                    ylimits = c(0.6,1.63)) #Y axis limits
