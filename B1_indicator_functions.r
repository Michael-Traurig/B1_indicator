# Functions to calculate B1 headline indicator for GBF
# Author: "Michael Traurig"

### 1. Data wrangling functions###

# Function to add ES categories to the data
Add_ES_categories = function(data, # Accounts data frame
                             Prov_types, # List of provisioning ES
                             Reg_types, # List of regulating ES
                             Cult_types, # List of cultural ES
                             ES_type){ # Column name of ES type
  data <- data %>%
    mutate(ES_category = case_when(
      .data[[ES_type]]  %in% Prov_types ~ "Provisioning",
      .data[[ES_type]]  %in% Reg_types ~ "Regulating",
      .data[[ES_type]]  %in% Cult_types ~ "Cultural",
      .default= "Other" # If ES not listed in above categories they will be dropped into a category called Other
    ))
  return(data)
}

# If accounts have each ecosystem contribution as a seperate column, this function will pivot the data to long format
# Note that the function will only work if ecosystem columns are consecutive
Long_pivot_ecosystems = function(data, # Dataset
                                 Start_col, # First column of ecosystem data
                                 End_col){ # Last column of ecosystem data
  data_long <- data %>%
    pivot_longer(cols = all_of(Start_col):all_of(End_col), names_to = "Ecosystem", values_to = "Value") %>%
    mutate(Ecosystem = as.factor(Ecosystem))
  return(data_long)
}

# If accounts have each ecosystem service as a seperate column, this function will pivot the data to long format
# Note that the function will only work if ecosystem service columns are consecutive
Long_pivot_ecoservices = function(data, # Dataset
                                  Start_col, # First column of ecosystem service data
                                  End_col){ # Last column of ecosystem service data
  data_long <- data %>%
    pivot_longer(cols = all_of(Start_col):all_of(End_col), names_to = "Eco_service", values_to = "Value") %>%
    mutate(Year = as.factor(Eco_service))
  return(data_long)
}

# If accounts have each year as a seperate column, this function will pivot the data to long format
# Note that the function will only work if year columns are consecutive
Long_pivot_years = function(data, # Dataset
                            Start_col, # First column of year data
                            End_col){ # Last column of year data
  data_long <- data %>%
    pivot_longer(cols = all_of(Start_col):all_of(End_col), names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.numeric(Year))
  return(data_long)
}

# If there is a need to convert data back into a standard accounts format (i.e. each year as a column), this function will pivot the data to wide format
Wide_pivot_years = function(data, # Dataset
                            Year = Year, # Column name of year data
                            propdata){ # Column name of service values
  data_wide <- data %>%
    pivot_wider(names_from = Year, values_from = propdata)
  return(data_wide)
}

# Function to ensure variables are named correctly for the B1 headline indicator functions
Uniform_variables = function(data, # Dataset
                             Year, # Column name of year data
                             Ecosystem = FALSE, # If ecosystem data is included name of ecosystem column
                             ES, # Column name of ES names
                             ES_category, # Column name of ES categories (prov, reg, cult)
                             Value){ # Column name of ES values
  if(Ecosystem == FALSE){
    data <- data %>%
    rename(Year = {{Year}}, ES = {{ES}}, ES_category = {{ES_category}}, Value = {{Value}})
  }else{
    data <- data %>%
    rename(Year = {{Year}}, Ecosystem = {{Ecosystem}}, ES = {{ES}}, ES_category = {{ES_category}}, Value = {{Value}})
  }
  return(data)
}

# Function to interpolate missing values
# Note this function assumes that the data is pre prepared and in the correct format as per the functions above.
# This function will interpolate missing values within a time series but does not fill in the first or last missing values.
clean_up_datagap <- function(data, Ecosystem_disag = FALSE) {
impute_missing_values <- function(data, year_column, value_column) {
  require(dplyr)
  data <- data %>% arrange(!!sym(year_column))
  for (i in 2:(nrow(data) - 1)) {
    if (is.na(data[[value_column]][i]) || data[[value_column]][i] == 0) {
      p <- i - 1
      s <- i + 1
      while (is.na(data[[value_column]][s]) && s < nrow(data)) {
        s <- s + 1
      }
      if (!is.na(data[[value_column]][p]) && !is.na(data[[value_column]][s]) && data[[value_column]][p] != 0 && data[[value_column]][s] != 0) {
        data[[value_column]][i] <- data[[value_column]][p] * ((data[[value_column]][s] / data[[value_column]][p]) ^ ((data[[year_column]][i] - data[[year_column]][p]) / (data[[year_column]][s] - data[[year_column]][p])))
      }
    }
  }
  return(data)
}

# Split data into a list of data frames
if(Ecosystem_disag == TRUE){
  data_list <- split(data, list(data$Ecosystem, data$ES))
} else{
  data_list <- split(data, list(data$ES))
}
# Apply impute_missing_values() to each data frame
for(i in 1:length(data_list)){
  data_list[[i]] <- impute_missing_values(data_list[[i]], 'Year', 'Value')
}

# Combine results back into a single data frame
imputed_data <- bind_rows(data_list)
return(imputed_data)
}


### 2. B1 headline indicator functions#
#Note this function assumes that the data is pre prepared and in the correct format as per the functions above.
#Ecosystem service values must be in their raw format, the function will calculate proportional change and then apply the chain index method

B1_chain_analysis = function(data, #Input dataset
                             ecosystem_disag = FALSE, #Set to true if data has ecosystems disaggregated
                             sum_eco = FALSE, #Set to true if you want to sum up ecosystem data
                             comb_output = TRUE, #Set to false if you want the output of ecosystem data to be separate
                             Aggregate_eco = TRUE, #Set to TRUE if you want to aggregate Ecosystem data
                             dataout = FALSE, #Set to true if you want to return proportional data without calculating the index
                             Aggregate_sub = TRUE) { #Set to false if you want to calculate the combined index by all ES individually
  gmean <- function(x) {exp(mean(log(x)))}
  Calc_inter_prop <- function(Value) {
    return((Value / lag(Value, 1)))
  }
#Calculate indicies function
calc_index <- function(data, #Input dataset
                       ecosystem_disag, #Set to true if data has ecosystems disaggregated
                       sum_eco, #Set to true if you want to sum up ecosystem data
                       comb_output, #Set to false if you want the output of ecosystem data to be separate
                       Aggregate_eco, #Set to TRUE if you want to aggregate Ecosystem data
                       dataout) { #Set to true if you want to return proportional data without calculating the index
    if (ecosystem_disag == TRUE) {
      if (sum_eco == FALSE) {
        if (dataout == TRUE & comb_output == FALSE) { 
          df <- data %>%
            group_by(ES, ES_category, Ecosystem) %>%
            arrange(Year) %>%
            mutate(
              first_non_na_index = which.min(is.na(Value)),
              all_na = if_else(all(is.na(Value)), 0, 1),
              first_year_value = Value[first_non_na_index],
              first_year = Year[first_non_na_index]
              ) %>%
              mutate(
                proportional_change = if_else(Year == first_year, 1, Calc_inter_prop(Value))
              ) %>%
              ungroup()
            return(df)
        }
        df <- filter(data, !is.na(Value)) %>%
          group_by(ES, ES_category, Ecosystem) %>%
          arrange(Year) %>%
          mutate(
            first_year_value = first(Value),
            first_year = first(Year)
          ) %>%
          filter(!is.na(Value)) %>%
          mutate(
            proportional_change = if_else(Year == first_year, 1, Calc_inter_prop(Value))
          ) %>%
          ungroup()
      } else {
        df <- filter(data, !is.na(Value)) %>%
        group_by(ES, ES_category, Year) %>%
        summarize(Value = sum(Value)) %>%
        ungroup()
      df <- df %>%
        group_by(ES, ES_category) %>%
        arrange(Year) %>%
        mutate(
          first_year_value = first(Value),
          first_year = first(Year)
        ) %>%
        filter(!is.na(Value)) %>%
        mutate(
          proportional_change = if_else(Year == first_year, 1, Calc_inter_prop(Value))
        ) %>%
        ungroup()
      if (dataout == TRUE) {
        return(df)
      }
    }
  } else {
      if (dataout == TRUE) {
        df <- filter(data, !is.na(Value)) %>%
          group_by(ES, ES_category) %>%
          arrange(Year) %>%
          mutate(
            first_non_na_index = which.min(is.na(Value)),
            all_na = if_else(all(is.na(Value)), 0, 1),
            first_year_value = Value[first_non_na_index],
            first_year = Year[first_non_na_index]
          ) %>%
          mutate(
            proportional_change = if_else(Year == first_year, 1, Calc_inter_prop(Value))
          ) %>%
          ungroup()
        return(df)
      }
      df <- filter(data, !is.na(Value)) %>%
        group_by(ES, ES_category) %>%
        arrange(Year) %>%
        mutate(
          first_year_value = first(Value),
          first_year = first(Year)
        ) %>%
        filter(!is.na(Value)) %>%
        mutate(
          proportional_change = if_else(Year == first_year, 1, Calc_inter_prop(Value))
        ) %>%
        ungroup()
    }
  if (Aggregate_eco == FALSE | ecosystem_disag == FALSE) {
    output <- df %>% group_by(Year) %>%
      summarise(
        dt = gmean(proportional_change)
      ) %>%
      mutate(index = NA) %>%
      ungroup()

      output$index[1] <- 1
      for (i in 1:(nrow(output) - 1)) {
        t <- i + 1
        output$index[t] <- output$index[i] * output$dt[t]
      }
    return(output)
  } else {
    if (comb_output == TRUE & ecosystem_disag == TRUE) {
      output <- df %>% group_by(ES, Year) %>%
        summarise(
          dt1 = gmean(proportional_change)
        ) %>%
        ungroup() %>%
        group_by(Year) %>%
        summarise(
          dt = gmean(dt1)
        ) %>%
        mutate(index = NA) %>%
        ungroup()
      output$index[1] <- 1
      for (i in 1:(nrow(output) - 1)) {
        t <- i + 1
        output$index[t] <- output$index[i] * output$dt[t]
      }
      return(output)
    } else {
      stage1 <- df %>% group_by(ES, Year) %>%
        summarise(
          dt1 = gmean(proportional_change)
        ) %>%
        mutate(index = NA) %>%
        ungroup()
      Combined_data <- data.frame()
      ecosystems <- unique(stage1$Ecosystem)
      for (j in 1:length(ecosystems)) {
        stage2 <- stage1 %>% filter(Ecosystem == ecosystems[j]) %>%
          group_by(Ecosystem, Year) %>%
          summarise(
            dt = gmean(dt1)
          ) %>%
          mutate(index = NA) %>%
          ungroup()
        stage2$index[1] <- 1
        for (i in 1:(nrow(stage2) - 1)) {
          t <- i + 1
          stage2$index[t] <- stage2$index[i] * stage2$dt[t]
        }
        Combined_data <- rbind(Combined_data, stage2)
      }
      return(Combined_data)
    }
  }
  }

  if(dataout == TRUE) {
    df <- calc_index(data = data, ecosystem_disag = ecosystem_disag, sum_eco = sum_eco, comb_output = comb_output, Aggregate_eco = Aggregate_eco, dataout = dataout)
    return(df)
  } else {
    if (Aggregate_sub == TRUE) {
    index <- calc_index(data = data, ecosystem_disag = ecosystem_disag, sum_eco = sum_eco, comb_output = comb_output, Aggregate_eco = Aggregate_eco, dataout = dataout) %>%
    mutate(Category = "All ES")
    df_prov <- calc_index(data = data %>% filter(ES_category == "Provisioning"), ecosystem_disag = ecosystem_disag, sum_eco = sum_eco, comb_output = comb_output, Aggregate_eco = Aggregate_eco, dataout = dataout) %>%
    mutate(Category = "Provisioning")
    df_reg <- calc_index(data = data %>% filter(ES_category == "Regulating"), ecosystem_disag = ecosystem_disag, sum_eco = sum_eco, comb_output = comb_output, Aggregate_eco = Aggregate_eco, dataout = dataout) %>%
    mutate(Category = "Regulating")
    df_cult <- calc_index(data = data %>% filter(ES_category == "Cultural"), ecosystem_disag = ecosystem_disag, sum_eco = sum_eco, comb_output = comb_output, Aggregate_eco = Aggregate_eco, dataout = dataout) %>%
    mutate(Category = "Cultural")
    df_grouped <- full_join(index, df_prov, by = "Year") %>%
      full_join(df_reg, by = "Year") %>%
      full_join(df_cult, by = "Year") %>%
      group_by(Year) %>%
      summarise(ave = gmean(na.omit(c(dt.y, dt.x.x, dt.y.y)))) %>%
      mutate(index = NA) %>%
        ungroup()

    df_grouped$index[1] <- 1
    for(i in 1:length(df_grouped$index) - 1) {
        t <- i + 1
        df_grouped$index[t] <- ifelse(i == 0, 1, df_grouped$index[i] * (df_grouped$ave[t]))
      }
    df_grouped <- rename(df_grouped, dt = ave)
    df_grouped <- mutate(df_grouped, Category = "All ES")
    df_all <- rbind(df_grouped, df_prov) %>% rbind(df_cult) %>% rbind(df_reg)
    return(df_all)
    } else {
    df_index <- calc_index(data = data, ecosystem_disag = ecosystem_disag, sum_eco = sum_eco, comb_output = comb_output, Aggregate_eco = Aggregate_eco, dataout = dataout) %>%
    mutate(Category = "All ES")
    df_prov <- calc_index(data = data %>% filter(ES_category == "Provisioning"), ecosystem_disag = ecosystem_disag, sum_eco = sum_eco, comb_output = comb_output, Aggregate_eco = Aggregate_eco, dataout = dataout) %>%
    mutate(Category = "Provisioning")
    df_reg <- calc_index(data = data %>% filter(ES_category == "Regulating"), ecosystem_disag = ecosystem_disag, sum_eco = sum_eco, comb_output = comb_output, Aggregate_eco = Aggregate_eco, dataout = dataout) %>%
    mutate(Category = "Regulating")
    df_cult <- calc_index(data = data %>% filter(ES_category == "Cultural"), ecosystem_disag = ecosystem_disag, sum_eco = sum_eco, comb_output = comb_output, Aggregate_eco = Aggregate_eco, dataout = dataout) %>%
    mutate(Category = "Cultural")
    df_comb <- rbind(df_index, df_prov) %>% rbind(df_cult) %>% rbind(df_reg)
    return(df_comb)
    }
  }
}


### 3. Plotting function #
plot_chain_analysis = function(data,
                               title,
                               palette,
                               ylimits = NULL,
                               ecosystem_disag = FALSE,
                               Aggregate_eco = FALSE,
                               comb_output = TRUE,
                               sum_eco = FALSE,
                               Aggregate_sub = TRUE,
                               breaks = NULL,
                               yaxisbreak = NULL){

index_calc <- data %>% B1_chain_analysis(ecosystem_disag = ecosystem_disag, Aggregate_eco = Aggregate_eco, comb_output = comb_output, sum_eco = sum_eco)

if(is.null(breaks)){
  yearbreak <- unique(index_calc$Year)
} else {
  yearbreak <- breaks
}

      plot <-  ggplot() +
        geom_line(data = filter(index_calc, Category == "All ES"), aes(x = Year, y = index, color = Category, group = 1), linewidth = .6) +
        geom_point(data = filter(index_calc, Category == "ALL_ES"), aes(x = Year, y = index, color = Category, group = 1), size = 1) +
        geom_line(data = filter(index_calc, Category == "Provisioning"), aes(x = Year, y = index, color = Category, group = 1), linewidth = .6) +
        geom_point(data = filter(index_calc, Category == "Provisioning"), aes(x = Year, y = index, color = Category, group = 1), size = 1) +
        geom_line(data = filter(index_calc, Category == "Regulating"), aes(x = Year, y = index, color = Category, group = 1), linewidth = .6) +
        geom_point(data = filter(index_calc, Category == "Regulating"), aes(x = Year, y = index, color = Category, group = 1), size = 1) +
        geom_line(data = filter(index_calc, Category == "Cultural"), aes(x = Year, y = index, color = Category, group = 1), linewidth = .6) +
        geom_point(data = filter(index_calc, Category == "Cultural"), aes(x = Year, y = index, color = Category, group = 1), size = 1) +
        labs(x = "Year", y = "Index", title = title) +
        scale_x_continuous(breaks = yearbreak) +
#        scale_y_continious(breaks = yaxisbreak) +
        scale_color_manual(values = palette,
                           labels = c("All ES", "Provisioning", "Regulating", "Cultural"),
                           breaks = c("All ES", "Provisioning", "Regulating", "Cultural")) +
#        ylim(ylimits) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 15),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 10),
              title = element_text(size = 20),
              legend.margin = margin(t = -10))
      return(plot)
      break
    }


