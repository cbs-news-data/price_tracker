# Load necessary libraries
library(tidyverse)
library(stringr)
library(xml2)


#US GAS PRICES

gas_prices <- read.csv("data/gas_prices.csv")

#get min and max labels
date_max <- max(gas_prices$date)
date_min <- min(gas_prices$date)
value_max <- max(gas_prices$U.S.)
value_min <- min(gas_prices$U.S.)

#get pretty max date
date_max_pretty <- format(max(as.Date(gas_prices$date)), "%b %d, %Y")

#simplify to "label" and "value" column + "showLabel" + showValue" + "valueToShow" column...change columns 
gas_prices_us_only <- gas_prices %>% 
  select(date, `U.S.`) %>% 
  rename(label = date,
         value = `U.S.`) %>% 
  mutate(showLabel = case_when(label == date_max ~ "1",
                               label == date_min ~ "1",
                               TRUE ~ "0")) %>% 
  mutate(showValue = case_when(value == value_max ~ "1",
                               value == value_min ~ "1",
                               label == date_max ~ "1",
                               TRUE ~ "0")) %>% 
  mutate(label = format(as.Date(label), "%b %d, %Y")) %>% 
  mutate(label = str_replace_all(label, " 0", " ")) %>% 
  mutate(valueToShow = paste0(label, ": $", value))

#convert US data to XML

#variables 
xml_title <- "Weekly gas prices"
xml_subtitle <- "Per gallon"
xml_xaxis <- " " #labels for x axis, only fill out in necessary
xml_yaxis <- " " #labels for x axis, only fill out in necessary
xml_source <- "Energy Information Administration"
xml_date <- paste0("As of ", date_max_pretty)
xml_type <- "line" #line, bar, pie, etc
xml_qualifier <- " " #one line note, if needed


# Create chart node
gas_prices_chart <- xml_new_root("chart")

#add children (title, subtitle, type)
xml_add_child(gas_prices_chart, "title", xml_title)
xml_add_child(gas_prices_chart, "subtitle", xml_subtitle)
xml_add_child(gas_prices_chart, "type", xml_type)
xml_add_child(gas_prices_chart, "x-axis", xml_xaxis)
xml_add_child(gas_prices_chart, "y-axis", xml_yaxis)

# Add data rows
for (i in 1:nrow(gas_prices_us_only)) {
  row_node <- xml_add_child(gas_prices_chart, "dataPoint")
  for (col_name in names(gas_prices_us_only)) {
    xml_add_child(row_node, col_name, as.character(gas_prices_us_only[i, col_name]))
  }
}


xml_add_child(gas_prices_chart, "source", xml_source)
xml_add_child(gas_prices_chart, "date", xml_date)
xml_add_child(gas_prices_chart, "qualifier", xml_qualifier)


# Write XML to file
write_xml(gas_prices_chart, "data/xml/weekly_gas_prices.xml")





#US THINGS PRICES

prices_pivot <- read.csv("data/prices_pivot.csv")

#eggs


#get month
prices_pivot_month <- prices_pivot$periodName[1]

#simplify to "label" and "value" column + "showLabel" + showValue" + "valueToShow" column...change columns 
prices_pivot_eggs_only <- prices_pivot %>% 
  arrange(year) %>% 
  select(year, DOZEN.EGGS) %>% 
  rename(label = year,
         value = DOZEN.EGGS) %>% 
  mutate(showLabel = 1) %>% 
  mutate(showValue = 1) %>% 
  mutate(valueToShow = paste0("$", round(as.numeric(value), digits=2)))

#convert data to XML

#variables 
xml_title <- "Price of eggs"
xml_subtitle <- "Last month compared to prior years"
xml_xaxis <- " " #labels for x axis, only fill out in necessary
xml_yaxis <- " " #labels for x axis, only fill out in necessary
xml_source <- "Bureau of Labor Statistics"
xml_date <- paste0("Through the end of ", prices_pivot_month)
xml_type <- "bar" #line, bar, pie, etc
xml_qualifier <- " " #one line note, if needed


# Create chart node
egg_prices_chart <- xml_new_root("chart")

#add children (title, subtitle, type)
xml_add_child(egg_prices_chart, "title", xml_title)
xml_add_child(egg_prices_chart, "subtitle", xml_subtitle)
xml_add_child(egg_prices_chart, "type", xml_type)
xml_add_child(egg_prices_chart, "x-axis", xml_xaxis)
xml_add_child(egg_prices_chart, "y-axis", xml_yaxis)

# Add data rows
for (i in 1:nrow(prices_pivot_eggs_only)) {
  row_node <- xml_add_child(egg_prices_chart, "dataPoint")
  for (col_name in names(prices_pivot_eggs_only)) {
    xml_add_child(row_node, col_name, as.character(prices_pivot_eggs_only[i, col_name]))
  }
}


xml_add_child(egg_prices_chart, "source", xml_source)
xml_add_child(egg_prices_chart, "date", xml_date)
xml_add_child(egg_prices_chart, "qualifier", xml_qualifier)


# Write XML to file
write_xml(egg_prices_chart, "data/xml/prices_eggs.xml")

