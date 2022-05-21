library(tidyverse)

#Reading raw file
states_data = read_csv("https://raw.githubusercontent.com/paulh777/COMM295_data/main/data/SAGDP2N__ALL_AREAS_1997_2021.csv")

#Rename the columns
#Added letter "y" as it causes some issues
colnames(states_data) = c("state_name", "description", "unit", "y1997", "y1998", "y1999", "y2000", "y2001", "y2002", "y2003", "y2004", "y2005", "y2006", "y2007", "y2008", "y2009", "y2010", "y2011", "y2012", "y2013", "y2014", "y2015", "y2016", "y2017", "y2018", "y2019", "y2020", "y2021")

#Some data columns were classified as characters due to "(NA)" values which would cause pivotting errors
#Fixing all columns to be double

states_data$y1997 = as.double(states_data$y1997)
states_data$y1998 = as.double(states_data$y1998)
states_data$y1999 = as.double(states_data$y1999)
states_data$y2000 = as.double(states_data$y2000)
states_data$y2001 = as.double(states_data$y2001)
states_data$y2002 = as.double(states_data$y2002)
states_data$y2003 = as.double(states_data$y2003)
states_data$y2004 = as.double(states_data$y2004)
states_data$y2005 = as.double(states_data$y2005)
states_data$y2006 = as.double(states_data$y2006)
states_data$y2007 = as.double(states_data$y2007)
states_data$y2008 = as.double(states_data$y2008)
states_data$y2009 = as.double(states_data$y2009)
states_data$y2010 = as.double(states_data$y2010)
states_data$y2011 = as.double(states_data$y2011)
states_data$y2012 = as.double(states_data$y2012)
states_data$y2013 = as.double(states_data$y2013)
states_data$y2014 = as.double(states_data$y2014)
states_data$y2015 = as.double(states_data$y2015)
states_data$y2016 = as.double(states_data$y2016)
states_data$y2017 = as.double(states_data$y2017)
states_data$y2018 = as.double(states_data$y2018)
states_data$y2019 = as.double(states_data$y2019)
states_data$y2020 = as.double(states_data$y2020)
states_data$y2021 = as.double(states_data$y2021)

#Filter for only "All industry total" and "Manufacturing"

states_reduced = states_data %>%
    filter(description == "All industry total" | description == "Manufacturing")

#Bringing back numbered columns
colnames(states_reduced) = c("state_name", "description", "unit", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

#Pivot Longer/rename
states_pivotted = states_reduced %>%
    pivot_longer(cols = "1997":"2021" ,
                 names_to = "year",
                 values_to = "current_dollars_millions") %>%
    select(-unit)  #unit column is redundant

states_pivotted$year = as.integer(states_pivotted$year) #convert year to an integer

#Calculating the fraction

states_percentage_long = states_pivotted %>%
    group_by(state_name, year) %>%        #group variables by state then year -- creating the pairs of "Manufacturing" and "All" for each year
    summarize(percentage_manufacturing = min(current_dollars_millions)/max(current_dollars_millions))  #since each grouping only contains two numbers, we can simply divide smaller by larger to get percentage

#Create wider version
states_percentage_wide = states_percentage_long %>%
    pivot_wider(names_from = year,
                values_from = percentage_manufacturing)

#Saving file into output_files folder
write_csv(states_percentage_long, "output_files/percentage_manufacturing_long.csv")

write_csv(states_percentage_wide, "output_files/percentage_manufacturing_wide.csv")

#Calculations to confirm procedures were done correctly
states_analysis = states_percentage_long %>%
    group_by(state_name) %>%
    summarize(obs = n())

states_analysis  # There are 51 states in the data set, all of which have 25 calculations for the 25 years 1997-2021. This confirms that no year was left out.
#There is 1275 rows in states_percentage_long which matches perfectly