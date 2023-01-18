library(tidyverse) 

#Reading all data needed
state_area = read_csv("data/state_area.csv") #https://www.census.gov/geographies/reference-files/2010/geo/state-area.html

head(state_area)

state_pop = read_csv("data/dat_population.csv") 
colnames(state_pop) = c("state_name", "year", "population")

head(state_pop)

#Found a US census page with prexisting population density 

census_2020 = read_csv("data/2020_census_density.csv") #https://www.census.gov/data/tables/time-series/dec/density-data-text.html 

head(census_2020)

#Open a past file for list of 50 states

oilgas = read_csv("https://raw.githubusercontent.com/paulh777/COMM295_data/main/output_files/percentage_oilgas_wide.csv") 

state_list = oilgas$state_name

glimpse(state_list)

#Filter out all non_states and miles columns
state_area = state_area %>%
    filter(state_name %in% state_list) %>%
    select(state_name, land_area_sqkm)

head(state_area)

nrow(state_area) #check how many rows

#Joining population and area
pop_density = left_join(state_pop, state_area, by = "state_name")

head(pop_density, 10)

#Calculating population density
pop_density = pop_density %>%
    mutate(population = population *10^6,          #multiplying by 1 million
           pop_density = population/land_area_sqkm)
          
#Creating wide dataframe
pop_density_wide = pop_density %>%
    select(-population, -land_area_sqkm) %>%
    pivot_wider(names_from = year,
                values_from = pop_density)

head(pop_density)
head(pop_density_wide)

#Comparison analysis with US gov 2020 census population density
census_2020_km = census_2020 %>%
    mutate("2020_census" = pop_density_misq/2.58999) %>%
    select(state_name, "2020_census")

head(census_2020_km)

#Joining the 2020_census to both orignal output dataframes
pop_density_wide_census = left_join(pop_density_wide, census_2020_km, by = "state_name")

pop_density_long_census = pop_density_wide_census %>%
    pivot_longer(cols = "1997":"2020_census" ,
                 names_to = "year",
                 values_to = "population_density_p/kmsq") 

head(pop_density_wide_census)
tail(pop_density_long_census)

#Joining 2020 census to the wide data calculations
census_comparison = left_join(pop_density_wide, census_2020_km, by = "state_name") %>%
    select(state_name, "2019", "2020_census") 

colnames(census_comparison) = c( "state_name", "calculation_2019", "census_2020" )

census_comparison = census_comparison %>%
    mutate(abs_deviation = abs(calculation_2019 - census_2020),
           percentage_deviation = abs_deviation / census_2020)

comparison_summary = census_comparison %>%
    summarize(MAD = mean(abs_deviation),
              MAPE = mean(percentage_deviation))
           
head(census_comparison)
comparison_summary  #Comparisons confirm that choosing only land, and general calculations were correct. 2019 calculated data and 2020 retrieved data show very close ressemblance

#Writing output
write_csv(pop_density, "output_files/state_population_density_long.csv")

write_csv(pop_density_wide, "output_files/state_population_density_wide.csv")

#Addition of US gov census
write_csv(pop_density_long_census, "output_files/state_population_density_long_census.csv")

write_csv(pop_density_wide_census, "output_files/state_population_density_wide_census.csv")

#Visualizing density
pop_density_summary = pop_density %>%
    group_by(state_name) %>%
    summarize(mean_density = mean(pop_density),
              std_density = sd(pop_density))

head(pop_density_summary)

#Plotting avg Density
pop_density_plot = pop_density_summary %>%
    ggplot(aes(x = mean_density, y = state_name)) +
    geom_bar(stat="identity") +
    labs(y = "State Name", x = "Population Density (Person per km squared)", title =  "State Population Density") 

pop_density_plot
