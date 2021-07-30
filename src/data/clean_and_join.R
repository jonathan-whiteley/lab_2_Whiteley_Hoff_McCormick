clean_and_join <- function() { 
  require(tidyverse)
  
  #clean up column names
  parks <- parks %>% 
    select(census_fips_code, 
           parks_percent_change_from_baseline) %>%
    rename(fips_code = census_fips_code,
           parks_chg = parks_percent_change_from_baseline) %>% 
    mutate(fips_code = as.character(fips_code))
  
  vax <- vax %>% 
    select(FIPS, Recip_County, Recip_State, 
           Series_Complete_Pop_Pct, Series_Complete_Yes, SVI_CTGY) %>% 
    rename(fips_code = FIPS, 
           county = Recip_County, 
           state = Recip_State, 
           full_vax_pct = Series_Complete_Pop_Pct, 
           full_vax_count = Series_Complete_Yes, 
           social_vulnerability_index = SVI_CTGY)
  
  covid <- covid %>% 
    select(date, county, fips, cases, deaths) %>% 
    rename(fips_code = fips) %>% 
    group_by(fips_code) %>% 
    summarise_at(vars(cases, deaths), sum)
  
  mask_use <- mask_use %>% 
    select(COUNTYFP, ALWAYS) %>% 
    rename(fips_code = COUNTYFP, 
           mask_percent = ALWAYS) %>% 
    mutate(mask_percent = mask_percent*100)
  
  age <- age %>% 
    select(fips_code, median_age_years)
  
  #join
  data <- parks %>%
    dplyr::inner_join(vax, by = c("fips_code")) %>% 
    dplyr::inner_join(covid, by = c("fips_code")) %>% 
    dplyr::inner_join(mask_use, by = c("fips_code")) %>% 
    dplyr::inner_join(age, by = c("fips_code"))
  
}