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
           Series_Complete_Pop_Pct, Series_Complete_Yes, Completeness_pct) %>% 
    rename(fips_code = FIPS, 
           county = Recip_County, 
           state = Recip_State, 
           full_vax_pct = Series_Complete_Pop_Pct, 
           full_vax_count = Series_Complete_Yes,
           percent_vax_w_fips = Completeness_pct)
  
  covid <- covid %>% 
    select(date, county, fips, cases, deaths) %>% 
    rename(fips_code = fips) %>% 
    group_by(fips_code) %>% 
    summarise_at(vars(cases, deaths), sum) %>% 
    mutate(cases = case_when(
      cases == 0 ~ 0e-16,
      cases != 0 ~ cases)
    )
  
  mask_use <- mask_use %>% 
    select(COUNTYFP, ALWAYS) %>% 
    rename(fips_code = COUNTYFP, 
           mask_percent = ALWAYS) %>% 
    mutate(mask_percent = mask_percent*100)
  
  age <- age %>% 
    select(fips_code, median_age_years, population) %>%
    rename(census_population = population)
  
  #join
  data <- parks %>%
    dplyr::inner_join(vax, by = c("fips_code")) %>% 
    dplyr::inner_join(covid, by = c("fips_code")) %>% 
    dplyr::inner_join(mask_use, by = c("fips_code")) %>% 
    dplyr::inner_join(age, by = c("fips_code"))
  
  data <- data %>%
    mutate(
      implied_population = round(full_vax_count / (full_vax_pct/100), 0),
      parks_chg_nominal = (parks_chg / 100) * implied_population,
      region = case_when(
        state %in% c('AK', 'WA', 'OR', 'CA', 'NV', 'HI')
        ~ 'West',
        state %in% c('ID', 'UT', 'MT', 'WY', 'CO')
        ~ 'Mountain West',
        state %in% c('AZ', 'NM', 'TX')
        ~ 'South West',
        state %in% c('ND', 'SD', 'NE', 'KS', 'OK', 'IA')
        ~ 'Great Plains',
        state %in% c('MN', 'MO', 'WI', 'MI', 'IL', 'OH', 'IN', 'OH', 'PA', 'WV')
        ~ 'MidWest',
        state %in% c('KY', 'AR', 'LA', 'MS', 'AL', 'GA', 'TN', 'KY', 'VA', 'NC', 'SC')
        ~ 'South',
        state %in% c('FL')
        ~ 'Florida',
        state %in% c('DC', 'MD', 'DE', 'NJ', 'CT', 'NY', 'RI', 'VT', 'NH', 'MA', 'ME')
        ~ 'North East',
      ),
      region_ohe = case_when(
        state %in% c('AK', 'WA', 'OR', 'CA', 'NV', 'HI')
        ~ 1,
        state %in% c('ID', 'UT', 'MT', 'WY', 'CO')
        ~ 2,
        state %in% c('AZ', 'NM', 'TX')
        ~ 3,
        state %in% c('ND', 'SD', 'NE', 'KS', 'OK', 'IA')
        ~ 4,
        state %in% c('MN', 'MO', 'WI', 'MI', 'IL', 'OH', 'IN', 'OH', 'PA', 'WV')
        ~ 5,
        state %in% c('KY', 'AR', 'LA', 'MS', 'AL', 'GA', 'TN', 'KY', 'VA', 'NC', 'SC')
        ~ 6,
        state %in% c('FL')
        ~ 7,
        state %in% c('DC', 'MD', 'DE', 'NJ', 'CT', 'NY', 'RI', 'VT', 'NH', 'MA', 'ME')
        ~ 8,
      )
    )
  
}