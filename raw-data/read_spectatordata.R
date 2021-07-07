# Reading spectator data from qualtrics file and saving as R data.
require(tidyverse)
require(janitor)

headers <- read_csv(here::here("raw-data", "2017+mmtalent+spectators_December+4%2C+2017_14.06.csv"),
                    n_max=1,
                    col_names=FALSE) %>%
  unlist(., use.names=FALSE)
raw_data <- read_csv(here::here("raw-data", "2017+mmtalent+spectators_December+4%2C+2017_14.06.csv"),
                 skip=3,
                 col_names = headers) %>% clean_names()

# Keep data on consenting and complete observations
all_data <- raw_data %>% filter(progress == 100) %>%
  filter(consent==1) %>%
  filter(!(is.na(eap) && is.na(eai) && is.na(epp) && is.na(epi))) %>%
  filter(!is.na(luck_fair), !is.na(talent_fair), !is.na(effort_fair)) %>%
  filter(!is.na(luck_control),!is.na(talent_control), !is.na(effort_control)) %>%
  filter(!is.na(polpref), !is.na(redist_pref)) %>%
  dplyr::select(c(start_date, duration_in_seconds, gender, age, residence, edu, income, epp, eap, epi, eai,
         luck_fair, talent_fair, effort_fair, luck_control, talent_control, effort_control,
         redist_pref, polpref, ex_ante_personal, ex_ante_impersonal, ex_post_personal, ex_post_impersonal,
         payment_high_worker, payment_low_worker, redistribute))

# For coding residence:
states <- c("Alabama",
            "Alaska" ,
            "Arizona" ,
            "Arkansas" ,
            "California",
            "Colorado" ,
            "Connecticut",
            "Delaware" ,
            "DoC" ,
            "Florida" ,
            "Georgia" ,
            "Hawaii" ,
            "Idaho" ,
            "Illinois" ,
            "Indiana" ,
            "Iowa" ,
            "Kansas" ,
            "Kentucky" ,
            "Louisiana" ,
            "Maine" ,
            "Maryland" ,
            "Massachusetts" ,
            "Michigan" ,
            "Minnesota" ,
            "Mississippi" ,
            "Missouri" ,
            "Montana" ,
            "Nebraska" ,
            "Nevada" ,
            "New Hampshire" ,
            "New Jersey" ,
            "New Mexico" ,
            "New York" ,
            "North Carolina" ,
            "North Dakota" ,
            "Ohio" ,
            "Oklahoma" ,
            "Oregon" ,
            "Pennsylvania" ,
            "Puerto Rico" ,
            "Rhode Island" ,
            "South Carolina" ,
            "South Dakota" ,
            "Tennessee" ,
            "Texas" ,
            "Utah" ,
            "Vermont" ,
            "Virginia" ,
            "Washington" ,
            "West Virginia" ,
            "Wisconsin" ,
            "Wyoming" ,
            "I do not live in the United States")

edulevels <- tribble(~edu, ~edu_category,
                     1, "No High School",
                     2, "High School/GED",
                     3, "Some college",
                     4, "Associate's Degree",
                     5, "Bachelor",
                     6, "Masters",
                     7, "Professional (JD/MD)",
                     8, "PhD")

incomelevels <- tribble(~income, ~income_category,
                        1, "Less than 29 999",
                        2, "30k- 59 999",
                        3, "60k - 99 999",
                        4, "100k - 149 999",
                        5, "150k +")


mmtalent_df <- all_data %>%
  mutate(age_category = cut(age, breaks=c(18, 35, 45, 55, 65, 120), right=FALSE),
         age_category = as.character(age_category),
         gender = c("male", "female")[gender+1],
         state_residence = states[residence],
         region = 1*(residence %in% c(7,20,22,30,41,47,31,33,39)) +
           2*(residence %in% c(15,14,23,36,51,16,17,24,26,28,35,43)) +
           3*(residence %in% c(3,6,13,27,32,46,29,52,2,5,12,38,49)),
         region = ifelse(residence!=53, c("South", "Northeast","Midwest","West")[region+1], NA),
         region = ifelse(residence==53, "South", region), # Assigning largest region to one missing value
         treatment = factor( case_when(ex_ante_personal==1 ~ "ExAntePersonal",
                               ex_ante_impersonal==1 ~ "ExAnteImpersonal",
                               ex_post_personal==1 ~ "ExPostPersonal",
                               ex_post_impersonal==1 ~ "ExPostImpersonal")),
         timing = factor( c("ExAnte", "ExPost")[ 1 + treatment %in% c("ExPostPersonal", "ExPostImpersonal")]),
         personal = factor( c("Impersonal", "Personal")[1 + treatment %in% c("ExAntePersonal", "ExPostPersonal")])) %>%
  left_join(incomelevels, by="income") %>%
  left_join(edulevels, by="edu") %>%
  select(treatment, timing, personal,
         start_date, duration_in_seconds, gender, age, age_category, state_residence, region,
         edu_category, income_category,
         luck_fair, talent_fair, effort_fair, luck_control, talent_control, effort_control,
         redist_pref, polpref, redistribute, payment_low_worker, payment_high_worker)

saveRDS(mmtalent_df, file=here::here("data", "mmtalent_df.rds"))

