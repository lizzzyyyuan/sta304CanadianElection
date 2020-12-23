#### Preamble ####
# Purpose: Prepare and clean the survey data, "2017 General Social Survey: Families Cycle 31", downloaded from Statistics Canada
# Author: Junwen Yuan
# Data: 19 DEC 2020
# Contact: junwen.yuan@mail.utoronto.ca
# License: MIT


library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
setwd("/Users/lizyuan/Desktop/final project sta304")


# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("AAOA4Coy.csv")
dict <- read_lines("gss_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("gss_labels.txt")

#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))
 
# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex 
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>% 
  select(agec,
         sex, 
         prv, 
         ehg3_01b, 
         rlr_110,
         totchdc, 
         marstat,
         famincg2,
         brthcan)

#data is on 2017, so add three years in age for everyone
gss <- gss%>% mutate(age=agec+3)
gss<- gss %>% mutate(sex_2cats=ifelse((sex==1),"male","female or others"))
gss<- gss %>%mutate(province=
                      ifelse((prv==10),"Newfoundland and Labrador",
                             
                             ifelse(prv==11,"Prince Edward Island",
                                    ifelse(prv==12,"Nova Scotia",
                                           ifelse(prv==13,"New Brunswick",
                                                  ifelse(prv==24,"Quebec",
                                                         ifelse(prv==35,"Ontario",
                                                                ifelse(prv==46,"Manitoba",
                                                                       ifelse(prv==47,"Saskatchewan",
                                                                              ifelse(prv==48,"Alberta",
                                                                                     "British Columbia"
                                                                                     )) )  )
                                                  ) )))                  ))

gss<-gss %>% mutate(education=ifelse(ehg3_01b==01,"below high school",
                                     ifelse(ehg3_01b==02,"high school",
                                            ifelse(ehg3_01b==03|ehg3_01b==04,"certificate or college",
                                                   ifelse(ehg3_01b==05,"some university",
                                                          ifelse(ehg3_01b==06,"bachelor's degree",
                                                                 ifelse(ehg3_01b==07,"above bachelor's degree",
                                                                        "others")
                                                                 ) )   )
                                            )) )


gss<-gss %>% mutate(religion=ifelse(rlr_110==1,"very important",
                                                      ifelse(rlr_110==2,"somewhat important",
                                                             ifelse(rlr_110==3,"not very important",
                                                                    ifelse(rlr_110==4,"not important at all","others")
                                                             )
                                                      )))


gss<-gss %>% mutate(if_children=ifelse(totchdc==00,"no",
                                        ifelse(totchdc==01|02|03|04|05|06|07,"yes",
                                                                "others"
                                                                )))


gss<-gss %>% mutate(marital_status=ifelse(marstat==01,"married",
                                                            ifelse(marstat==02,"common-law",
                                                                   ifelse(marstat==05,"divorced",
                                                                          ifelse(marstat==04,"seperated",
                                                                                 ifelse(marstat==03,"widowed",
                                                                                        ifelse(marstat==06,"never married","others")
                                                                                 )))
                                                            )))
gss<-gss %>% mutate(marital_status=ifelse(is.na(marital_status),"others",marital_status))
gss<-gss %>% mutate(born_canada=ifelse(brthcan==1,"yes",
                                                         ifelse(brthcan==2,"no","others") ))
gss<-gss %>% mutate(agebin=ifelse(age>=18&age<=22,"18-22",
                                  ifelse(age>=23&age<=27,"23-27",
                                         ifelse(age>=28&age<=32,"28-32",
                                                ifelse(age>=33&age<=37,"33-37",
                                                       ifelse(age>=38&age<=42,"38-42",
                                                              ifelse(age>=43&age<=47,"43-47",
                                                                     ifelse(age>=48&age<=52,"48-52",
                                                                            ifelse(age>=53&age<=57,"53-57",
                                                                                   ifelse(age>=58&age<=62,"58-62",
                                                                                          ifelse(age>=63&age<=67,"63-67",
                                                                                                 ifelse(age>=68&age<=72,"68-72",
                                                                                                        ifelse(age>=73&age<=77,"73-77",
                                                                                                               ifelse(age>=78&age<=82,"78-82",
                                                                                                                      "older than 83")        )            ) )
                                                                                   )      )       )  )     )   )    )     )))


#put them into cells           
gss<-gss %>%select(agebin,
                   sex_2cats,
                   province,
                   education,
                   religion,
                   if_children,
                   marital_status,
                   born_canada) %>% 
  count(agebin,
        sex_2cats,
        province,
        education,
        religion,
        if_children,
        marital_status,
        born_canada) %>% 
  group_by(agebin,
           sex_2cats,
           province,
           education,
           religion,
           if_children,
           marital_status,
           born_canada)





write_csv(gss, "gss.csv")




