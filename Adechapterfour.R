## Chapter four
## tidy data
library(tidyverse)
library(readxl)
library(dataskills)

ocean <- read_csv("https://psyteachr.github.io/msc-data-skills/data/personality.csv")

ocean
library("devtools")

devtools::install_github("psyteachr/msc-data-skills")



## how to gather data
## gather the ocean data
ocean_gather<- gather(ocean, "question", "score", Op1:Ex9)
ocean_gather

##separate
## to separate the question into domain and question number
ocean_separate <- separate(ocean_gather, question, c("domain","qnumber"), sep = 2)
ocean_separate

##unite
## unite the domain and qnumber
ocean_unite <- unite(ocean_separate, "domain_n", domain, qnumber,sep = "_Q")
ocean_unite

##spread
ocean_spread <- spread(ocean_unite, "domain_n", score)
ocean_spread


##pipes
## create a table of 10 participants, with 2 columns for var of mean_A and var mean_B.
data_original <- tibble(
  id= 1:10,
  A1 = rnorm(10, 0),
  A2 = rnorm(10, 1),
  B1 = rnorm(10, 2),
  B2 = rnorm(10, 3)
)

data_original 

 ## gather columns A1 to B2 into "variables" and "values" columns
data_gathered <- gather(data_original, "variables", "values", A1:B2)

data_gathered

##separate the var column at the _ into "var" and "var_n columns
data_separated <- separate(data_gathered, variables, c("var", "var_n"), sep = 1)

data_separated

## group data by id and var
data_grouped <- group_by(data_separated, id, var)
  
data_grouped

## calculate the mean values for each value and var
data_summarised <- summarise(data_grouped, mean = mean(values))
data_summarised

##spread the mean column into A and B columns
data_spread <- spread(data_summarised, var, mean)
data_spread

## rename A and B into A_mean and B_mean
data <- rename(data_spread, A_mean = A, B_mean = B)
data

#pipes
# calculate mean of A and B variable for each participant
data <- tibble(
  id = 1:10,
  A1 = rnorm(10, 0),
  A2 = rnorm(10, 1),
  B1 = rnorm(10, 2),
  B2 = rnorm(10, 3)
  )%>%
  gather(variables, values, A1:B2) %>%
  separate(variables, c("var", "var_n"), sep = 1) %>%
  group_by(id, var)%>%
  summarise(mean = mean(values)) %>%
  spread(var, mean) %>%
  rename(A_mean = A, B_mean = B)

data("personality", package = "dataskills")
personality

##convert the personality wide table format to long format
personality_long <- pivot_longer(
  data = personality,
  cols = Op1:Ex9,
  names_to = c("domain", "qnumber"),
  names_sep =2,
  values_to = "scores"
  )%>%
  glimpse()

##convert personality_long to wider format

personality_wide <- pivot_wider(
  data = personality_long,
  names_from = c(domain, qnumber),
  values_from = scores,
  names_sep = ""
 )%>%
  glimpse()

##using a complex example
## renaming and retrieving the data file
data("infmort", package = "dataskills")
infmort

data("matmort", package = "dataskills")
matmort

#convert matmort from wide to long
matmort_long <- matmort %>%
  pivot_longer(cols = '1990':'2015',
               names_to ="years",
               values_to = "stats") %>%
  glimpse()

##alternatively using gather
matmort_long_gathered <- matmort %>%
  gather ("years", "stats", '1990':'2015') %>%
  glimpse()

##remove the spaces by mutate and separate stats
matmort_mutate <- matmort_long %>%
  mutate(stats = gsub(" ", "",stats)) %>%
  separate(stats, c("rates", "ci_low", "ci_high"))%>%
  glimpse()

##use extra function to drop the additional pieces discard error message
matmort_mutate_drop <- matmort_long %>%
  mutate(stats = gsub(" ", "",stats)) %>%
  separate(stats, c("rates", "ci_low", "ci_high"), extra = "drop") %>%
  glimpse()

##set delimiters using separate
infmort_split <- infmort %>%
  separate(3, c("rates", "ci_low", "ci_high"), extra = "drop") %>%
  glimpse()

##to split appropriately
infmort_split_appro <- infmort %>%
  separate(
    col = 3,
    into = c("rates", "ci_low", "ci_high"), 
    extra = "drop",
    sep = "(\\[|-|])" 
    ) %>%
  glimpse()

##add convert 
infmort_split_appro <- infmort %>%
  separate(
    col = 3,
    into = c("rates", "ci_low", "ci_high"), 
    extra = "drop",
    sep = "(\\[|-|])", 
    convert = TRUE) %>%
  glimpse()

##perform same task with the matmort_long
matmort_split <- matmort_long %>%
  mutate(stats =gsub(" ", "", stats)) %>%
  separate(
    col = stats,
    into = c("rates", "ci_low", "ci_high"), 
    extra = "drop",
    convert = TRUE) %>%
  glimpse()

##add in one step
matmort2 <- dataskills::matmort %>%
  gather("year", "stats", '1990':'2015')%>%
  mutate(stats = gsub(" ", "", stats))%>%
  separate(
    col = stats,
    into = c("rates", "ci_low", "ci_high"), 
    extra = "drop",
    convert = TRUE) %>%
  glimpse()

## spread rate by year for matmort data
matmort_wide <- matmort2 %>%
  spread(key = year, value = rates) %>%
  print()

##pivot_wider to get a better spread
matmort_wide <- matmort2 %>%
  pivot_wider(
    names_from = year,
    values_from = c(rates, ci_low, ci_high)
  )
glimpse(matmort_wide)

##working with experimentum data
dataskills::experimentum_quests

##get data into wide format by using pivot_wide function
q <- dataskills::experimentum_quests %>%
  pivot_wider(id_cols = session_id:user_age,
              names_from = q_name,
              values_from = dv) %>%
  type.convert(as.is = TRUE) %>%
  print()

##EXERCISE
dataskills::exercise(4)
