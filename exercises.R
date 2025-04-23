#############################################################
#### Tidying and manipulating data using the tidyverse ######
##### Dr. Calum Webb, the University of Sheffield ###########
#############################################################

# Introduction and setup --------------------------------------------------

#' In this training course, you'll be learning some advanced
#' data tidying techniques for taking data of any format and
#' making it work well with R. Specifically, we'll be taking
#' untidy data and making it "tidy", which means that each
#' observation is a row and each column is a variable. 
#' 
#' We'll be working through the following skills which, when
#' combined, can be used to bring order to even the most 
#' strangely structured data. Throughout, we'll be using real
#' examples of untidy data and, at the end, we'll look at
#' how these skills can be combined.
#' 
#'  1) Reading data from non-.csv formats (SPSS, STATA, Excel)
#'   1.1) Tidying column names and keeping a codebook
#'   1.2) Selecting specific columns
#'   1.3) Generalised and conditional select() 
#'  2) Creating new variables using mutate:
#'   2.1) Creating transformations of variables
#'   2.2) Recoding categorical variables
#'   2.3) Correcting variable types using parse_*()
#'   2.4) Generalised transformations across many columns
#'        using mutate_at
#'  3) Aggregating data to higher levels 
#'    3.1) Aggregating data using group_by() and summarise()
#'    3.2) Generalised aggregation using summarise_at()
#'    3.3) Creating aggregated (level 2) variables using mutate
#'  4) Pivoting data from long to wide formats
#'    4.1) Pivoting a wide dataset to a long dataset
#'    4.2) Pivoting a long dataset to a wide dataset
#'  5) Working with strings and a little bit of regex
#'    5.1) Removing certain characters from strings
#'    5.2) Extracting certain characters from strings
#'    5.3) Splitting a variable based on a character string
#'  6) Joining relational datasets
#'    6.1) Joining data with the same observations based on 
#'         a single shared key
#'    6.2) Joining data with the same observations based on 
#'         a combination of variables that form a key
#'    6.3) Joining higher-level data to lower level data
#'    6.4) Checking your data with anti_join()
#'  7) Working with dates 
#'  8) Filtering data
#'    8.1) Filtering data using characters
#'    8.2) Filtering data using numbers
#'    8.4) Filtering data using dates
#'  9) Data tidying challenges (in groups or independently)
#'  END and References
#'  
#'  For this training we'll be primarily using packages from
#'  the tidyverse. Prior to the training, you should install
#'  the tidyverse on your computer using the code below:
#'     

install.packages("tidyverse")
install.packages("janitor")
install.packages("readxl")
install.packages("haven")
install.packages("labelled")
install.packages("ggeffects")

#' Loading the tidyverse will also load many of the packages
#' we'll be using today. We're going to make use of the
#' janitor package, which isn't included in the tidyverse.
#' For reading data, we'll be making use of haven and 
#' readxl. Lastly, we're going to use the labelled package
#' to make SPSS datasets more "R friendly".

library(tidyverse)
library(janitor)
library(readxl)
library(haven)
library(labelled)
library(ggeffects)

#' We're going to read in all the data we'll be using for the
#' training course here, with some extra attention paid to
#' reading in the Excel, SPSS, and Stata files.

bigmac <- read_csv("data/bigmac.csv")
tea <- read_csv("data/tea.csv")
age_structure <- read_csv("data/age_structure.csv")
child_spend <- read_csv("data/child_spend.csv")
child_care <- read_csv("data/child_care.csv")
idaci <- read_csv("data/idaci.csv")
health <- read_csv("data/health.csv")
queen <- read_csv("data/queen.csv")

# 1) Reading data from non-.csv formats (SPSS, STATA, Excel) --------------------

#' The first major hurdle many social scientists come across
#' when trying to move over to R for the first time is working
#' with datasets that are in either SPSS, Stata, or Excel
#' format. Some of these are easier to work with than 
#' others. The first challenge is how to read them at all,
#' but that is easily solved using the haven package. 
#' 
#' Let's start by reading in a Stata .dta file:

bsa_stata <- haven::read_dta("data/bsa21.dta")
bsa_stata


#' We'll come back to this shortly, but for now, note how the
#' "HHincomex" variable has values like "9" for Prefer not to Say.
#' and for RClassGP has values like 8 Don't Know, which should
#' be missing for most analyses.
#' 
#' The other thing to note here is that we have this 
#' unusual "dbl+l" number plus label type for many of
#' our variables. SPSS also uses this type of variable. 
#' 
#' This leads to some behaviour that is not particularly
#' desirable, such as showing the numeric value instead of
#' the labels:

table(bsa_stata$HHincomex)

#' And, probably more problematically, making categorical
#' variables be treated as continuous variables in linear
#' regression models. This is a very bad thing. 

model_out <- lm(data = bsa_stata, formula = welfare2 ~ HHincomex)
summary(model_out)

#' For this reason, I actually prefer to use the labelled
#' package to completely remove these "special" variable
#' types and replace them with straightforward factors.
#' This can be achieved using the labelled package.

bsa_unlabelled <- labelled::unlabelled(bsa_stata)
bsa_unlabelled

table(bsa_unlabelled$HHincomex)

model_out <- lm(data = bsa_unlabelled, formula = welfare2 ~ HHincomex)
summary(model_out)


#' In addition, SPSS also allows users to specify multiple 
#' different kinds of codes for missing data, which is 
#' especially useful for studies into non-response (but is 
#' not particularly relevant for a lot of other social 
#' research). Sometimes these missing codes are recorded 
#' as missing, but other times they aren't. 

#' Let's take a look at the same data, but as an SPSS
#' file:

bsa_spss <- haven::read_spss("data/bsa21.sav")
bsa_spss

#' At least with SPSS, the *user defined missing* values 
#' are, by default when using haven, changed into regular
#' missing values. However, not all SPSS files (especially
#' in the UK data service) have user defined missing values
#' set. If, for some reason, you want the missing values 
#' to have their codes attached, you can change the
#' user_na argument to TRUE. Let's assume we have the
#' user_na values here.

bsa_spss <- haven::read_spss("data/bsa21.sav", user_na = TRUE)
bsa_spss

#' It's possible to get an idea for which variables are
#' likely to be missing by looking at the value labels 
#' (before we delete them)
val_labels(bsa_spss$HHincomex)
table(bsa_spss$HHincomex)

#' In the case where the user missing values haven't been
#' picked up properly, or haven't been set in SPSS, you
#' can set these using the set_na_values() function. 
#' For example, let's say we wanted to add -9 to the 
#' missing values for household income:

na_values(bsa_spss$HHincomex) <- c(8, 9, -9)
head(bsa_spss$HHincomex)

#' If we were to then use labelled's user_na_to_na() function,
#' this will turn all of the valus tagged as being missing
#' into regular "missing" NA values in R.

bsa_spss <- user_na_to_na(bsa_spss)
bsa_spss

#' There's no nice convenient way I can find to set the
#' same range of values to missing across all variables,
#' and because you sometimes get variables that are dbl+l
#' and can contain legitimate negative numbers, it's not
#' always safe to set missing values across multiple 
#' columns. However, if you wanted to, for example, 
#' set all labels from -1 to -9 to be missing across
#' multiple variables, you could do the following:

vars_to_change <- c("DVSex21", "Ragecat", "HHincomex")

for (i in 1:length(vars_to_change)) {
  na_values(bsa_spss[vars_to_change[i]]) <- -9:-1
}

head(bsa_spss$DVSex21)
head(bsa_spss$Ragecat)
head(bsa_spss$HHincomex)

#' Once this is done, you can set the "special" SPSS
#' NA values to regular values, as before.

bsa_spss <- user_na_to_na(bsa_spss)
bsa_spss

#' Though, if I'm honest, I would probably just convert
#' the variables to factors and then deal with the values
#' that should be NA through recoding later (covered in 
#' 2.2)

#' Once you've dealt with your missing values, you can 
#' use the same unlabelled process as before:

bsa_spss <- unlabelled(bsa_spss)
bsa_spss

table(bsa_spss$HHincomex)


#' Okay, last is reading Excel files using readxl.
#' Excel files come with a number of challenges: they 
#' are often formatted weirdly, and data may be spread
#' out across multiple different sheets. Let's see
#' what happens if we just try to read wgi.xlsx:

readxl::read_xlsx("data/wgi.xlsx")

#' We just get the cover page. However, we can specify
#' that we want to read a specific sheet. Let's say we
#' want to read the sheet "VoiceandAccountability". We
#' can use the sheet argument:
#' 
#' (P.S. I would always recommend using the sheet's 
#' name, rather than its position, as position will also
#' read hidden sheets.)

readxl::read_xlsx("data/wgi.xlsx", 
                  sheet = "VoiceandAccountability")

#' This is better, but still pretty terrible because we 
#' have a header of about 14 rows. We could skip these
#' using the skip argument:

readxl::read_xlsx("data/wgi.xlsx", 
                  sheet = "VoiceandAccountability", 
                  skip = 14)

#' Alternatively, we could specify a cell range:

readxl::read_xlsx("data/wgi.xlsx", 
                  sheet = "VoiceandAccountability", 
                  range = cell_rows(15:229))

#' The sheet is still quite a mess, there's one more thing
#' that we could use to help fix it and that would be 
#' specifying what NA (missing) looks like in this 
#' spreadsheet. In this case, it looks like the string 
#' "#N/A":

wgi <- readxl::read_xlsx("data/wgi.xlsx", 
                  sheet = "VoiceandAccountability", 
                  range = cell_rows(15:229), 
                  na = "#N/A")

wgi

#' Okay, it's still awful and doesn't meet our definition of
#' tidy data, but it's much better than before.
#' That's all we can do with readxl for now; we're going to 
#' need a whole lot of additional skills before we can tidy
#' this one the rest of the way.



# 1.1) Tidying column names and keeping a codebook ------------------------

#' Everyone likes to use different conventions when they're 
#' naming variables or objects. The tidyverse style guide
#' suggests that everything is named in snake case. Personally,
#' I prefer using snake case names because it cuts down on the
#' guess work and how much you have to remember about names:
#' if there's never any capital letters, you never have to
#' guess whether there might be a capital letter or not.
#' 
#' Let's look at the names in our bsa_spss dataset again:

names(bsa_spss)

#' Okay, they're all over the place. Sometimes with capitals,
#' sometimes without capitals, etc. The janitor package we 
#' loaded earlier has quite a useful function called 
#' clean_names(). It defaults to snake_case, but you can
#' choose any common programming case

bsa_spss <- bsa_spss %>%
  clean_names()

names(bsa_spss)

tea <- tea %>% 
  clean_names()

names(tea)


#' Sometimes, you might need to do some much more intensive
#' cleaning of variable names. For example, let's load the
#' age_structure.csv dataset from the 2011 Census.

age <- read_csv("data/age_structure.csv")
age

codebook <- names(age) # saving the names to a codebook
codebook


#' Those are quite long names, with a lot of special 
#' characters. We don't want to be typing all of those 
#' out every time we're programming. This is where we 
#' might want to rename some variables using the 
#' rename() function.
#' 
#' The new name goes on the left of the equals, and the
#' old name goes on the right:

age %>%
  rename(
    year = date,
    geography = geography,
    code = `geography code`,
    rural = `Rural Urban`,
    population =`Age: All usual residents; measures: Value`,
    between0and4 = `Age: Age 0 to 4; measures: Value`,
    between5and7 = `Age: Age 5 to 7; measures: Value`,
    between8and9 = `Age: Age 8 to 9; measures: Value`,
    between10and14 = `Age: Age 10 to 14; measures: Value`,
    between15 = `Age: Age 15; measures: Value`,
    between16and17 = `Age: Age 16 to 17; measures: Value`,
    between18and19 = `Age: Age 18 to 19; measures: Value`,
    between20and24 = `Age: Age 20 to 24; measures: Value`,
    between25and29 = `Age: Age 25 to 29; measures: Value`,
    between30and44 = `Age: Age 30 to 44; measures: Value`,
    between45and59 = `Age: Age 45 to 59; measures: Value`,
    between60and64 = `Age: Age 60 to 64; measures: Value`,
    between65and74 = `Age: Age 65 to 74; measures: Value`,
    between75and84 = `Age: Age 75 to 84; measures: Value`,
    between85and89 = `Age: Age 85 to 89; measures: Value`,
    between90andmore = `Age: Age 90 and over; measures: Value`,
    mean_age = `Age: Mean Age; measures: Value`,
    median_age = `Age: Median Age; measures: Value`
  )

#' That is quite a lot of code, and definitely something 
#' that is prone to inconsistencies and errors. 

#' One alternative I've found is actually using generative
#' AI to create these types of very long renamed variable 
#' lists, because it tends to 1) be more consistent and 
#' less error prone in naming conventions than me and 
#' 2) pick more understanable variable names than me. 

#' First, we can get a vector of names. We'll be copy 
#' and pasting this into TalkAI.info

names(age)

#' Then try using the following prompt, with the names 
#' vector pasted underneath:

#' Write a simple vector in R using c() that contains 
#' short variable names for all of the following variables.
#' Make sure that these variable names are less than 
#' 10 characters long and are unique. 


#' Here's the response I got:

var_names <- c(
  "date",         # 4 characters
  "geo",          # 3 characters (short for geography)
  "geo_code",     # 8 characters (short for geography code)
  "rural_urb",    # 8 characters (short for Rural Urban)
  "age_all",      # 7 characters (short for Age: All usual residents)
  "age_0_4",      # 6 characters (short for Age: Age 0 to 4)
  "age_5_7",      # 6 characters (short for Age: Age 5 to 7)
  "age_8_9",      # 6 characters (short for Age: Age 8 to 9)
  "age_10_14",    # 8 characters (short for Age: Age 10 to 14)
  "age_15",       # 5 characters (short for Age: Age 15)
  "age_16_17",    # 8 characters (short for Age: Age 16 to 17)
  "age_18_19",    # 8 characters (short for Age: Age 18 to 19)
  "age_20_24",    # 8 characters (short for Age: Age 20 to 24)
  "age_25_29",    # 8 characters (short for Age: Age 25 to 29)
  "age_30_44",    # 8 characters (short for Age: Age 30 to 44)
  "age_45_59",    # 8 characters (short for Age: Age 45 to 59)
  "age_60_64",    # 8 characters (short for Age: Age 60 to 64)
  "age_65_74",    # 8 characters (short for Age: Age 65 to 74)
  "age_75_84",    # 8 characters (short for Age: Age 75 to 84)
  "age_85_89",    # 8 characters (short for Age: Age 85 to 89)
  "age_90_plus",  # 9 characters (short for Age: Age 90 and over)
  "age_mean",     # 8 characters (short for Age: Mean Age)
  "age_median"    # 9 characters (short for Age: Median Age)
)


#' This vector can be used to rename all the names in the dataset,
#' though make sure you check them carefully:

names(age) <- var_names
age

#' You may want to edit your prompt to use a specific case, such
#' as snake case. If you don't like the naming conventions
#' of data providers such as the UK data service, you
#' can use this same trick using the var_label() function
#' to get a list of descriptions. 

var_label(bsa_spss, unlist = TRUE)

#' An example of the output:

short_var_names <- c(
  "serial_s", # serial_scrambled
  "bsa_wt",   # bsa21_final_wt
  "sex21",    # dv_sex21
  "rage",     # ragecat
  "hh_inc",   # h_hincomex
  "r_class",  # r_class_gp
  "hedqual",  # hedqual2x
  "mar_stat", # mar_stat6x
  "hh_kids",  # hhl_chl_gpdx
  "pm_op",    # pm_sx
  "homo_sex", # homo_sexx
  "party_id", # partyfwx
  "pol_int",  # politicsx
  "voted",    # votedx
  "spend",    # spend1x
  "dole_op",  # dolex
  "tax_sp",   # taxspen_dx
  "eqopp_ga", # eqoppga_yx
  "eqopp_tx", # eqopp_tx
  "eqopp_wm", # chopwom_mx
  "eqopp_bl", # eqoppbl_kx
  "bir_cert", # t_bir_certx
  "eq_op_dis",# eq_op_disx
  "eq_op_mh", # eq_op_mhx
  "welf",     # welfare2
  "leftr",    # leftrigh
  "lib_auth", # libauth
  "nhs_sat",  # nhs_satx
  "gp_sat",   # gp_satx
  "nhs_prob", # nhsf_probx
  "nhs_acc"   # nhs_accx
)


# 1.2) Selecting specific columns -----------------------------------------

#' Let's say we then want to select only some specific 
#' columns. This is commonly needed when working with
#' data, and I used to make the mistake of trying to
#' keep absolutely everything, even if there was no
#' chance that I needed it. It, for example, we just
#' want to keep the age groups for children in the age
#' dataset, we can use select()

age %>%
  select(date,
         geo,
         geo_code, 
         rural_urb,
         age_0_4,
         age_5_7,
         age_8_9,
         age_10_14,
         age_15,
         age_16_17)

#' If any of the columns are sequential in the dataset,
#' you can write this code faster by using the : operator:

age %>%
  select(date:age_16_17)

#' You can mix the colon with commas and other ranges, e.g.

age %>%
  select(date:age_16_17, age_65_74:age_90_plus)



# 1.3) Generalised and conditional select()  ------------------------------

#' How about if you want to select something according to
#' some criteria, for example, our Excel spreadsheet that
#' was in bad shape earlier. Imagine we want to keep all of
#' the columns that contain the word "Estimate":

wgi %>%
  select(contains("Estimate"))

#' That's handy. But it does mean we've lost our ID variables
#' which we might want to keep a hold of. Luckily, you can add
#' regular variables separated by commas in the same way as 
#' using select regularly:

wgi <- wgi %>%
  select(country = `Country/Territory`, 
         country_code = Code,
         contains("Estimate"))

wgi

#' For this dataset in particular, we have something of an
#' issue with our variable names. If we look back at the
#' spreadsheet, we can see that each column is called 
#' Estimate but there's a year on top of each column that
#' is merged. The years go from 1996 to 2019, and they
#' increase in two year intervals from 1996 until 2002 and 
#' then by one year intervals from 2002 to 2019. We can 
#' mimic this using seq()

seq(1996, 2002, 2)
seq(2003, 2019, 1)
c(seq(1996, 2002, 2), seq(2003, 2019, 1))

col_years <- c(seq(1996, 2002, 2), seq(2003, 2019, 1))

new_col_names <- paste0("voice_", col_years)
new_col_names

#' Before we can apply these new names, we need to make 
#' sure we are starting from the first column we're 
#' replacing (which is Estimate...3) through to the 
#' last column we want to replace (which is 
#' Estimate...123)

names(wgi)
names(wgi)[3:23]

names(wgi)[3:23] <- new_col_names

wgi



# 2) Creating new variables using mutate: ---------------------------------

#' We're going to do some more tidying shortly, but
#' first let's assume that you've loaded in your data
#' and it's actually in pretty good shape, and then
#' you just want to create some more variables that 
#' are needed for your analysis.



# 2.1) Creating transformations of variables ------------------------------

#' Here I'm referring to transformations of numeric 
#' variables. For example, let's look at the bigmac
#' data. 

bigmac

#' Let's say we want to calculate new variables that 
#' are based on some transformation of existing variables,
#' or are the result of some function. For example:

bigmac %>%
  mutate(
    price_dollars = local_price / dollar_ex
  ) 

#' Will add a new variable to the end of the data
#' that is the result of the local price divided
#' by the exchange rate. You can add additional 
#' variables by separating them with a comma.

bigmac <- bigmac %>%
  mutate(
    price_dollars = local_price / dollar_ex,
    log_gdp = log(GDP_dollar),
    log_price_dollars = log(price_dollars),
    price_dollars_std = scale(price_dollars)
  )

bigmac

#' One thing to keep in mind is that some packages,
#' such as ggeffects, will reverse transform any 
#' transformations that are made *in a model*, but
#' will not reverse any transformations made in prior 
#' to modelling in the mutate function. For example:


#' A model using prior-made transformations:

bm <- lm(data = bigmac, 
         formula = log_price_dollars ~ log_gdp)

summary(bm)

ggeffects::ggeffect(bm, terms = "log_gdp") %>%
  plot()

#' You would need to change the scales using
#' scale_x_continuous and scale_y_continuous
#' to match the function that reverses the 
#' transformation:

ggeffects::ggeffect(bm, terms = "log_gdp") %>%
  plot() +
  scale_x_continuous(transform = scales::exp_trans()) +
  scale_y_continuous(transform = scales::exp_trans()) 

#' If you make the transformations in the model,
#' then ggeffects will automatically reverse the
#' transformation for you.

bm <- lm(data = bigmac, 
      formula = I(log(price_dollars)) ~ I(log(GDP_dollar)))

summary(bm)

ggeffects::ggeffect(bm, terms = "GDP_dollar") %>%
  plot()


# 2.2) Recoding categorical variables -------------------------------------

#' Another common type of new variable that is
#' needed is one that has been recoded, for example
#' in order to create a dummy variable or to 
#' simplify a likert scale. For example, if we 
#' look at the following two variables, let's 
#' imagine we want to create a dummy variable for 
#' people who are aged 65 and over and a to simplify
#' the five point politicsx into a three point scale.

table(bsa_spss$ragecat)
table(bsa_spss$politicsx)

#' We can achieve this using the case_when function,
#' which is a lot more readable than using multiple 
#' nested "ifelse()" functions:

bsa_spss <- bsa_spss %>%
  mutate(
    over65 = case_when(is.na(ragecat) ~ NA_character_, # IMPORTANT
                       ragecat == "65+" ~ "Over 65",
                       TRUE ~ "Under 65"
                       ),
    politics_simple = case_when(is.na(politicsx) ~ NA_character_, # IMPORTANT
                                politicsx == "Not very much" | politicsx == "None at all" ~ "Low",
                                politicsx == "A great deal" | politicsx == "Quite a lot" ~ "High",
                                TRUE ~ "Mid"
                                )
  )

table(bsa_spss$ragecat, bsa_spss$over65)
table(bsa_spss$politicsx, bsa_spss$politics_simple)

#' This function works in the following ways:
#' 
#' Firstly, I always make the first line 
#' is.na(var) ~ NA_character_ (or NA_integer_ if creating a
#' numeric variable).
#' This means, if any response is missing in the original
#' variable then keep it missing. 
#' 
#' The next line sets our recoding. For example, the line:
#' politicsx == "Not very much" | politicsx == "None at all" ~ "Low",
#' Means: when the original variable, politicsx, response is (==)
#' "Not very much", OR (|) the original variable, politicsx,
#' is (==) "None at all", THEN (~), set the new variable's 
#' value to "Low".
#' 
#' The last line is a little bit confusing:
#' TRUE ~ "Mid"
#' In the case of case_when, when you use TRUE this 
#' means that you're saying "If the variable has a value
#' of anything other than the above statements, then make
#' it "".
#' 

#' Encoding the order of variables
#' You might have noticed that our new politics_simple
#' variable is out of order, first is High, then Low, 
#' and then Mid, whereas we'd rather have High -> Mid -> Low
#' or Low -> Mid -> High, otherwise our plots and tables
#' might not look so sensible. We can achieve this by
#' changing the variable to a factor() and setting its 
#' levels.

bsa_spss <- bsa_spss %>%
  mutate(
    politics_simple = factor(politics_simple, 
                             levels = c("Low", "Mid", "High"))
  )

table(bsa_spss$politicsx, bsa_spss$politics_simple)

#' If you have a variable with a lot of categories,
#' and you just want to change the reference level,
#' (i.e. the reference category in a model), it is
#' much easier to use the relevel() function with
#' the ref argument, for example:

table(bsa_spss$over65)

bsa_spss <- bsa_spss %>%
  mutate(
    over65 = relevel(factor(over65), ref = "Under 65")
  )

table(bsa_spss$over65)

#' Now, if you were creating a model, the Under 65 category
#' would be the reference category for the over65 variable,
#' and the Low category would be the reference category for
#' the politics_simple variable:

lm(data = bsa_spss,
   formula = welfare2 ~ over65 + politics_simple) %>%
  summary()



# 2.3) Correcting variable types using parse_*() --------------------------

#' Another very helpful function is parse_*, particularly
#' parse_number. For example, imagine we had the following 
#' character vector:

c("£20", "£21", "£22", "£37")

#' Imagine that, actually, we don't need the £ symbol and
#' in fact all it does is cause us trouble when we are 
#' plotting or modelling. We can use parse_numeric() to
#' extract the first complete number from the values:

parse_number(c("£20", "£21", "£22", "£37"))

#' Let's try this with the tea dataset:

tea 

tea %>%
  mutate(
    tea_kg = parse_number(tea_consumption)
  )

#' That is good, but we can actually do a bit better
#' and we'll discuss that in part 5.


# 2.4) Generalised transformations ----------------------------------------

#' Let's imagine you have multiple variables that
#' you want to apply an identical transformation or
#' recode to. For example, imagine we wanted to 
#' simplify the following five point scales down to
#' three points:

table(bsa_spss$eqoppga_yx)
table(bsa_spss$eqopp_tx)
table(bsa_spss$chopwom_mx)
table(bsa_spss$eqoppbl_kx)

#' We can use the generalisable "mutate_at". The 
#' first thing we need to do is provide a collection
#' of variables that we want to mutate in some way,
#' followed by the transformation/recode (note that
#' your transformation needs to have a ~ in front of
#' it):

bsa_temp <- bsa_spss %>%
  mutate_at(
    vars(eqoppga_yx, eqopp_tx, chopwom_mx, eqoppbl_kx),
    ~case_when(
      is.na(.) ~ NA_character_,
      . == "Gone much too far" | . == "Gone too far" ~ "Too far",
      . == "About right" ~ "About right",
      TRUE ~ "Not far enough"
      ) %>%
      factor(., levels = c("Not far enough", "About right", "Too far"))
  )

table(bsa_temp$eqoppga_yx)
table(bsa_temp$eqopp_tx)
table(bsa_temp$chopwom_mx)
table(bsa_temp$eqoppbl_kx)

#' Notice here that, by default, mutate_at will
#' overwrite the original variables with the 
#' transformations. If you *don't* want it to 
#' do this, you can provide your transformation
#' as a named list. This will then create a 
#' set of new variables with the name in the list
#' appended to the end of the new variables' 
#' names:

bsa_spss <- bsa_spss %>%
  mutate_at(
    vars(eqoppga_yx, eqopp_tx, chopwom_mx, eqoppbl_kx),
    list(
      l3 =  ~case_when(
        is.na(.) ~ NA_character_,
        . == "Gone much too far" | . == "Gone too far" ~ "Too far",
        . == "About right" ~ "About right",
        TRUE ~ "Not far enough"
      ) %>%
        factor(., levels = c("Not far enough", "About right", "Too far"))
    )
  ) 

table(bsa_spss$eqoppga_yx)
table(bsa_spss$eqoppga_yx_l3)
table(bsa_spss$eqopp_tx)
table(bsa_spss$eqopp_tx_l3)
table(bsa_spss$chopwom_mx)
table(bsa_spss$chopwom_mx_l3)
table(bsa_spss$eqoppbl_kx)
table(bsa_spss$eqoppbl_kx_l3)


# 3) Aggregating data to higher levels  -----------------------------------

#' Very often, you will want to aggregate data to a higher
#' level in order to present some summary statistics or,
#' in the case of multilevel modelling, to create level
#' 2 versions of variables (which you could then centre
#' responses to). Let's consider the following:


# 3.1) Aggregating data using group_by() and summarise() ------------------

#' The following dataset is a "long format" dataset with
#' each row representing a single observation of a local
#' authority for a single year. Imagine we wanted to 
#' summarise the dataset according to the local authority;
#' this can be achieved using a combination of group_by()
#' and summarise():

child_care %>%
  group_by(la_name) %>%
  summarise(
    mean_cla_rate = mean(cla_rate, na.rm = TRUE),
    sd_cla_rate = sd(cla_rate, na.rm = TRUE)
  )

#' TASK: Okay, to break things up a bit: modify the
#' above code to summarise the dataset by year:






# 3.2) Generalised aggregation using summarise_at() -----------------------

#' Let's imagine you wanted to summarise the same thing
#' across multiple variables; you can achieve this in a
#' similar way to mutate_at() by using summarise_at():

queen %>%
  group_by(album_name) %>%
  summarise_at(
    vars(danceability:energy, loudness, tempo),
    ~mean(., na.rm = TRUE)
  )

#' Since we already explained a lot of the ways this 
#' works above, I won't go through that again here.


# 3.3) Creating aggregated (level 2) variables using mutate -----------------------

#' Very often, especially for multilevel modelling, we want
#' to create some level 2 (group mean centered) variables
#' (if using within-between models). If you group datasets,
#' every function afterwards will be performed *per group*.
#' In order to stop this behaviour, you need then *ungroup()*
#' the data. This can be very helpful for creating level 2 
#' variables using mutate() or mutate_at()

bigmac <- bigmac %>%
  group_by(name) %>%
  mutate(
    group_mean_gdp_dollar = mean(GDP_dollar, na.rm = TRUE)
  ) %>%
  ungroup()

#' If you were creating a within-between model, you
#' can then center the variables (though the below
#' code is only appropriate for balanced data, i.e.
#' the same number of L1 observations for each L2
#' unit; you'd need a slightly different approach
#' for unbalanced)

bigmac <- bigmac %>% 
  mutate(
    gdp_dollar_group_mean_centered = GDP_dollar - group_mean_gdp_dollar,
    group_mean_gdp_dollar_centered = group_mean_gdp_dollar - mean(group_mean_gdp_dollar, na.rm = TRUE)
  ) 

bigmac %>%
  select(
    GDP_dollar, 
    gdp_dollar_group_mean_centered,
    group_mean_gdp_dollar_centered
  ) %>% tail()

#' If you're curious what that kind of model might look
#' like in lme4, below is an example of that kind of 
#' model (but I've left it commented out)

# lme4::lmer(data = bigmac, 
#            formula = price_dollars ~ I(gdp_dollar_group_mean_centered/1000) + I(group_mean_gdp_dollar_centered/1000) + (1 | name)) %>%
#   summary()


# 4) Pivoting data from long to wide formats ------------------------------

#' Our next sections moves us into pivoting data between
#' long and wide data formats. These terms typically mean
#' between formats where year of observations is encoded 
#' into variable names (wide) or is encoded into its own
#' variable (long). Wide datasets are used for latent 
#' growth models in structural equation modelling, whereas
#' long datasets are used in multilevel models (and are
#' used for plotting time series data). 

#' This is a wide format dataset:

child_spend

#' This is a long format dataset:

child_care


# 4.1) Pivoting a wide dataset to a long dataset --------------------------

#' If you're pivoting a wide dataset to a long dataset,
#' you need to use the pivot_longer() function. This 
#' function takes three arguments: firstly, cols 
#' takes all of the variables that are being pivoted.
#' In this case, we don't want to pivot new_la_code 
#' or the la_name, we just want those values to be 
#' "copied down" for each year:

child_spend_long <- child_spend %>%
  pivot_longer(
    cols = spend_2010:spend_2022,
    names_to = "year",
    values_to = "spend"
  )

child_spend_long

#' Now we'll also need to make sure our year
#' variable actually just has the year, rather
#' than the full variable names. One easy way 
#' to do this in our case is to use the 
#' parse_number() function that we just learned 
#' about:

child_spend_long <- child_spend_long %>%
  mutate(
    year = parse_number(year)
  )

child_spend_long

#' It's very likely that you might have several
#' variables in wide format and you want to 
#' convert them *all* to long. To be honest, the
#' safest and easiest way I've found to do this
#' is to just create subsets of your dataset using
#' select, and then to join them all back together
#' again at the end (Part 6).


# 4.2) Pivoting a long dataset to a wide dataset --------------------------

#' How about achieving the revserse? Going from a long
#' data format to a wide data format? For this, we can
#' just use the inverse function, pivot_wider:

child_care %>%
  pivot_wider(
    names_from = year, 
    values_from = cla_rate, 
    names_prefix = "clarate_"
  )

#' Here I recommend using the optional names_prefix
#' argument, otherwise (particularly when using
#' years), you end up with the following:

child_care %>%
  pivot_wider(
    names_from = year, 
    values_from = cla_rate
  )

#' As with before, if you have multiple sets of variables
#' that you want to pivot wider, I recommend first 
#' selecting a smaller number of columns and then 
#' joining the wider datasets together using the id_cols
#' at the end.


# 5) Working with strings and a little bit of regex -----------------------

#' Before we get onto our final "pillar" of data tidying,
#' let's talk about manipulating strings. Very often,
#' rather than recoding, we want to manipulate the 
#' characters themselves.

# 5.1) Removing certain characters from strings ---------------------------

#' Notice how in the tea dataset, which has been scraped
#' from wikipedia, we have some residual footnotes in
#' the country column that we want to get rid of:

tea

#' We can use mutate and certain functions from the
#' stringr package in order to manipulate strings.
#' For example, let's try removing a number from 
#' the footnotes in the country_region vaiable:

tea %>%
  mutate(
    country_region = str_remove(country_region, "2")
  )

#' That successfully removed the number 2. But what
#' if we want to remove the number 3 and 4 as well?
#' We would do many mutates one after another, but 
#' it would actually be better to use a regular 
#' expression (regex) in our str_remove function.
#' 
#' In regex, a range of sequential numbers or letters 
#' is specified using - within square brackets. 
#' For example, [0-9] means 0, 1, 2, 3, 4, 5, 6, 7,
#' 8, and 9. [A-Z] means A, B, C, D, and so on. 
#' a-z means a, b, c, d, and so on. [A-z] 
#' means all letters, capital or lowercase.
#' 
#' Let's try replacing 2 with [0-9].

tea %>%
  mutate(
    country_region = str_remove(country_region, "[0-9]")
  )

#' Now you might be wondering, but how do I remove 
#' the square brackets themselves!? If you need to 
#' remove special characters, i.e. characters that 
#' perform some function in regex (parentheses, 
#' asterisks, square brackets, ampersands, dollar
#' signs, full stops, and so on), you usually need
#' to escape them using a backslash or a double 
#' backslash (also, yes, that means you need to 
#' backslash a backslash to remove a backslash!)

tea <- tea %>%
  mutate(
    country_region = str_remove(country_region, "\\[[0-9]\\]")
  )

tea

#' There are plenty of regex guides and regex 
#' "creators" out there to help you with expressing
#' particularly tricky things. 



# 5.2) Extracting certain characters from strings ---------------------------

#' Quite often, we might want to extract certain words
#' from a string if they are present. This is often 
#' useful when you want to simplify some very, very
#' long names from data (of course, it's good to 
#' have a record of these long names somewhere so
#' you can reference them and ensure you're referring
#' to them accurately). For example, let's look at the
#' hedqual2x variable:

table(bsa_spss$hedqual2x)

#' Okay, these labels are veeerrryyyy long. We probably
#' would want to simplify this in some way or another.
#' One, of many, options is to try and extract some key
#' words from the variable in a second, simplified,
#' variable. For example:

bsa_spss <- bsa_spss %>%
  mutate(
    qual_simple = str_extract(hedqual2x, "Degree|A levels|GCSE|No qualifications")
  )

table(bsa_spss$qual_simple)

#' Note that this only *happens* to work because the
#' strings "Degree", "A levels", "GCSE", and "No 
#' qualifications *only* appear uniquely in each
#' of the categories (Degree is case-sensitive). It's
#' always worth double-checking your table totals when 
#' using this approach.


# 5.3) Splitting a variable based on a character string -------------------

#' Going back to our tea table, we can see that we 
#' have two different values contained in a single
#' column: tea consumption in kilograms and tea 
#' consumption in pounds. It's quite common to get
#' something like this, for example, a table with 
#' a - or — separating the lower and upper bounds
#' of some number. 

tea

#' We can use the separate() function in order to
#' split a variable at the first point that a 
#' specific string is detected. We know that 
#' every entry in this dataset is separated with
#' a kg in the middle, so we can split the 
#' variable based on where the string kg is 
#' detected:

tea <- tea %>%
  separate(
    tea_consumption,
    into = c("tea_kg", "tea_lb"), 
    sep = "kg"
  )

tea

#' Lastly, we can turn our variables into 
#' numeric types by using the parse_number() function
#' from earlier:

tea <- tea %>%
  mutate(
    tea_kg = parse_number(tea_kg),
    tea_lb = parse_number(tea_lb)
  )

tea



# 6) Joining relational datasets ------------------------------------------

#' One of the last, and one of the most important tools, 
#' in your data tidying arsenal is joins. When data are
#' relational, it means that they share a key by which
#' they can be connected: some data about the observation
#' may be stored in one dataset, and some data may be 
#' stored in another. Sometimes a key is called a "key"
#' or an "id", but othertimes it it just a unique 
#' combination of values that defines that observation
#' in the row (e.g. local authority name, code, and 
#' year)

# 6.1) Joining data with a shared key ---------------------------

#' In many cases, relational datasets will share a 
#' single key with the same name in both files. For
#' example, let's have a look at the IDACI and 
#' health deprivation data files:

idaci
health

#' Both contain a key with the same name, new_la_code.
#' In this case, we can join the two datasets together
#' using one of the many join functions in dplyr
#' (part of the tidyverse). I tend to use left_join,
#' and specify one "main" dataset which I want to
#' build upon with relational data (this is usually
#' the one with the most data to start with)

imd <- left_join(idaci, health, by = "new_la_code")
imd

#' We have now added the imd19_health column from
#' the health dataset onto the data from the idaci
#' dataset, matching everything by key. This is much
#' safer than just sorting our data and then appending
#' columns onto the end!

#' In this join, idaci would be considered out "left"
#' dataset, and health would be considered our "right"
#' dataset. new_la_code is our key.


# 6.2) Joining data with a combination key -----------------------------

#' Sometimes, especially when using administrative data,
#' our 'keys' will not always have the same names and we
#' will often have to use a combination of variables in
#' order to uniquely identify each row, in order to match
#' them up across both datasets.

#' For example, let's have a look at our two datasets
#' we've been tidying about spending on children's services
#' and rates of children in care:

child_spend_long
child_care

#' Here we have three variables that can make up a unique 
#' key for each row: new_la_code, la_name, and year. However,
#' in the child_care dataset the new_la_code variable is 
#' called "code". We could rename this, but we can actually
#' tell the left_join function what the key variables are 
#' called *independently for each dataset*.

child_dat <- left_join(child_spend_long, child_care, 
                       by = c("new_la_code" = "code", "la_name", "year"))

child_dat

#' Notice here that each of our 'key' variables is separated 
#' by a comma, and that they are all within a c() function.
#' In the case where the name is different in the two 
#' different datasets, the name in the "left" hand side
#' dataset is on the left hand side of the equals sign,
#' and the name of the variable in the "right" dataset
#' is on the right hand side of the equals sign.


# 6.3) Joining higher-level data to lower level ---------------------------

#' When you are joining level 2 (for example, person
#' level data) to level 1 data (for example, person-time
#' observations), I would recommend that you always 
#' *prioritise the lower level dataset*. For example,
#' if using left-join, your lower-level dataset should
#' be on the left. 
#' 
#' Otherwise, this works just the same as any other 
#' kind of join.
#' 
#' Let's work through an example using the data we just 
#' joined:

child_dat
imd

child_imd_dat <-left_join(child_dat, 
                          imd, 
                          by = c("new_la_code", "la_name"))

child_imd_dat


# 6.4) Checking your data with anti_join() --------------------------------

#' One last helpful trick is to use anti_join to double
#' check any observations that are *not* present in both
#' datasets. An anti_join() will tell you which observations
#' in the left hand side dataset *cannot be found* in the 
#' right hand side dataset. I usually tend to check what is
#' missing in both directions:

anti_join(child_dat, imd, by = c("new_la_code", "la_name"))
anti_join(imd, child_dat, by = c("new_la_code", "la_name"))

#' In this case, everything matches in both datasets. This
#' might look quite different if we tried to, for example,
#' match the country names in the bigmac dataset to the 
#' country names in the tea dataset:

anti_join(wgi, tea, by = c("country" = "country_region"))
anti_join(tea, wgi, by = c("country_region" = "country"))

#' So, 166 rows in the wgi dataset can't be found in the 
#' tea dataset, and 8 countries in the wgi dataset couldn't
#' be found in the tea dataset.

#' Often, when using names (which aren't very reliable for
#' joining data), you'll find that said data is available
#' but is simply named something different. For example, in
#' the wgi dataset Iran is called "Iran, Islamic Rep." and
#' Russia is called "Russian Federation".
#' 
#' If we wanted to fix these codes, we could use the same
#' case_when() function we used for recoding to replace every
#' instance of "Iran, Islamic Rep." in the wgi data with just
#' "Iran", or vice versa with the tea data.

wgi <- wgi %>%
  mutate(
    country = case_when(is.na(country) ~ NA_character_,
                        country == "Iran, Islamic Rep." ~ "Iran",
                        country == "Russian Federation" ~ "Russia",
                        TRUE ~ country # Important!
                        )
  )

#' The last line on the case_when - TRUE ~ country -
#' is important here because it means "if none of the
#' above conditions apply *keep the value of country for*
#' *that observation the same as they currently are*.

#' Now we can try our anti_join again and see if we 
#' get fewer mismatches:

anti_join(tea, wgi, by = c("country_region" = "country"))

#' This is very time consuming, hence why it's always 
#' better to use a code when available, but it *is* much
#' better to do this kind of renaming *in code* so that 
#' it is reproducible and well documented. It is always 
#' best *not to edit the data directly*, as this is often
#' not recorded or reproducible, and can lead to errors.


# 7) Working with dates  --------------------------------------------------

#' R is often quite good at recognising dates and 
#' treating them appropriately, but occassionally
#' you might need to tell R that something is a date. 
#' For example, let's look at the queen dataset:

queen_s <- queen %>% select(artist_name, album_name, album_release_date, tempo)
queen_s

#' I've made a very small version of the dataset here
#' for convenience, so we can see what is happening.

queen_s %>%
  ggplot() +
  geom_point(aes(x = album_release_date, y = tempo))

#' As we can see, the variable is being read in as a 
#' "character" type, rather than a date type, meaning
#' R doesn't know how to put the variable in date order.
#' This can be corrected using the mutate() function 
#' and the dmy() function, because the date is currently
#' in Day Month Year format rather than Year Month Day 
#' (R's, and all programming languages', preferred format):

queen_s <- queen_s %>%
  mutate(
    album_release_date = dmy(album_release_date)
  )

queen_s

queen_s %>%
  ggplot() +
  geom_point(aes(x = album_release_date, y = tempo))

#' There are equivalent functions for Month Day Year,
#' mdy(), and other formats.

#' You might also want to separately extract the month,
#' day of the week, or year from a date variable. This 

queen_s %>%
  mutate(
   year_of_rel = year(album_release_date),
   weekday_of_rel = weekdays(album_release_date),
   month_of_rel = months(album_release_date)
  ) 


# 8) Filtering data  --------------------------------------------------

#' Our last tool is filtering, and this one is fairly
#' straightforward. We can apply a conditional statement
#' including a variable to the filter() function. This 
#' is normally needed towards the end of data tidying,
#' when you're more focused on analysis:

# 8.1) Filtering data using characters -------------------------------

#' You can filter data using character strings in two
#' different ways; one is where you match exact strings
#' within a vector using the %in% operator:

child_dat %>%
  filter(la_name %in% c("Sheffield"))

#' This can be extended as far as you like, but does
#' rely on the strings matching exactly:

child_dat %>%
  filter(la_name %in% c("Sheffield", "Barnsley"))

#' An alternative to this is to detect strings using
#' str_detect(). You still have to spell *part* of the
#' string correctly, but this can be helpful if you
#' know there might be some variation in how something
#' might be spelled (e.g. is Bristol just Bristol, or
#' is it City of Bristol, or is it Bristol, City of,
#' or is it Bristol, City Of?)

child_dat %>%
  filter(str_detect(la_name, "Bristol"))

#' If you want to extend this way of filtering data,
#' the best thing to do is separate each unique search
#' query in the pattern string with a |, meaning OR:

child_dat %>%
  filter(str_detect(la_name, "Bristol|Herefordshire"))


# 8.2) Filtering data using numbers -------------------------------

#' Filtering data using numbers works in the same way,
#' but you have additional options including the ability
#' to use greater than or less than (and greater than or
#' equal to and less than or equal to) logic, for example,
#' to filter all observations with more than £1000 per
#' child spending:

child_dat %>%
  filter(spend > 1000)


# 8.3) Filtering data using dates -------------------------------

#' Dates can be a little more unusual, but the only
#' thing you need to remember is to write them in 
#' the same YYYY-MM-DD format that R tends to use,
#' and to do so *after* transforming them into date
#' type variables (part 7), and wrapping them in the
#' appropriate ymd() function. For example, if we wanted
#' to filter the data based on all albums released 
#' before 1980, we can use:

queen_s %>%
  filter(
    album_release_date < ymd("1980-01-01")
  ) 

#' The same goes for choosing a range of dates, for example, 
#' between 1980 and 1990, we just need an & (AND) operator:

queen_s %>%
  filter(
    album_release_date > ymd("1980-01-01") &
      album_release_date < ymd("1990-01-01")
  ) 



# 9) Data tidying challenges ----------------------------------------------

#' I've included some untidy datasets that I've come across over
#' several years that you can use to practice your data tidying
#' skills - have a go at them in groups or independently to see
#' if you can apply the tools we've used today. There are some
#' suggested solutions in the tidying-challenge-solutions.R file.

######################################################################

# Tidying Challenge: data/sotkanet-data.xls

#' This data is from the Sotkanet.fi (https://sotkanet.fi/sotkanet/en/index)
#' statistical data service, which contains a wide range of interesting
#' municipal data from Finland. The major challenge of tidying this data 
#' comes from the fact that the years are given in the columns, and the 
#' rows represent variable, year, municipality observations. We want to
#' switch this over so that each row represents an observation in a given
#' year, for a specific municipality, and each column represents a variable.
#' 
#' Difficulty: Easy
#' - This is quite a typical and useful example of official statistical 
#' releases across many countries


######################################################################

# Tidying Challenge: data/mortality-nomis.xlsx

#' Successfully read and tidy the above spreadsheet so that all of the
#' numeric variables are of the proper type, that missing values are 
#' detected as missing. Turn the data into a long format, where each
#' row represents a region for a specific year for a specific gender 
#' (all,  male, female) and each column represents the number or
#' rate of the mortality statistic.

# Difficulty: Medium (but tedious!)
# - It would be easier to try and read in each section and then join
#   them all together at the end!



######################################################################


# Tidying Challenge: data/healthcare-beds-data.xlsx

#' Successfully read and tidy the above spreadsheet so that all of the
#' numeric variables are of the proper type, that missing values are 
#' detected as missing, and that country names are consistent, without
#' any footnotes or extra spaces.

# Difficulty: Hard
# - Probably need to use some regex/functions not covered: (.+), str_trim()
# - Uses merged cells that throw an error with readxl


# END and References -----------------------------------------------------

#' This has been an attempt to try and condense all
#' of the varied skills that are built up over
#' a very long time writing code to tidy datasets
#' in an efficient and accessible way. There's no way
#' you'll be able to retain all of this information,
#' all of these different tools and techniques are the
#' "most common verbs" of working with data in R, and 
#' the only way to become fluent in using them is to
#' work with them often. 
#' 
#' However, I hope that this material will be a useful
#' 'phrasebook' for you getting started. That is, by 
#' providing a lot of real use cases with real social
#' science data, I hope that when you come across something
#' similar in your own work your memory might be jogged 
#' about something that was illustrated in this script, and
#' that you can revisit it and adapt it to your purposes.
#' 
#' Don't worry if some things don't make sense, especially
#' the more high level things such as generalisation of
#' code across multiple variables. Do what makes sense
#' to you; if that means writing the same code with a 
#' lot of copy and pasting, then that's fine. The important
#' thing is that you reach the end result, that the tidying
#' you do is documented, and that you avoid making errors as
#' far as is possible.
#' 
#' The best textbook to learn data tidying, and to reinforce
#' what we've talked about in this course, is R for Data
#' Science by Wickham, Cetinkaya-Rundel, and Grolemund,
#' now in its second edition. It can be read for free online
#' here: https://r4ds.hadley.nz Consider making a donation to
#' Kākāpō Recovery if you find it useful.
#' 
#' If you want a shorter, more conceptual paper about data
#' tidying, see Wickham, H. (2014). *Tidy Data*. Journal of 
#' Statistical Software. 
#' https://www.jstatsoft.org/article/view/v059i10



