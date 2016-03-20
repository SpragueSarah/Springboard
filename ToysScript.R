# Springboard Data Science Class: Data Wrangling Exercise 1. Toys

#Loading refine_original.csv into data frame (file given with problem), tidyr/dplyr/car packages
refine_original = read.csv("refine_original.csv")
library(dplyr)
library(car)
library(tidyr)

#printing the new data frame for posterity (now commented out)
#refine_original

#Transforming company names to: philips, akzo, van houten and unilever by car.recode
refine_original$company <- recode(refine_original$company, "c('Phillips', 'phillips', 
        'phllips', 'phillps', 'fillips', 'phillipS', 'phlips') = 'philips'")
refine_original$company <- recode(refine_original$company, "c('Akzo', 'AKZO', 'akz0', 
        'ak zo') = 'akzo'")
refine_original$company <- recode(refine_original$company, "c('Van Houten', 'van Houten') 
        = 'van houten'")
refine_original$company <- recode(refine_original$company, "c('unilver', 'Unilever') = 
        'unilever'")

#Separating out product code and product number into two new columns using tidyr::separate
refine_separated <- separate(refine_original, Product.code...number, c("product_code", "product_number"), sep = "-")

# Adding product category column based on product_code by first adding the column
# with the default name of Smartphone, then testing with a for loop to change based
# on the contents of the product_code column
refine_mutated <- mutate(refine_separated, product_category = "Smartphone")
  
for (i in 1:25) {
  if (refine_mutated$product_code[i] == "x"){
    refine_mutated$product_category[i] = "Laptop"
    } else if (refine_mutated$product_code[i] == "v") {
      refine_mutated$product_category[i] = "TV"
    } else if (refine_mutated$product_code[i] == "q") {
      refine_mutated$product_category[i] = "Tablet"
    }
}

# Uniting address/city/country columns for geocoding
refined_united <- unite(refine_mutated, full_address, address:country, sep = ", ")

# Creating dummy variables for company in binary for further analysis
refined_binary <- mutate(refined_united, company_philips = 0)
refined_binary <- mutate(refined_binary, company_akzo = 0)
refined_binary <- mutate(refined_binary, company_van_houten = 0)
refined_binary <- mutate(refined_binary, company_unilever = 0)

for (i in 1:25) {
  if (refined_binary$company[i] == "philips"){
    refined_binary$company_philips[i] = 1
  } else if (refined_binary$company[i] == "akzo") {
    refined_binary$company_akzo[i] = 1
  } else if (refined_binary$company[i] == "van houten") {
    refined_binary$company_van_houten[i] = 1
  } else if (refined_binary$company[i] == "unilever") {
    refined_binary$company_unilever[i] = 1
  }
}

# Creating dummy variables for product category in binary for further analysis
refined_binary <- mutate(refined_binary, product_smartphone = 0)
refined_binary <- mutate(refined_binary, product_tv = 0)
refined_binary <- mutate(refined_binary, product_laptop = 0)
refined_binary <- mutate(refined_binary, product_tablet = 0)

for (i in 1:25) {
  if (refined_binary$product_category[i] == "Smartphone"){
    refined_binary$product_smartphone[i] = 1
  } else if (refined_binary$product_category[i] == "TV") {
    refined_binary$product_tv[i] = 1
  } else if (refined_binary$product_category[i] == "Laptop") {
    refined_binary$product_laptop[i] = 1
  } else if (refined_binary$product_category[i] == "Tablet") {
    refined_binary$product_tablet[i] = 1
  }
}
refined_binary

