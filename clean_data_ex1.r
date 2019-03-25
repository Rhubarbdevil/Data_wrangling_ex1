library("dplyr")
library("tidyr")


clean_me <- as.data.frame(read.csv(file = "/Users/Rhubarbking1/Documents/practice/refine_original.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE))

#correct misspelled brand names in company column
clean_me <- mutate(clean_me, company = tolower(company))

clean_me <- mutate(clean_me, company_name1 = ifelse(grepl("l.+p.+", company), "philips", ""))
clean_me <- mutate(clean_me, company_name2 = ifelse(grepl("a.+z.+", company), "akzo", ""))
clean_me <- mutate(clean_me, company_name3 = ifelse(grepl("van.+h.+", company), "van houten", ""))
clean_me <- mutate(clean_me, company_name4 = ifelse(grepl("uni.+", company), "unilever", ""))

clean_me <- mutate(clean_me, company_name = case_when(company_name1 == "philips" ~ "philips",
                                                      company_name2 == "akzo" ~ "akzo",
                                                      company_name3 == "van houten" ~ "van houten",
                                                      company_name4 == "unilever" ~ "unilever"))

clean_me <- mutate(clean_me, company = company_name)

clean_me$company_name1 <- NULL
clean_me$company_name2 <- NULL
clean_me$company_name3 <- NULL
clean_me$company_name4 <- NULL
clean_me$company_name <- NULL

#separate product and code number
clean_me <- separate(clean_me, col = Product.code...number, sep = "-", into = c("product_code", "product_number"))

#add a column with the product categories: p = Smartphone,
#v = TV, x = Laptop, q = Tablet

clean_me <- mutate(clean_me, product_category = case_when(product_code == "p" ~ "Smartphone",
                                    product_code == "v" ~ "TV",
                                    product_code == "x" ~ "Laptop",
                                    product_code == "q" ~ "Tablet", TRUE ~ as.character(product_code)))
         
#create new column full_address that concatenates other address columns, sep ","
clean_me <- mutate(clean_me, full_address = paste(address, city, country, sep = ","))

#create dummy variables for company and product category with prefixes company_ and product_ (e.g., company_akzo)
# add four binary columns for company name
clean_me <- mutate(clean_me, company_akzo = ifelse(company == "akzo", 1, 0))
clean_me <- mutate(clean_me, company_philips = ifelse(company == "philips", 1, 0))
clean_me <- mutate(clean_me, company_vanhouten = ifelse(company == "van houten", 1, 0))
clean_me <- mutate(clean_me, company_unilever = ifelse(company == "unilever", 1, 0))

#add four binary columns for product category
clean_me <- mutate(clean_me, product_smartphone = ifelse(product_category == "Smartphone", 1, 0))
clean_me <- mutate(clean_me, product_tv = ifelse(product_category == "TV", 1, 0))
clean_me <- mutate(clean_me, product_laptop = ifelse(product_category == "Laptop", 1, 0))
clean_me <- mutate(clean_me, product_tablet = ifelse(product_category == "Tablet", 1, 0))

write.csv(clean_me, "/Users/Rhubarbking1/Documents/practice/refine_clean.csv")