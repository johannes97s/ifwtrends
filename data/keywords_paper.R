# library(tidyverse)
# library(gtrendsR)
#
#
# crisis <- c( "Economic crisis", "Crisis", "Recession", "Financial crisis", 'Krach')
# unemp <- c( "Unemployment", "Unemployment benefits", 'Welfare & Unemployment')
# credit <- c('Student loan', 'Credit & Lending', 'Loan', 'Interest', 'Mortgage', 'Auto Financing')
# consum <- c('Food & Drink', 'GPS & Navigation', 'Performing Arts','Luggage topic', 'Vehicle Brands', 'Birthday',
#             'Travel', 'Energy & Utilities', 'Vehicle Shopping', 'Tobacco Products', 'Health',
#             'Pharmacy', 'Carpooling & Ridesharing', 'Sports', 'Animal Products & Services', 'Fitness', 'Weddings',
#             'Car Rental & Taxi Services', 'Autos & Vehicles', 'Tourist Destinations', 'Home & Garden', 'Events & Listings',
#             'Grocery & Food Retailers', 'Vehicle Licensing & Registration', 'Timeshares & Vacation Properties',
#             'Home Appliances', 'Mass Merchants & Department Stores', 'Car Electronics', 'Fashion & Style', 'Trucks & SUVs',
#             'Home Furnishings', 'Footwear', 'Cruises & Charters', 'Hotels & Accommodations', 'Luggage & Travel Accessories',
#             'Fast Food', 'Book Retailers', 'Veterinarians', 'Spas & Beauty Services', 'Acting & Theater', 'Travel Agencies & Services')
# jobs <- c( 'Waiter', 'Job Listings', 'Resumes & Portfolios', 'Jobs topic', 'Temporary jobs', 'Private employment agency',
#            'Recruitement', 'Developer Jobs', 'Job search')
# bankruptcy <- c('Bankruptcy topic', 'Judicial Liquidation', 'Bankruptcy')
# housing <- c('Affordable housing', 'House price index', 'Apartments & Residential Rentals', 'Home Insurance',
#              'Home Improvement')
# news <- c('Economy News', 'Business News', 'World News', 'Politics', 'Newspapers')
# construction <- c('Flooring', 'Construction Consulting & Contracting', 'Swimming Pools & Spas', 'Civil Engineering', 'Construction & Maintenance')
# pers_fin <- c('Investment','Investing','Financial Planning')
# busin_serv <- c('Data Management', 'Enterprise Technology', 'Accounting & Auditing', 'CAD & CAM', 'Development Tools',
#                 'Customer Relationship Management (CRM)', 'Printing & Publishing', 'Corporate Events', 'Computer Security',
#                 'Outsourcing', 'Distribution & Logistics', 'Computer Servers', 'Consulting', 'Web Hosting & Domain Registration',
#                 'Enterprise Resource Planning (ERP)', 'Business Operations', 'Commercial Vehicles')
# ind_activ <- c('Agriculture & Forestry', 'Agrochemicals', 'Aviation', 'Business & Industrial',
#                'Chemicals Industry', 'Textiles & Nonwovens', 'Coatings & Adhesives', 'Food Production',
#                'Dyes & Pigments', 'Freight & Trucking', 'Transportation & Logistics', 'Mail & Package Delivery',
#                'Manufacturing')
#
#
# topics_paper <- c("Birthday", "Private employment agency" , "House moving", "Unemployment benefits", "Recruitment", "Investment",
#             "Lawyer", "Jobs", "Economic crisis", "Unemployment", "Financial crisis", 'Public debt', "Office space", "Job search",
#             "Temporary jobs", "Housing bubble", "House price index", "Mortgage", "Crisis", "Loan", "Interest", "Student loan",
#             "Affordable housing", "Recession", "Krach", "Bank", "Bankruptcy", "Exportation", "Commercial Building", "Luggage",
#             "Judicial Liquidation", "Foreclosure")
#
# categories_paper <- as_tibble(read.csv("~/ifwtrends/data/Kategorien_Woloszko.csv", header = F))
# names(categories_paper) = "category"
#
#
# cats = as_tibble(categories) %>% select(category = name)
#
# length(crisis) <- 41
# length(unemp) <- 41
# length(credit) <- 41
# length(consum) <- 41
# length(jobs) <- 41
# length(bankruptcy) <- 41
# length(housing) <- 41
# length(news) <- 41
# length(construction) <- 41
# length(pers_fin) <- 41
# length(busin_serv) <- 41
# length(ind_activ) <- 41
#
#
# vars <- tibble(crisis,
#               unemp,
#               credit,
#               consum,
#               jobs,
#               bankruptcy,
#               housing,
#               news,
#               construction,
#               pers_fin,
#               busin_serv,
#               ind_activ) %>%
#   pivot_longer(cols = everything(), names_to = "grouping", values_to = "variable") %>%
#   arrange(grouping) %>%
#   filter(!is.na(variable)) %>%
#   filter(!grepl("topic", variable))
#
#
#
# length(topics_paper) <- 1426
# lookup <- bind_cols(cats, topic = topics_paper) %>%
#   pivot_longer(cols = everything(), names_to = "id", values_to = "variable")
#
# res <- right_join(lookup, vars, by = "variable") %>%
#   select(variable, id, grouping) %>%
#   arrange(grouping)
#
#
