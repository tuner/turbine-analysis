###
# This script aggregates the wind turbine survey data and joins it to
# the census data set.
#
# Steps:
#  1. For each turbine responder we assing a district by sampling from
#     the zip-conditional distribution. 
#  2. Group the turbine data by responders district and compute mean
#     for each variable.
#  3. Join the two data sets and write them to a file.
#
###


# Clear the workspace
rm(list=ls())

library(readr)

###############################################################################
### Load the zip and district co-occurence matrix

zip_and_district <- read.table("../derived_data/zip_and_district_helsinki.tsv", sep = "\t")

# For some reason read.table prefixes numeric column names with X. Remove it.
colnames(zip_and_district) <- sub("X", "", colnames(zip_and_district))


###############################################################################
#### Load demography data (Census)

demography <- read_delim("../raw_data/helsinki_demography_subset.csv", 
                         "\t",
                         escape_double = FALSE,
                         locale = locale(decimal_mark = ",", 
                                         grouping_mark = " ",
                                         encoding = "MACROMAN"), 
                         trim_ws = TRUE)

# Skip the first row, which contains totals for whole Helsinki
demography <- demography[-1, ]

# Split district column
districts <- as.data.frame(
  matrix(unlist(regmatches(demography$District, regexec("^(\\d+)\\s+(.*)$", demography$District))),
         ncol = 3, byrow = TRUE)[, 2:3])

colnames(districts) <- c("District id", "District name")

demography <- cbind(districts, demography)
demography$District <- NULL

# Remove "Suurpiiri", retain "Peruspiiri"
demography <- demography[nchar(as.character(demography$`District id`)) == 3, ]


###############################################################################
### Load turbine data

turbine <- read_delim("../raw_data/turbine.csv", 
                      "\t",
                      escape_double = FALSE,
                      trim_ws = TRUE)

# ID is useless, as we are interested in aggregates
turbine$ID <- NULL

# So are sex and age. We are interested in opinions.
turbine$Sex <- NULL
turbine$Age <- NULL

# Retain samples that contain a valid zip code and belongs to Helsinki
turbine <- turbine[grep("^ZIP_00\\d{3}", turbine$Zip), ]

# Zip codes contain an extra ZIP_ prefix. Remove it.
turbine$Zip <- sub("ZIP_", "", turbine$Zip)

# The data set appears to contain nonexistent Zip codes. Remove the affected rows.
turbine <- turbine[turbine$Zip %in% rownames(zip_and_district), ]



# Group the turbine data by zip code. 
zip_aggregated_turbine <- aggregate(turbine, list(turbine$`Zip`), mean)
# TODO: Compute per-district sample size

# Clean the columns. Fix names and drop Zip
zip_aggregated_turbine$`Zip` <- zip_aggregated_turbine$Group.1
zip_aggregated_turbine$Group.1 <- NULL

write.table(zip_aggregated_turbine,
            "../derived_data/zip_aggregated_turbine.csv",
            sep = "\t",
            row.names = FALSE)


# Now assign a district for each row.
# Some postal code areas overlap with multiple districts, so for each row we
# use the zip_and_districts matrix as discrete probability distributions and
# randomly choose a matching district.
turbine <- cbind(
  `District id` = sapply(turbine$Zip,
                         function(zip)
                           sample(colnames(zip_and_district),
                                  size = 1,
                                  prob = zip_and_district[zip, ],
                                  replace = TRUE)),
  turbine)


# Aggregate the data by the districts
aggregated_turbine <- aggregate(turbine, list(turbine$`District id`), mean)
# TODO: Compute per-district sample size

# Clean the columns. Fix names and drop Zip
aggregated_turbine$`District id` <- aggregated_turbine$Group.1
aggregated_turbine$Group.1 <- NULL
aggregated_turbine$Zip <- NULL

# Compute sample sizes
aggregated_turbine$n <- table(turbine$`District id`)[aggregated_turbine$`District id`]

# Combine the two data sets
combined_data <- merge(aggregated_turbine, demography, by = "District id")


write.table(combined_data,
            "../derived_data/combined_data.csv",
            sep = "\t",
            row.names = FALSE)

