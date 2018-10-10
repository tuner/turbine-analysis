###
# This script build a co-occurence matrix of the zip codes of addresses and
# the districts of Helsinki.
#
# By normalizing the matrix either row- or column-wise, it can be used as
# probability distributions; at which probability an address in a specific
# zip code area belongs to a specific district.
#
# This can also be used for estimating the district where a person lives
# when only the zip code is known. And vice versa. However, the number of
# addresses is not really proportional to the population in a zip code area
# or district; some areas consists of single family houses and others
# contain mainly apartment houses.
#
# We could have alternatively used the overlapping surface areas for computing
# the matrix, but some districts and zip code areas are of enormous size and
# mainly contain sea or fields.
###


# Clear the workspace
rm(list=ls())

library(readr)
library(rgdal)

# Use longlat projection for both the addresses and districts
crs <- CRS("+proj=longlat")

addresses <- read_csv("../raw_data/uusimaa-fi.csv")

# Only retain addresses with a Helsinki postal code
addresses <- addresses[as.numeric(addresses$POSTCODE) < 1000, ]

address_points <- SpatialPoints(with(addresses, cbind(LON, LAT)), crs)
spatial_addresses <- SpatialPointsDataFrame(address_points, addresses)

districts <- readOGR("../raw_data/piirialuejako-1995-2016.gpkg", "perus_2016")

# Fix projection
districts <- spTransform(districts, crs)

# Retain only the addresses that actually reside inside the borders of Helsinki
spatial_addresses <- spatial_addresses[districts, ]

# Create a palette for plotting
pal <- c(colors(TRUE), colors(TRUE))

# Plot a pretty picture
# First, the addresses as colorful circles
plot(spatial_addresses[districts, ],
     col=pal[as.numeric(addresses$POSTCODE)],
     pch=19,
     cex=0.1
     )
# Second, overlay it with the borders of districts
plot(districts, add=T)

# Create matrix for addresses that are located in a specific postal code area and a specific district
zip_and_district <- matrix(nrow = length(unique(spatial_addresses$POSTCODE)),
                           ncol = length(districts$PERUS),
                           dimnames = list(
                             sort(unique(spatial_addresses$POSTCODE)),
                             sort(districts$PERUS)
                           ))
# Initialize with zeroes
zip_and_district[,] <- 0

for (district_id in districts$PERUS) {
  # Compute the frequency of the zip codes of the addresses that are located within a specific district
  counts <- table(spatial_addresses[districts[which(district_id == districts$PERUS), ], ]$POSTCODE)
  for (zip in names(counts)) {
    # Store the counts in the matrix
    zip_and_district[zip, district_id] = counts[[zip]]
  }
}

# Write to file
write.table(zip_and_district, "../derived_data/zip_and_district_helsinki.tsv", sep="\t")

