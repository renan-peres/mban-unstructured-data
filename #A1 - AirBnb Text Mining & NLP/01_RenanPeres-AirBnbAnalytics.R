#installing and loading the mongolite library to download the Airbnb data
# install.packages(c("mongolite", "openssl", "arrow"))
library(mongolite)
library(openssl)
library(arrow)

prepare_for_parquet <- function(df) {
	flattened_df <- jsonlite::flatten(df)

	for (column_name in names(flattened_df)) {
		column <- flattened_df[[column_name]]

		if (is.list(column)) {
			flattened_df[[column_name]] <- vapply(
				column,
				function(value) {
					if (is.null(value)) {
						return(NA_character_)
					}

					jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
				},
				character(1)
			)
		}
	}

	flattened_df
}

output_dir <- file.path("#A1 - AirBnb Text Mining & NLP", "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Connect to MongoDB cluster
connection_string <- 'mongodb+srv://user1:eh0yz55chR5L5k3l@cluster0.j6jqvfm.mongodb.net/?appName=Cluster0'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
airbnb_all <- airbnb_collection$find() ## This is huge and you need a ton of RAM memory
airbnb_all_parquet <- prepare_for_parquet(airbnb_all)
write_parquet(airbnb_all_parquet, file.path(output_dir, "airbnb_all.parquet"))

#######################################################
#1 subsetting your data based on a condition:
mydf <- airbnb_collection$find('{"bedrooms":2, "price":{"$gt":50}}')
mydf_parquet <- prepare_for_parquet(mydf)
write_parquet(mydf_parquet, file.path(output_dir, "airbnb_bedrooms2_price_gt50.parquet"))

#2 writing an analytical query on the data::
mydf_analytical <- airbnb_collection$aggregate('[{"$group":{"_id":"$room_type", "avg_price": {"$avg":"$price"}}}]')
write_parquet(mydf_analytical, file.path(output_dir, "airbnb_avg_price_by_room_type.parquet"))
