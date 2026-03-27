adapted_appendixs5 <- read.csv("adapted_appendixs5.csv", stringsAsFactors = FALSE)

# switch rows & columns
new_header <- adapted_appendixs5[,1]
data_no_first_col <- adapted_appendixs5[,-1]
transposed_data <- t(data_no_first_col)
df_t <- as.data.frame(transposed_data, stringsAsFactors = FALSE)
colnames(df_t) <- new_header
original_header <- colnames(adapted_appendixs5)[-1]
transposed_adapted_appendixs5 <- cbind(OriginalHeader = original_header, df_t)

# rename header titles
colnames(transposed_adapted_appendixs5)[1] <- "SampleID"

# adjust row numbers
rownames(adapted_appendixs5) <- 1:nrow(adapted_appendixs5)
rownames(transposed_adapted_appendixs5) <- 1:nrow(transposed_adapted_appendixs5)

###

# check names of regions
list_regions <- unique(transposed_adapted_appendixs5$Region)
writeLines(list_regions, "list_regions.txt")

# write csv file with only red sea data
transposed_adapted_appendixs5_red_sea <- subset(transposed_adapted_appendixs5, Region == "Red Sea")
rownames(transposed_adapted_appendixs5_red_sea) <- 1:nrow(transposed_adapted_appendixs5_red_sea)
write.csv(transposed_adapted_appendixs5_red_sea, "transposed_adapted_appendixs5_red_sea.csv", row.names = FALSE)

###

adapted_appendixs1 <- read.csv("adapted_appendixs1.csv", stringsAsFactors = FALSE)

# merge two files
library(dplyr)
joined_data_red_sea <- full_join(adapted_appendixs1, transposed_adapted_appendixs5_red_sea, by = "SampleID", suffix = c(".s1", ".s5"))
common_cols <- intersect(names(adapted_appendixs1), names(transposed_adapted_appendixs5_red_sea))
common_cols <- setdiff(common_cols, "SampleID")
for (col in common_cols) {
  s5_col <- paste0(col, ".s5")
  if (s5_col %in% names(joined_data_red_sea)) {
    joined_data_red_sea[[s5_col]] <- NULL
  }
  s1_col <- paste0(col, ".s1")
  if (s1_col %in% names(joined_data_red_sea)) {
    names(joined_data_red_sea)[names(joined_data_red_sea) == s1_col] <- col
  }
}

write.csv(joined_data_red_sea, "joined_data_red_sea.csv", row.names = FALSE)

# extract major subclade names
general_cols <- 1:10
subclade_cols <- 11:ncol(joined_data_red_sea)
subclade_data <- joined_data_red_sea[, subclade_cols]
max_indices <- max.col(subclade_data, ties.method = "first")
major_sublclade <- colnames(subclade_data)[max_indices]
proportion <- apply(subclade_data, 1, max)
cleaned_joined_data_red_sea <- cbind(
  joined_data_red_sea[, general_cols],
  Major_Sublclade = major_sublclade,
  Proportion = proportion
)

# clean naming of subclades
cleaned_joined_data_red_sea$Major_Sublclade <- sub("^GS_", "", cleaned_joined_data_red_sea$Major_Sublclade)

# add clade row
cleaned_joined_data_red_sea$Major_Clade <- substr(cleaned_joined_data_red_sea$Major_Sublclade, 1, 1)

write.csv(cleaned_joined_data_red_sea, "cleaned_joined_data_red_sea.csv", row.names = FALSE)

