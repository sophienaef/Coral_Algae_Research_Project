abund_and_meta_txt <- "abund_and_meta.txt"

# code to read txt into csv file
lines <- readLines(abund_and_meta_txt)
parse_line <- function(line) {
  parts <- strsplit(line, "\t")[[1]]
  parts <- trimws(parts)
  parts
}
parsed_lines <- lapply(lines, parse_line)
max_cols <- max(sapply(parsed_lines, length))
parsed_lines_padded <- lapply(parsed_lines, function(x) {
  length(x) <- max_cols
  x
})
abund_and_meta <- do.call(rbind, parsed_lines_padded)
abund_and_meta <- as.data.frame(abund_and_meta, stringsAsFactors = FALSE)
colnames(abund_and_meta) <- abund_and_meta[1, ]
abund_and_meta <- abund_and_meta[-1, ]
abund_and_meta[, 1] <- paste(abund_and_meta[, 1], abund_and_meta[, 2], sep = " ")
abund_and_meta <- abund_and_meta[, -2]
write.csv(abund_and_meta, "abund_and_meta.csv", row.names = FALSE, na = "")

# switch rows & columns
new_header <- abund_and_meta[,1]
data_no_first_col <- abund_and_meta[,-1]
transposed_data <- t(data_no_first_col)
df_t <- as.data.frame(transposed_data, stringsAsFactors = FALSE)
colnames(df_t) <- new_header
original_header <- colnames(abund_and_meta)[-1]
transposed_abund_and_meta <- cbind(OriginalHeader = original_header, df_t)

# rename header titles
colnames(transposed_abund_and_meta)[1] <- "ITS2_type_profile_UID"
colnames(transposed_abund_and_meta)[-1] <- gsub(" ", "_", colnames(transposed_abund_and_meta)[-1])

# adjust row numbers
rownames(abund_and_meta) <- 1:nrow(abund_and_meta)
rownames(transposed_abund_and_meta) <- 1:nrow(transposed_abund_and_meta)

# reorder columns
cols <- colnames(transposed_abund_and_meta)
new_order <- c(
  cols[1],
  cols[4],
  cols[2:3],
  if(length(cols) > 4) cols[5:length(cols)] else NULL
)
transposed_abund_and_meta <- transposed_abund_and_meta[, new_order]

# write transposed file
write.csv(transposed_abund_and_meta, "transposed_abund_and_meta.csv", row.names = FALSE, na = "")


###

