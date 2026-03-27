ChlA_data_txt <- "ChlA_data.txt"

# code to read txt into csv file
lines <- readLines(ChlA_data_txt)
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
ChlA_data <- do.call(rbind, parsed_lines_padded)
ChlA_data <- as.data.frame(ChlA_data, stringsAsFactors = FALSE)
colnames(ChlA_data) <- ChlA_data[1, ]
ChlA_data <- ChlA_data[-1, ]
ChlA_data[, 1] <- paste(ChlA_data[, 1], ChlA_data[, 2], sep = " ")
ChlA_data <- ChlA_data[, -2]

write.csv(ChlA_data, "ChlA_data.csv", row.names = FALSE, na = "")


###

sup_data_1 <- read.csv("sup_data_1.csv", stringsAsFactors = FALSE)

# merge sample.ID and location column
sup_data_1$combined <- paste(as.character(sup_data_1$sample.ID), as.character(sup_data_1$location), sep = " ")
sup_data_1 <- sup_data_1[, !(colnames(sup_data_1) %in% c("sample.ID", "location"))]
colnames(sup_data_1)[colnames(sup_data_1) == "combined"] <- "sample.ID"
sup_data_1 <- sup_data_1[, c("sample.ID", setdiff(colnames(sup_data_1), "sample.ID"))]

common_ids <- intersect(sup_data_1$sample.ID, ChlA_data$sample.ID)
write.table(common_ids, file = "common_sample_IDs.txt", row.names = FALSE, col.names = "sample.ID", quote = FALSE)

