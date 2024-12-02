count_safe_reports <- function(input_file) {
  df <- read.table(input_file, header = FALSE, fill = NA)
  deltas <- df |> apply(1, diff)
  filter_1 <- sign(deltas) |>
    apply(2, \(col) length(unique(col[!is.na(col)])) == 1)
  filter_2 <- deltas[, filter_1] |>
    abs() |>
    apply(2, \(col) all(unique(col[!is.na(col)]) %in% 1:3))
  sum(filter_2)
}

count_safe_reports("input.txt")

is_safe <- function(report) {

}
