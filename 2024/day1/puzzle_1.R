compute_total_distance <- function(df) {
  stopifnot(ncol(df) == 2)
  df |>
    apply(2, \(col) sort(col)) |>
    apply(1, \(row) abs(row[[1]] - row[[2]])) |>
    sum()
}

df <- read.table("input.txt", header = FALSE)
compute_total_distance(df)
