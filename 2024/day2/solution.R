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

count_safe_reports("example.txt")

is_safe <- function(report) {
  report <- report[!is.na(report)]
  deltas <- diff(report)
  sgns <- sign(deltas)
  if (length(unique(sgns)) != 1) {
    return(FALSE)
  }
  mags <- abs(deltas)
  all(mags %in% 1:3)
}

count_safe_reports <- function(input_file) {
  df <- read.table(input_file, header = FALSE, fill = NA)
  df |> apply(1, is_safe) |> sum()
}

count_safe_reports("input.txt")

is_safe_problem_dampener <- function(report) {
  report <- report[!is.na(report)]
  n <- length(report)
  for (i in 1:n) {
    if (is_safe(report[-i])) {
      return(TRUE)
    }
  }
  FALSE
}


count_safe_reports_problem_dampener <- function(input_file) {
  df <- read.table(input_file, header = FALSE, fill = NA)
  df |> apply(1, is_safe_problem_dampener) |> sum()
}

