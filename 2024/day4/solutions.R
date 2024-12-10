count_xmas <- function(string) {
  matches <- gregexpr("XMAS", string)
  matches[matches != -1] |> length()
}

reverse <- function(string) {
  string |>
    strsplit(split = "") |>
    unlist() |>
    rev() |>
    paste0(collapse = "")
}

count_along_rows <- function(df) {
  row_counts <- df |>
    apply(1, count_xmas) |>
    sum()
  row_counts <- row_counts + (df |>
    apply(1, \(row) count_xmas(reverse(row))) |>
    sum())
  row_counts
}

count_along_cols <- function(df) {
  cols <- df |>
    apply(1, \(row) {
      strsplit(row, "") |>
        unlist() |>
        unname()
    }) |>
    apply(1, \(row) paste0(row, collapse = ""))
  col_counts <- cols |>
    sapply(count_xmas) |>
    sum()
  col_counts <- col_counts + (cols |> sapply(\(col) count_xmas(reverse(col))) |> sum())
  col_counts
}

extract_all_diags_inner <- function(char_df, thresh = 4) {
  diags <- c()
  nrows <- nrow(char_df)
  ncols <- ncol(char_df)
  for (i in seq_len(nrows - 1)) {
    for (j in seq_len(ncols - 1)) {
      d <- diag(char_df[i:nrows, j:ncols]) |> paste0(collapse = "")
      if (nchar(d) >= thresh) {
        diags <- c(diags, d)
      }
    }
  }
  diags
}

extract_all_diags <- function(df, thresh = 4) {
  char_df <- df |>
    apply(1, \(row) {
      strsplit(row, "") |>
        unlist() |>
        unname()
    }) |>
    t()
  diags <- c(
    extract_all_diags_inner(char_df, thresh = thresh),
    extract_all_diags_inner(char_df[, rev(seq_len(ncol(char_df)))], thresh = thresh)
  )
  diags
}

count_along_diags <- function(df, thresh = 4) {
  diags <- extract_all_diags(df, thresh = thresh)
  diag_count <- diags |>
    sapply(count_xmas) |>
    sum()
  diag_count <- diag_count + (diags |> sapply(\(d) count_xmas(reverse(d))) |> sum())
  diag_count
}

solution_puzzle_1 <- function(input_path, thresh = 4) {
  df <- read.table(input_path)
  row_counts <- count_along_rows(df)
  col_counts <- count_along_cols(df)
  diag_counts <- count_along_diags(df, thresh = thresh)
  row_counts + col_counts + diag_counts
}
solution_puzzle_1("example.txt") |> print()
