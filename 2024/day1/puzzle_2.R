compute_similarity_score <- function(df) {
  stopifnot(ncol(df) == 2)
  left <- df[, 1]
  right <- df[, 2]
  left_counts <- table(left)
  right_counts <- table(right)
  common <- intersect(names(left_counts), names(right_counts))
  scores <- left_counts[common] * as.integer(common) * right_counts[common]
  sum(scores)
}

# df <- read.table("example.txt", header = FALSE)
df <- read.table("input.txt", header = FALSE)
compute_similarity_score(df) |> print()
