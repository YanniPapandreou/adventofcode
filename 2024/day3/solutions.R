scan_mem <- function(path) {
  mem <- readLines(path)
  mem
}

extract_uncorrupted <- function(mem, pattern = "mul\\([0-9]*,[0-9]*\\)", debug = FALSE) {
  if (debug) {
    stringr::str_view(mem, pattern)
  }
  matches <- gregexpr(pattern, mem)
  uncorrupted_data <- lapply(seq_along(mem), function(i) {
    mem_line <- mem[[i]]
    line_matches <- matches[[i]]
    data <- sapply(seq_along(line_matches), function(j) {
      start <- line_matches[[j]]
      len <- attr(line_matches, "match.length")[[j]]
      mem_line |> substr(start, start + len - 1)
    })
    data
  })
  uncorrupted_data
}

perform_mul <- function(mul_instruction) {
  gsub("mul\\(|\\)", "", mul_instruction) |>
    strsplit(",") |>
    unlist() |>
    as.numeric() |>
    prod()
}

process_uncorrupted <- function(uncorrupted_data) {
  uncorrupted_data |>
    sapply(function(data) {
      sapply(data, perform_mul) |>
        unname() |>
        sum()
    }) |>
    sum()
}

solve_puzzle_1 <- function(input_path) {
  input_path |>
    scan_mem() |>
    extract_uncorrupted() |>
    process_uncorrupted()
}

print(glue::glue("Solution to example problem in puzzle 1: {solve_puzzle_1('example.txt')}"))
print(glue::glue("Solution to puzzle 1: {solve_puzzle_1('input.txt')}"))

split_first <- function(input, pattern) {
  stopifnot("`input` is a character vector of length 1" = length(input) == 1)
  matches <- regexpr(pattern, input)
  if (matches[[1]] == -1) {
    # no match, return original string
    return(input)
  }
  start <- matches[[1]]
  end <- start + attr(matches, "match.length") - 1
  before <- substr(input, 1, start - 1)
  after <- substr(input, end + 1, nchar(input))
  return(c(before, after))
}

split_and_label <- function(part, pattern, label) {
  labels <- names(part) |> as.logical()
  stopifnot(length(labels) == 1)
  parts <- split_first(unname(part), pattern)
  new_labels <- rep(labels, 2)
  new_labels[[2]] <- label
  parts <- setNames(parts, new_labels)
  parts
}

split_mem <- function(mem) {
  stopifnot(
    "`mem` is a character vector of length 1" = length(mem) == 1,
    "`mem` has names" = !is.null(names(mem)),
    "`mem` has correct names (either 'TRUE' or 'FALSE')" = all(names(mem) %in% c("TRUE", "FALSE"))
  )
  if (!grepl("don't()", mem, fixed = TRUE) && !grepl("do()", mem, fixed = TRUE)) {
    return(mem)
  } else if (grepl("don't()", mem, fixed = TRUE)) {
    parts <- split_and_label(mem, "don't\\(\\)", FALSE)
    return(c(split_mem(parts[1]), split_mem(parts[2])))
  } else {
    parts <- split_and_label(mem, "do\\(\\)", TRUE)
    return(c(split_mem(parts[1]), split_mem(parts[2])))
  }
}

solve_puzzle_2 <- function(input_path) {
  parts <- input_path |>
    scan_mem() |>
    paste0(collapse = "") |>
    setNames(TRUE) |>
    split_mem()
  parts[as.logical(names(parts))] |>
    unname() |>
    extract_uncorrupted() |>
    process_uncorrupted()
}

print(glue::glue("Solution to example problem in puzzle 2: {solve_puzzle_2('example2.txt')}"))
print(glue::glue("Solution to puzzle 2: {solve_puzzle_2('input.txt')}"))
