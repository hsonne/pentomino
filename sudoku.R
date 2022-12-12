#
# Sudouku
#

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  m_1 <- init_sudoku(
    2,0,7,0,0,0,6,0,4,
    0,4,0,5,0,2,0,1,0,
    9,0,0,6,0,1,0,0,7,
    0,0,1,4,0,5,9,0,0,
    0,0,0,0,0,0,0,0,0,
    0,0,9,1,0,3,2,0,0,
    3,0,0,8,0,6,0,0,2,
    0,8,0,7,0,9,0,3,0,
    7,0,6,0,0,0,1,0,5
  )
  
  m_2 <- init_sudoku(
    7,0,0,0,0,0,0,0,0,
    9,0,0,0,0,4,0,0,0,
    0,0,0,0,0,2,0,0,0,
    6,0,0,4,0,0,0,0,0,
    0,0,0,0,0,0,7,0,0,
    0,0,0,0,0,0,2,3,0,
    0,0,0,0,0,0,0,0,0,
    0,0,0,9,0,6,3,0,5,
    0,6,0,0,4,0,0,0,0
  )
  
  m_3 <- init_sudoku(
    0,0,0,0,0,6,0,0,0,
    0,9,5,7,0,0,3,0,0,
    4,0,0,0,9,2,0,0,5,
    7,6,4,0,0,0,0,0,3,
    0,0,0,0,0,0,0,0,0,
    2,0,0,0,0,0,9,7,1,
    5,0,0,2,1,0,0,0,9,
    0,0,7,0,0,5,4,8,0,
    0,0,0,8,0,0,0,0,0
  )
  
  m <- m_1

  print_sudoku(m)
  
  check_sudoku(m)

  m <- fill_simple(m)

  (choices <- get_next(m))
  
  (new_choices <- do.call(rbind, c(
    lapply(1:9, find_choice, m = m, choices = choices, type = 1L),
    lapply(1:9, find_choice, m = m, choices = choices, type = 2L),
    lapply(1:9, find_choice, m = m, choices = choices, type = 3L)
  )))
  
  m <- apply_choices(m, new_choices)
  m <- fill_simple(m)
  
  print_sudoku(m)
}

# init_sudoku ------------------------------------------------------------------
init_sudoku <- function(...)
{
  values <- c(...)
  
  stopifnot(length(values) == 81L)
  
  #rows <- strsplit(text_field, "\n")[[1L]]
  #chars <- unlist(strsplit(rows, ""))
  
  result <- matrix(
    data = values, #as.integer(gsub("-", "", chars)), 
    nrow = 9L, 
    byrow = TRUE
  )
  
  kwb.utils::addClass(result, "sudoku")
}


# print_sudoku -----------------------------------------------------------------
print_sudoku <- function(x, ...)
{
  collapse <- function(x) paste(x, collapse = " ")
  
  to_row <- function(y) {
    y <- as.character(y)
    y[y == "0"] <- " "
    paste(collapse(y[1:3]), 
          collapse(y[4:6]), 
          collapse(y[7:9]), sep = "|")    
  }
  
  writeLines(c(
    to_row(x[1L, ]), 
    to_row(x[2L, ]), 
    to_row(x[3L, ]), 
    "-----+-----+-----",
    to_row(x[4L, ]),
    to_row(x[5L, ]),
    to_row(x[6L, ]),
    "-----+-----+-----",
    to_row(x[7L, ]),
    to_row(x[8L, ]),
    to_row(x[9L, ])
  ))
}

# check_sudoku -----------------------------------------------------------------
check_sudoku <- function(x)
{
  stopifnot(inherits(x, "sudoku"))
  
  check_group <- function(x) {
    y <- x[x != 0L]
    all(y >= 1L & y <= 9L & !duplicated(y))
  }
  
  split_sudoku <- function(x) unname(split(m, get_group_matrix()))
  
  c(
    row_check = all(apply(x, 1L, check_group)),
    col_check = all(apply(x, 2L, check_group)),
    group_check = all(sapply(split_sudoku(x), check_group))
  )
}


# fill_simple ------------------------------------------------------------------
fill_simple <- function(m)
{
  finished <- FALSE
  
  while (!finished) {
    
    choices <- get_next(m)
    
    if (!is.null(choices) && nrow(choices) && ncol(choices) > 2L) {
      m <- apply_choices(m, choices)      
    } else {
      finished <- TRUE
    }
  }
  
  m
}

# get_next ---------------------------------------------------------------------
get_next <- function(x)
{
  is_empty <- x == 0L
  
  if (!any(is_empty)) {
    message("No more empty fields.")
    return()
  }
  
  positions <- which(is_empty, arr.ind = TRUE)
  
  candidates <- get_candidates(x, positions = positions)
  
  n_candidates <- lengths(candidates)
  
  has_one <- n_candidates == 1L
  
  if (any(has_one)) {
    result <- positions[has_one, , drop = FALSE]
    result <- cbind(result, choice = unlist(candidates[has_one]))
    return(result)
  }
  
  result <- positions
  rownames(result) <- sapply(candidates, paste, collapse = "|")
  
  result
}

# get_candidates ---------------------------------------------------------------
get_candidates <- function(x, i, j, positions = NULL) 
{
  stopifnot(inherits(x, "sudoku"))
  
  if (!is.null(positions)) {
    return(apply(
      X = positions, 
      MARGIN = 1L, 
      FUN = function(pos) get_candidates(x, pos[1L], pos[2L]),
      simplify = FALSE
    ))
  }
  
  group_matrix <- get_group_matrix()
  
  cands <- 1:9
  
  cands <- setdiff(cands, x[i, ])
  cands <- setdiff(cands, x[, j])
  setdiff(cands, x[which(group_matrix == group_matrix[i, j])])
}

# get_group_matrix -------------------------------------------------------------
get_group_matrix <- function()
{
  do.call(rbind, lapply(
    X = 0:2, 
    FUN = function(i) {
      do.call(cbind, lapply(
        X = seq_len(3L) + 3L * i, 
        FUN = function(k) matrix(rep(k, 9L), 3L)
      ))  
    }
  ))
}

# apply_choices ----------------------------------------------------------------
apply_choices <- function(m, choices)
{
  m[choices[, 1:2, drop = FALSE]] <- choices[, 3L]
  m
}

# find_choice ------------------------------------------------------------------
find_choice <- function(m, choices, index, type)
{
  stopifnot(type %in% 1:3)
  stopifnot(!is.null(rownames(choices)))
  
  # Relevant choices: rc
  rc <- list(row_choices, col_choices, group_choices)[[type]](choices, index)
  needs <- list(row_needs, col_needs, group_needs)[[type]](m, index)
  
  matches <- lapply(needs, function(choice) {
    which(sapply(rownames(rc), function(string_list) {
      in_string_list(choice, string_list)
    }))
  })
  
  is_unambiguous <- lengths(matches) == 1L
  
  if (!any(is_unambiguous)) {
    return(NULL)
  }
  
  result <- rc[unlist(matches[is_unambiguous]), , drop = FALSE]
  
  rownames(result) <- NULL
  
  cbind(result, choice = needs[is_unambiguous])
}

# row_choices ------------------------------------------------------------------
row_choices <- function(choices, row) {
  choices[choices[, "row"] == row, , drop = FALSE]
}

# col_choices ------------------------------------------------------------------
col_choices <- function(choices, col) {
  choices[choices[, "col"] == col, , drop = FALSE]
}

# group_choices ----------------------------------------------------------------
group_choices <- function(choices, group)
{
  group_coords <- which(get_group_matrix() == group, arr.ind = TRUE)
  paste_coords <- function(coords) paste(coords[, "row"], coords[, "col"])
  choices[paste_coords(choices) %in% paste_coords(group_coords), , drop = FALSE]
}

# row_needs --------------------------------------------------------------------
row_needs <- function(x, i) {
  setdiff(1:9, x[i, ])
}

# col_needs --------------------------------------------------------------------
col_needs <- function(x, j) {
  setdiff(1:9, x[, j])
}

# group_needs ------------------------------------------------------------------
group_needs <- function(x, k) {
  setdiff(1:9, x[which(get_group_matrix() == k, arr.ind = TRUE)])
}

# in_string_list ---------------------------------------------------------------
in_string_list <- function(x, s) 
{
  stopifnot(is.character(s), length(s) == 1L)
  x %in% strsplit(s, "[|]")[[1L]]
}
