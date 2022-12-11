#
# Sudouku
#

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  m <- init_sudoku(text_field = "
2-7---6-4
-4-5-2-1-
9--6-1--7
--14-59--
---------
--91-32--
3--8-6--2
-8-7-9-3-
7-6---1-5")
  
  print_sudoku(m)
  check_sudoku(m)
  
  (choices <- get_next(m))
  
  m <- apply_choices(m, choices)

  row_choices <- find_choice_for_row(m, choices, row = 1L)
  
  m <- apply_choices(m, row_choices)
}

# in_string_list ---------------------------------------------------------------
in_string_list <- function(x, s) 
{
  stopifnot(is.character(s), length(s) == 1L)
  x %in% strsplit(s, "[|]")[[1L]]
}

# find_choice_for_row ----------------------------------------------------------
find_choice_for_row <- function(m, choices, row)
{
  stopifnot(!is.null(rownames(choices)))

  row_choices <- function(choices, row) choices[choices[, "row"] == row, "col"]
  
  rc <- row_choices(choices, row)
  rc_names <- names(rc)
  
  rn <- row_needs(m, row)
  
  matches <- lapply(rn, function(choice) {
    which(sapply(seq_along(rc), function(i) {
      in_string_list(choice, names(rc)[i])
    }))
  })
  
  is_safe <- lengths(matches) == 1L
  
  if (!any(is_safe)) {
    return(NULL)
  }
  
  cbind(
    row = row, 
    col = unname(rc[unlist(matches[is_safe])]), 
    choice = rn[is_safe]
  )
}

# init_sudoku ------------------------------------------------------------------
init_sudoku <- function(text_field)
{
  rows <- strsplit(text_field, "\n")[[1L]]
  chars <- unlist(strsplit(rows, ""))
  
  result <- matrix(
    data = as.integer(gsub("-", "", chars)), 
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
    y[is.na(y)] <- " "
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
    y <- x[!is.na(x)]
    all(y >= 1L & y <= 9L & !duplicated(y))
  }

  split_sudoku <- function(x) unname(split(m, get_group_matrix()))
  
  c(
    row_check = all(apply(x, 1L, check_group)),
    col_check = all(apply(x, 2L, check_group)),
    group_check = all(sapply(split_sudoku(x), check_group))
  )
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

# get_next ---------------------------------------------------------------------
get_next <- function(x)
{
  positions <- which(is.na(x), arr.ind = TRUE)
  
  candidates <- get_candidates(x, positions = positions)
  
  n_candidates <- lengths(candidates)
  
  is_safe <- n_candidates == 1L
  
  if (!any(is_safe)) {
    has_two <- n_candidates == 2L
    result <- positions[has_two, , drop = FALSE]
    rownames(result) <- sapply(candidates[has_two], paste, collapse = "|")
    return(result)
  }
  
  cbind(
    positions[is_safe, , drop = FALSE], 
    choice = unlist(candidates[is_safe])
  )
}

# get_candidates ---------------------------------------------------------------
get_candidates <- function(x, i, j, positions = NULL) 
{
  stopifnot(inherits(x, "sudoku"))
  
  if (!is.null(positions)) {
    return(apply(
      X = positions, 
      MARGIN = 1L, 
      FUN = function(ij) get_candidates(x, ij[1L], ij[2L]),
      simplify = FALSE
    ))
  }
  
  group_matrix <- get_group_matrix()
  
  cands <- 1:9
  
  cands <- setdiff(cands, x[i, ])
  cands <- setdiff(cands, x[, j])
  setdiff(cands, x[which(group_matrix == group_matrix[i, j])])
}

# apply_choices ----------------------------------------------------------------
apply_choices <- function(m, choices)
{
  m[choices[, -3L, drop = FALSE]] <- choices[, 3L]
  m
}

row_needs <- function(x, i)
{
  setdiff(1:9, x[i, ])
}

col_needs <- function(x, j)
{
  setdiff(1:9, x[, j])
}
