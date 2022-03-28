#
# Pentomino: My Favourite Puzzle
#
# The Pentomino puzzle consists of twelve unique parts, each of which is 
# composed of five unit squares. I gave each part the name of a letter:
#
# I: █  L: █    Y: █    V: █     T: █ █ █  X:  █      
#    █     █       █ █     █          █      █ █ █
#    █     █       █       █ █ █      █        █
#    █     █ █     █
#    █
#
# M: █ █    E: █ █     U: █   █  K:  █ █  P: █ █  S:  █ █ 
#      █ █       █ █ █    █ █ █    █ █       █ █      █
#        █                           █       █      █ █
# 
# The task is to puzzle the parts together into a rectangular field with a size
# of 12 * 5 = 60 unit squares, i.e. sized 3 x 15, 5 x 12, 6 x 10.
#
# The following script finds all solutions for the 6 x 10 rectangle. 
# 
# Instructions:
# - Source the whole script to define the functions below
# - Manually go through the lines within the "if (FALSE) {...}" block
#
if (FALSE) 
{
  # Give one known solution, just as a simple means of defining the shapes of
  # all pieces "at once"
  template_strings <- matrix(nrow = 6L, c(
    "TTTXMMYYYY",
    "ITXXXMMYKK",
    "ITSXEEMKKV",
    "IPSSSEEEKV",
    "IPPUSULVVV",
    "IPPUUULLLL"
  ))
  
  # Convert the matrix of strings to a matrix of character
  template_matrix <- matrix(nrow = 6L, byrow = TRUE, unlist(
    strsplit(template_strings, "")
  ))
  
  # Determine the names of the different pieces
  piece_names <- unique(c(template_matrix))
  
  # Check that there are exactly 12 pieces
  stopifnot(length(piece_names) == 12L)

  # Name the piece names (so that lapply() will use these names)
  names(piece_names) <- piece_names
  
  # For each piece, determine the coordinates of their unit squares  
  pieces <- lapply(piece_names, function(name) {
    to_coords(m = template_matrix, find = name)
  })
  
  # Check that each piece consists of exactly five unit squares
  stopifnot(all(sapply(pieces, nrow) == 5L))
  
  # Create all possible variations of the pieces that result from turning or
  # flapping the pieces
  variations <- lapply(piece_names, function(name) {
    create_variants(to_matrix(pieces[[name]], value = name))
  })
  
  # Calculate the coordinates of the corresponding unit squares
  all_coords <- lapply(piece_names, function(name) {
    lapply(variations[[name]], to_coords, find = name)
  })
  
  # Recalculate the coordinates so that they are relative to the most upper left
  # unit square
  for (i in seq_along(all_coords)) {
    all_coords[[i]] <- lapply(all_coords[[i]], function(x) {
      relative_to(x, upper_left(x))
    })
  }
  
  # Define a vector of colours to be used when printing a solution
  colours <- stats::setNames(
    grDevices::gray.colors(length(all_coords)), 
    names(all_coords)
  )
  
  # Create the representation of an empty puzzle area (rectangle)
  emptyfield <- matrix(rep("", 60L), nrow = 6L)

  # Define the coordinates of the positions of the X piece. In order to prevent
  # the puzzler from finding duplicate solutions (that can be derived from an
  # existing solution by flapping horizontally or vertically) we fix the 
  # position of the (fully symmetric) X piece before we start the actual 
  # puzzling.
  x_startpositions <- list(
    c(1L, 3L), # -> 389 solutions
    c(1L, 4L), # -> 342
    c(1L, 5L), # -> 291
    c(2L, 2L), # -> 442
    c(2L, 3L), # -> 262
    c(2L, 4L), # -> 276
    c(2L, 5L)  # -> 337
  )
  
  # Global result list to store all solutions
  solutions <- list()

  # Main loop
  system.time({
    
    for (startpos in x_startpositions) {
      
      # Start with the empty playing area
      playfield <- emptyfield
      
      # Put the X piece into its current start position
      playfield <- put_part(playfield, add_offset(all_coords$X[[1L]], startpos), "X")
      
      # Start puzzling. The function will put solutions into the global list
      puzzle(playfield, all_coords, used = "X")
    }
  })

  length(solutions) # 2339
  lapply(solutions[1:100], findblobs:::plot_integer_matrix, colours)
  kwb.utils:::set_cache_dir("~/tmp")
  x <- kwb.utils:::cache_and_return(solutions)
  solutions_2  <- kwb.utils:::loadObject(kwb.utils:::get_cached_file("solutions"), "x")
  identical(solutions, solutions_2)
}

# Functions --------------------------------------------------------------------
to_coords <- function(m, find = "X") {
  stopifnot(is.matrix(m))
  x <- which(m == find, arr.ind = TRUE)
  relative_to(x, ref = c(min(x[, 1L]), min(x[, 2L])), offset = 1L)
}

relative_to <- function(x, ref, offset = 0L) {
  add_offset(x, - ref + offset)
}

add_offset <- function(x, ref) {
  x[, 1L] <- x[, 1L] + ref[1L]
  x[, 2L] <- x[, 2L] + ref[2L]
  x
}

to_matrix <- function(p, value = "X") {
  m <- matrix("", nrow = max(p[, "row"]), ncol = max(p[, "col"]))
  m[p] <- value
  m
}

turn <- function(m) {
  matrix(
    ncol = nrow(m), 
    sapply(rev(seq_len(nrow(m))), function(i) m[i, , drop = FALSE])
  )
}

flip <- function(m, vertical = TRUE) {
  if (vertical) {
    m[, rev(seq_len(ncol(m))), drop = FALSE]
  } else {
    m[rev(seq_len(nrow(m))), , drop = FALSE]
  }
}

matrix_in_list <- function(m, L) {
  stopifnot(is.list(L), is.matrix(m))
  for (x in L) {
    if (identical(x, m)) return(TRUE)
  }
  FALSE
}

add_if_new <- function(m, L) {
  stopifnot(is.list(L), is.matrix(m))
  if (matrix_in_list(m, L)) {
    return(L)
  }
  c(L, list(m))
}

create_variants <- function(m)
{
  add_turns <- function(v, m) {
    v <- add_if_new(m, v)
    for (i in 1:3) {
      m <- turn(m)
      v <- add_if_new(m, v)
    }
    v
  }
  
  variants <- list()
  variants <- add_turns(variants, m)
  variants <- add_turns(variants, flip(m, vertical = TRUE))
  variants <- add_turns(variants, flip(m, vertical = FALSE))
  
  variants
}

can_be_put <- function(coords, m)
{
  dm <- dim(m)
  rows <- coords[, 1L]
  cols <- coords[, 2L]
  
  all(rows >= 1L & rows <= dm[1L] & cols >= 1L & cols <= dm[2L]) && 
    all(m[coords] == "")
}

put_part <- function(playfield, target_coords, part_name)
{
  if (! can_be_put(target_coords, playfield))  {
    return(NULL)
  }
  
  `[<-`(playfield, target_coords, part_name)
}

next_ref <- function(x) {
  free_coords <- which(x == "", arr.ind = TRUE)
  
  if (nrow(free_coords) == 0L) {
    return(NULL)
  }
  
  min_row <- min(free_coords[, 1L])
  min_col <- min(free_coords[free_coords[, 1L] == min_row, 2L])
  c(min_row, min_col)
}

used_parts <- function(m)
{
  setdiff(unique(c(m)), "")
}

puzzle <- function(playfield, all_coords, used = character())
{
  ref <- next_ref(playfield)
  
  if (is.null(ref)) {
    solutions[[length(solutions) + 1L]] <<- playfield
    cat(length(solutions), "solutions found.\n")
    findblobs:::plot_integer_matrix(playfield, colours)
    #readline("Continue...")
    return(playfield)
  }
  
  (possible_parts <- setdiff(names(all_coords), used))
  
  if (length(possible_parts) == 0L) {
    return(NULL)
  }
  
  for (part_name in possible_parts) {
    for (coords in all_coords[[part_name]]) {
      target_coords <- add_offset(coords, ref)
      new_playfield <- put_part(playfield, target_coords, part_name)
      if (! is.null(new_playfield)) {
        puzzle(
          playfield = new_playfield, 
          all_coords = all_coords,
          used = c(used, part_name)
        )
      }
    }
  }
}

upper_left <- function(x) {
  c(1L, min(x[x[, 1L] == 1L, 2L]))
}
