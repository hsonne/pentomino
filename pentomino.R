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
  template_rows <- get_template_rows()
  
  # Show the solution
  writeLines(template_rows)

  # Create pieces from template, provide piece coordinates  
  piece_info <- get_piece_info_from_template(template_rows)

  # Piece names and number of pieces
  piece_names <- names(piece_info$pieces)
  n_pieces <- length(piece_names)
  
  # Define a vector of colours to be used when printing a solution
  colours <- stats::setNames(grDevices::gray.colors(n_pieces), piece_names)
  
  # Create the representation of an empty puzzle area (rectangle)
  emptyfield <- get_empty_field()

  # Prepare vector holding TRUE for used pieces and FALSE for unused pieces
  x_used <- stats::setNames(logical(n_pieces), piece_names)
  x_used[["X"]] <- TRUE
  
  # Global result list to store all solutions
  solutions <- list()
  tmp_solution <- NULL
  
  # Provide list of piece coordinates
  piece_coords <- piece_info$piece_coords
  
  # Main loop
  system.time({
    
    for (startpos in get_x_startpositions()) {

      # Put X piece into empty playfield at its current start position
      playfield <- put_piece(
        emptyfield, 
        coords = add_offset(piece_coords$X[[1L]], startpos), 
        name = "X"
      )
      
      # Start puzzling. The function will put solutions into the global list
      puzzle(playfield, piece_coords, used = x_used)
    }
  })

  #    User      System verstrichen 
  # 1309.60        0.75     1318.87
  
  # Save solutions  
  kwb.utils:::set_cache_dir("~/tmp")
  x <- kwb.utils:::cache_and_return(solutions, name = "pentomino_solutions")
}

# Load solutions ---------------------------------------------------------------
if (FALSE)
{
  kwb.utils:::set_cache_dir("~/tmp")
  
  # Read all solutions
  solutions <- kwb.utils:::get_cached("pentomino_solutions")

  length(solutions) # 2339

  # Get piece information
  piece_info <- get_piece_info_from_template(get_template_rows())
  
  # Indices of fields occupied by part X  
  indices_list <- lapply(get_x_startpositions(), function(ref) {
    which("X" == put_piece(
      get_empty_field(), add_offset(piece_info$piece_coords$X[[1L]], ref), "X"
    ))
  })
  
  # Number of solutions per start position of part X
  counts <- sapply(indices_list, function(indices) {
    sum(sapply(solutions, function(x) {
      all(which(x == "X") == indices)
    }))
  })

  # Number of all solutions (for 6 x 12 playing area)
  stopifnot(sum(counts) == length(solutions))

  # Which start position of X has the least solutions?
  which.min(counts) # 5
  
  # Select solutions referring to X start position 5
  solutions_5 <- solutions[which(sapply(solutions, function(x) {
    identical(which(x == "X"), indices_list[[5L]])
  }))]
  
  length(solutions_5)
}

# Plot solutions ---------------------------------------------------------------
if (FALSE)
{
  solutions <- solutions_5 # (created above)
  
  n <- length(solutions)
  s1 <- solutions[[1L]]
  
  puzzle_ratio <- nrow(s1)/ncol(s1)
  postcard <- list(height.cm = 10.5, width.cm = 14.8)

  mfrow <- kwb.plot::bestRowColumnSetting(n, 6/10, do.call(`/`, postcard))
  nr <- mfrow[["nrow"]]
  nc <- mfrow[["ncol"]]
  
  mfrow <- optimal_row_column_pair(
    N = length(solutions),   
    target_width_height_ratio = 15/10
  )
  nr <- mfrow$nrow
  nc <- mfrow$ncol
  
  nr*nc-length(solutions)
  result <- arrange_solutions_in_matrix(solutions, nr, nc)
  View(result)
  nrow(result)/ncol(result)
  
  RColorBrewer::display.brewer.all(12L)
  
  #palette_name <- "Paired"
  palette_name <- "Set3"
  
  colours <- stats::setNames(
    RColorBrewer::brewer.pal(12L, palette_name), 
    unique(c(solutions[[1L]]))
  )
  
  a4 <- kwb.utils::DIN.A4()
  
  width.cm <- a4$height.cm
  height.cm <- a4$width.cm
  
  width.cm <- postcard$width.cm
  height.cm <- postcard$height.cm
  
  width.cm <- 15
  height.cm <- 10
  
  file <- file.path("pentomino.png")
  png(file, width = width.cm, height = height.cm, units = "cm", res = 600)
  findblobs:::plot_integer_matrix(result, colours)
  dev.off()
  kwb.utils::hsOpenWindowsExplorer(dirname(file))
  
  for (i in seq_along(solutions)) {
    kwb.plot::output_to_png(
      FUN = findblobs:::plot_integer_matrix, 
      args = list(solutions[[i]], colours), 
      filename = sprintf("solution_%03d", i), 
      res = 30
    )
  }
 
  images <- magick::image_read(
    dir(pattern = "^solution_...\\.png", full.names = TRUE)
  )
  
  magick::image_write_gif(images, path = "solutions.gif", delay = 0.5)
}

# Functions --------------------------------------------------------------------

get_template_rows <- function() c(
  "TTTXMMYYYY",
  "ITXXXMMYKK",
  "ITSXEEMKKV",
  "IPSSSEEEKV",
  "IPPUSULVVV",
  "IPPUUULLLL"
)

get_piece_info_from_template <- function(template_rows)
{
  # Convert the template rows to a matrix of character
  template_matrix <- matrix(nrow = 6L, byrow = TRUE, unlist(
    strsplit(template_rows, "")
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
  piece_coords <- lapply(piece_names, function(name) {
    lapply(variations[[name]], to_coords, find = name)
  })
  
  # Recalculate the coordinates so that they are relative to the most upper left
  # unit square
  for (i in seq_along(piece_coords)) {
    piece_coords[[i]] <- lapply(piece_coords[[i]], function(x) {
      relative_to(x, upper_left(x))
    })
  }
  
  list(pieces = pieces, piece_coords = piece_coords)
}

to_coords <- function(m, find = "X") {
  stopifnot(is.matrix(m))
  x <- which(m == find, arr.ind = TRUE)
  relative_to(x, ref = c(min(x[, 1L]), min(x[, 2L])), offset = 1L)
}

relative_to <- function(x, ref, offset = 0L) {
  add_offset(x, - ref + offset)
}

add_offset <- function(x, ref) {
  x + rep(ref, each = nrow(x))
}

to_matrix <- function(p, value = "X") {
  m <- matrix("", nrow = max(p[, "row"]), ncol = max(p[, "col"]))
  m[p] <- value
  m
}

create_variants <- function(m) {
  
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

add_if_new <- function(m, L) {
  stopifnot(is.list(L), is.matrix(m))
  if (matrix_in_list(m, L)) {
    return(L)
  }
  c(L, list(m))
}

matrix_in_list <- function(m, L) {
  stopifnot(is.list(L), is.matrix(m))
  for (x in L) {
    if (identical(x, m)) return(TRUE)
  }
  FALSE
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

upper_left <- function(x) {
  c(1L, min(x[x[, 1L] == 1L, 2L]))
}

get_empty_field <- function() {
  matrix(rep("", 60L), nrow = length(get_template_rows()))
}

# Define the coordinates of the positions of the X piece. In order to prevent
# the puzzler from finding duplicate solutions (that can be derived from an
# existing solution by flapping horizontally or vertically) we fix the 
# position of the (fully symmetric) X piece before we start the actual 
# puzzling.
get_x_startpositions <- function() list(
  c(1L, 3L),
  c(1L, 4L),
  c(1L, 5L),
  c(2L, 2L),
  c(2L, 3L),
  c(2L, 4L),
  c(2L, 5L)
)

put_piece <- function(playfield, coords, name) {
  if (can_be_put(coords, playfield))  {
    `[<-`(playfield, coords, name)
  } # else NULL
}

can_be_put <- function(coords, m) {
  dm <- dim(m)
  rows <- coords[, 1L]
  cols <- coords[, 2L]
  all(rows >= 1L & rows <= dm[1L] & cols >= 1L & cols <= dm[2L]) && 
    all(m[coords] == "")
}

puzzle <- function(
  playfield, 
  all_coords, 
  used = stats::setNames(logical(length(all_coords)), names(all_coords))
)
{
  #findblobs:::plot_integer_matrix(playfield, colours)
  #readline("Return...")
  
  if (all(used)) {
    solutions[[length(solutions) + 1L]] <<- playfield
    cat(length(solutions), "solutions found.\n")
    findblobs:::plot_integer_matrix(playfield, colours)
    #readline("Return...")
    
    return()
  }

  if (any(is_single(playfield != ""))) {
    #cat("Single empty fields found:")
    #findblobs:::plot_integer_matrix(playfield, colours)
    #readline("Return...")
    
    return()
  }

  #readline("Continue...")
  
  # if (sum(used) > 5L) {
  #   tmp_solution <<- playfield
  #   findblobs:::plot_integer_matrix(playfield, colours)
  #   readline("Continue...")
  # }

  ref <- next_ref(playfield)
  
  for (name in names(which(! used))) {
    for (coords in all_coords[[name]]) {
      new_playfield <- put_piece(playfield, add_offset(coords, ref), name)
      if (! is.null(new_playfield)) {
        puzzle(
          playfield = new_playfield, 
          all_coords = all_coords,
          used = `[<-`(used, name, TRUE)
        )
      }
    }
  }
}

next_ref <- function(x) {
  # Indices of non-empty fields, when walking down the lines
  indices <- which(t(x) == "")
  if (length(indices)) {
    i <- indices[1L] -1L
    nc <- ncol(x)
    c(i %/% nc, i %% nc) + 1L
  } # else NULL
}

is_single <- function(occupied) {
  
  ! occupied & 
    # occupied above: Add row of TRUE above matrix without last row
    rbind(TRUE, occupied[-nrow(occupied), ]) & 
    # occupied below: Add row of TRUE below matrix without first row
    rbind(occupied[-1L, ], TRUE) & 
    # occupied left: Add column of TRUE left to matrix without last column
    cbind(TRUE, occupied[, -ncol(occupied)]) &
    # occupied right: Add column of TRUE right to matrix without first column
    cbind(occupied[, -1L], TRUE)
}

arrange_solutions_in_matrix <- function(solutions, nrow, ncol) {
  
  n <- length(solutions)
  
  stopifnot(n <= nrow * ncol)
  
  indices <- seq_len(nrow * ncol)
  indices[-seq_len(n)] <- NA
  
  index_matrix <- matrix(indices, byrow = TRUE, ncol = ncol)
  
  solution_rows <- apply(index_matrix, 1L, function(i) {
    is_na <- is.na(i)
    c(
      solutions[i[! is_na]], 
      lapply(seq_len(sum(is_na)), function(i) get_empty_field())
    )
  })
  
  result <- do.call(rbind, lapply(solution_rows, function(x) {
    x <- lapply(x, function(xx) cbind(0L, xx))
    x[[length(x)]] <- cbind(x[[length(x)]], 0L)
    rbind(0L, do.call(cbind, x))
  }))
  
  result <- cbind(0L, result, 0L)
  result <- rbind(0L, result, 0L, 0L)
  
  result
}

optimal_row_column_pair <- function(
  N, target_width_height_ratio, size = 10, border = size
)
{
  n <- seq_len(N)
  m <- ceiling(N/n)
  pairs <- data.frame(nrow = n, ncol = m)
  
  stopifnot(pairs$nrow * pairs$ncol >= N)
  stopifnot(pairs$nrow * (pairs$ncol - 1L) < N)
  
  width <- pairs$ncol * (10 * size) + (pairs$ncol - 1L) * border + 4 * border
  height <- pairs$nrow * (6 * size) + (pairs$nrow - 1L) * border + 4 * border
  
  ratios <- width / height
  
  pairs[which.min(abs(ratios - target_width_height_ratio)), ]
}
