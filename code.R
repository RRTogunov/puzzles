# Load necessary libraries
library(igraph)
library(grid)
library(dplyr)
set.seed(1234)  # For reproducibility
# Define the puzzle size
grid_size <- 4  # 3x3 grid
num_pieces <- grid_size * grid_size

# Function to get row and column from position
get_row_col <- function(position, grid_size) {
  row <- ceiling(position / grid_size)
  col <- position - (row - 1) * grid_size
  return(list(row = row, col = col))
}

# Functions to get positions based on type
get_corner_positions <- function(grid_size) {
  return(c(1, grid_size, grid_size * (grid_size - 1) + 1, grid_size * grid_size))
}

get_edge_positions <- function(grid_size) {
  positions <- c()
  # Top edge (excluding corners)
  positions <- c(positions, 2:(grid_size - 1))
  # Bottom edge (excluding corners)
  positions <- c(positions, (grid_size * (grid_size - 1) + 2):(grid_size * grid_size - 1))
  # Left edge (excluding corners)
  positions <- c(positions, seq(grid_size + 1, grid_size * (grid_size - 2) + 1, by = grid_size))
  # Right edge (excluding corners)
  positions <- c(positions, seq(grid_size * 2, grid_size * (grid_size - 1), by = grid_size))
  return(positions)
}

get_center_positions <- function(grid_size) {
  all_positions <- 1:(grid_size * grid_size)
  corner_positions <- get_corner_positions(grid_size)
  edge_positions <- get_edge_positions(grid_size)
  center_positions <- setdiff(all_positions, c(corner_positions, edge_positions))
  return(center_positions)
}

get_required_rotation <- function(original_position, new_position, grid_size) {
  # browser()
  # Get outer edges for original and new positions
  original_outer_edges <- get_outer_edges(original_position, grid_size)
  new_outer_edges <- get_outer_edges(new_position, grid_size)
  
  # Determine the rotation needed to map the original outer edges to the new outer edges
  # For corner pieces, there are four possible orientations (0, 90, 180, 270 degrees)
  # For edge pieces, there are four possible orientations as well
  
  # Create a mapping of edges to orientations
  orientations <- list(
    "top_left" = 0,
    "top_right" = 90,
    "bottom_right" = 180,
    "bottom_left" = 270
  )
  
  # For corner pieces
  if (length(original_outer_edges) == 2 && length(new_outer_edges) == 2) {
    # Determine the orientation index for original and new positions
    original_orientation <- orientations[[paste(original_outer_edges, collapse = "_")]]
    new_orientation <- orientations[[paste(new_outer_edges, collapse = "_")]]
    
    # Calculate rotation needed
    rotation_needed <- (original_orientation - new_orientation) %% 360
    return(rotation_needed)
  }
  
  # For edge pieces
  edge_orientations <- c("top" = 0, "right" = 90, "bottom" = 180, "left" = 270)
  if (length(original_outer_edges) == 1 && length(new_outer_edges) == 1) {
    original_orientation <- edge_orientations[[original_outer_edges]]
    new_orientation <- edge_orientations[[new_outer_edges]]
    
    # Calculate rotation needed
    rotation_needed <- (original_orientation - new_orientation) %% 360
    return(rotation_needed)
  }
  
  # For center pieces, no rotation is needed
  return(0)
}
# Function to get outer edges of a position
get_outer_edges <- function(position, grid_size) {
  rc <- get_row_col(position, grid_size)
  outer_edges <- c()
  if (rc$row == 1) {
    outer_edges <- c(outer_edges, "top")
  }
  if (rc$row == grid_size) {
    outer_edges <- c(outer_edges, "bottom")
  }
  if (rc$col == 1) {
    outer_edges <- c(outer_edges, "left")
  }
  if (rc$col == grid_size) {
    outer_edges <- c(outer_edges, "right")
  }
  return(outer_edges)
}


# Get positions
corner_positions <- get_corner_positions(grid_size)
edge_positions <- get_edge_positions(grid_size)
center_positions <- get_center_positions(grid_size)

# Create data frame for pieces with initial rotations
pieces <- data.frame(
  piece_id = 1:num_pieces,
  type = rep(NA, num_pieces),
  initial_rotation = rep(0, num_pieces)  # Add initial_rotation column
)

# Assign types and initial rotations based on positions
for (i in 1:num_pieces) {
  position <- i
  piece_id <- i
  piece_type <- if (position %in% corner_positions) {
    "corner"
  } else if (position %in% edge_positions) {
    "edge"
  } else {
    "center"
  }
  pieces$type[piece_id] <- piece_type
  
  # # Determine initial rotation for the piece
  # initial_rotation <- get_required_rotation(piece_type, pieces$initial_rotation, position, grid_size)
  # pieces$initial_rotation[piece_id] <- initial_rotation
}

# Arrangement 1: Assign piece IDs equal to positions with their initial rotations
arrangement1 <- data.frame(
  piece_id = 1:num_pieces,                      # Piece IDs equal to positions
  position = 1:num_pieces,                      # Positions from 1 to num_pieces
  rotation = pieces$initial_rotation            # Use initial rotations
)



# Get piece IDs by type
corner_piece_ids <- pieces$piece_id[pieces$type == "corner"]
edge_piece_ids <- pieces$piece_id[pieces$type == "edge"]
center_piece_ids <- pieces$piece_id[pieces$type == "center"]

# Shuffle piece IDs within each type
corner_piece_ids_shuffled <- sample(corner_piece_ids)
edge_piece_ids_shuffled <- sample(edge_piece_ids)
center_piece_ids_shuffled <- if(length(center_piece_ids) == 1) center_piece_ids else sample(center_piece_ids)

# Keep positions the same as in arrangement1
arrangement2 <- data.frame(
  piece_id = c(corner_piece_ids_shuffled, edge_piece_ids_shuffled, center_piece_ids_shuffled),
  position = c(corner_positions, edge_positions, center_positions),
  rotation = 0  # We'll calculate rotations next
) %>% arrange(piece_id)


# Calculate rotations for arrangement2
arrangement2$rotation <- sapply(1:nrow(arrangement2), function(i) {
  piece_id <- arrangement2$piece_id[i]
  original_position <- piece_id  # Since piece IDs are equal to original positions
  new_position <- arrangement2$position[i]
  piece_type <- pieces$type[piece_id]
  
  if (piece_type == "center") {
    # For center pieces, assign a random rotation
    rotation_needed <- sample(c(0, 90, 180, 270), 1)
  } else {
    # Get the rotation needed
    rotation_needed <- get_required_rotation(original_position, new_position, grid_size)
  }
  
  return(rotation_needed)
})


# Function to get adjacent positions
get_adjacent_positions <- function(position, grid_size) {
  rc <- get_row_col(position, grid_size)
  adjacents <- list(
    top = if (rc$row > 1) position - grid_size else NA,
    bottom = if (rc$row < grid_size) position + grid_size else NA,
    left = if (rc$col > 1) position - 1 else NA,
    right = if (rc$col < grid_size) position + 1 else NA
  )
  return(adjacents)
}

# Function to get edge matches for an arrangement
get_edge_matches <- function(arrangement, grid_size) {
  edge_matches <- data.frame(
    piece_id = integer(),
    edge = character(),
    adjacent_piece_id = integer(),
    adjacent_edge = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(arrangement)) {
    piece <- arrangement[i, ]
    position <- piece$position
    rotation <- -piece$rotation %% 360
    adj_positions <- get_adjacent_positions(position, grid_size)
    
    # Map edges based on rotation
    edges <- c("top", "right", "bottom", "left")
    rotated_edges <- edges[((0:3 - rotation / 90) %% 4) + 1]
    names(rotated_edges) <- edges
    
    for (edge in names(adj_positions)) {
      adj_position <- adj_positions[[edge]]
      if (!is.na(adj_position)) {
        adj_piece <- arrangement[arrangement$position == adj_position, ]
        adj_piece_id <- adj_piece$piece_id
        adj_rotation <- -adj_piece$rotation %% 360
        
        # Opposite edge for the adjacent piece
        opposite_edges <- c("bottom", "left", "top", "right")
        adj_edges <- c("top", "right", "bottom", "left")
        rotated_adj_edges <- adj_edges[((0:3 - adj_rotation / 90) %% 4) + 1]
        names(rotated_adj_edges) <- adj_edges
        
        opposite_edge <- switch(edge,
                                top = "bottom",
                                bottom = "top",
                                left = "right",
                                right = "left")
        
        edge_rotated <- rotated_edges[edge]
        adj_edge_rotated <- rotated_adj_edges[opposite_edge]
        
        # Record the edge match
        edge_matches <- rbind(edge_matches, data.frame(
          piece_id = piece$piece_id,
          edge = edge_rotated,
          adjacent_piece_id = adj_piece_id,
          adjacent_edge = adj_edge_rotated,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(edge_matches)
}

# Get edge matches for both arrangements
edge_matches1 <- get_edge_matches(arrangement1, grid_size)
edge_matches2 <- get_edge_matches(arrangement2, grid_size)

# Combine and remove duplicates
combined_edge_matches <- unique(rbind(edge_matches1, edge_matches2))

# Create edge nodes
edge_nodes <- unique(c(
  paste(combined_edge_matches$piece_id, combined_edge_matches$edge, sep = "_"),
  paste(combined_edge_matches$adjacent_piece_id, combined_edge_matches$adjacent_edge, sep = "_")
))

# Build the graph
g <- graph_from_data_frame(
  d = data.frame(
    from = paste(combined_edge_matches$piece_id, combined_edge_matches$edge, sep = "_"),
    to = paste(combined_edge_matches$adjacent_piece_id, combined_edge_matches$adjacent_edge, sep = "_")
  ),
  vertices = edge_nodes,
  directed = FALSE
)

# Find connected components
comp <- components(g)

# Assign shape IDs
edge_shape_assignments <- data.frame(
  edge_node = names(comp$membership),
  shape_id = comp$membership,
  stringsAsFactors = FALSE
)

# Split edge_node into piece_id and edge
edge_shape_assignments$piece_id <- as.integer(sub("_.*", "", edge_shape_assignments$edge_node))
edge_shape_assignments$edge <- sub(".*_", "", edge_shape_assignments$edge_node)

# Function to draw an edge
draw_edge <- function(edge, shape_id) {
  # Use colors to represent different shapes
  colors <- rainbow(max(edge_shape_assignments$shape_id))
  color <- colors[shape_id]
  
  positions <- list(
    top = list(x = c(0, 1), y = c(1, 1)),
    right = list(x = c(1, 1), y = c(1, 0)),
    bottom = list(x = c(1, 0), y = c(0, 0)),
    left = list(x = c(0, 0), y = c(0, 1))
  )
  
  grid.lines(
    x = positions[[edge]]$x,
    y = positions[[edge]]$y,
    gp = gpar(col = color, lwd = 2)
  )
}

# Function to draw a puzzle piece
draw_puzzle_piece <- function(piece_id, edge_shapes, rotation = 0) {
  pushViewport(viewport(angle = rotation))
  
  # Draw piece background
  grid.rect(gp = gpar(fill = "white", col = "black"))
  
  # Draw edges
  edges <- edge_shapes$edge
  shape_ids <- edge_shapes$shape_id
  
  for (i in seq_along(edges)) {
    draw_edge(edges[i], shape_ids[i])
  }
  
  # Add piece ID
  grid.text(label = paste("Piece", piece_id), x = 0.5, y = 0.5)
  
  popViewport()
}

# Create a directory to save piece images
dir.create("pieces", showWarnings = FALSE)

# Draw and save each piece
for (piece_id in pieces$piece_id) {
  edge_shapes <- edge_shape_assignments[edge_shape_assignments$piece_id == piece_id, ]
  
  # Save each piece as a PNG image
  png(filename = sprintf("pieces/piece_%02d.png", piece_id), width = 200, height = 200)
  grid.newpage()
  draw_puzzle_piece(piece_id, edge_shapes)
  dev.off()
}

# Function to assemble and display the puzzle
assemble_puzzle <- function(arrangement, grid_size, title = "Puzzle") {
  grid.newpage()
  pushViewport(viewport())
  grid.text(title, x = 0.5, y = 1, just = "top", gp = gpar(fontsize = 16))
  
  for (i in 1:nrow(arrangement)) {
    piece <- arrangement[i, ]
    position <- piece$position
    rc <- get_row_col(position, grid_size)
    rotation <- piece$rotation
    
    # Calculate the viewport for the piece
    vp <- viewport(
      x = (rc$col - 0.5) / grid_size,
      y = (grid_size - rc$row + 0.5) / grid_size,
      width = 1 / grid_size,
      height = 1 / grid_size,
      just = c("center", "center")
    )
    
    pushViewport(vp)
    
    # Draw the piece
    piece_id <- piece$piece_id
    edge_shapes <- edge_shape_assignments[edge_shape_assignments$piece_id == piece_id, ]
    draw_puzzle_piece(piece_id, edge_shapes, rotation)
    
    popViewport()
  }
  
  popViewport()
}

# Assemble Arrangement 1
assemble_puzzle(arrangement1, grid_size, title = "Arrangement 1")

# Assemble Arrangement 2
assemble_puzzle(arrangement2, grid_size, title = "Arrangement 2")
