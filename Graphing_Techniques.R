---
title: "Exercise Sheet 6 Solution"
fontsize: 11pt
header-includes: \usepackage[german]{babel}
output:
  html_document: default
  pdf_document:
    highlight: tango
fig_caption: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, # -> Sollen Code Chunks im gerenderten Dokument angezeigt werden?
                      eval = TRUE, # -> Sollen R Code Chunks ausgeführt werden?
                      warning = FALSE, # -> Warnungen sollten nur am Ende zum Rendern auf FALSE gesetzt werden
                      message = FALSE) # -> Hinweise sollten nur am Ende zum Rendern auf FALSE gesetzt werden
```

```{r}
# Set up libraries (make sure they are installed, first)
library(tidyverse)
library(biclust)
library(reshape)
library(ggplot2) 
```

1. The built-in data set "trees" contains measurements of the girth, height and volume of timber in 31 felled black cherry trees. Use the function `data` to load the data set. Then, create a scatterplot matrix to determine visually between which of the three variables the strongest correlation seems to occur. To investigate whether your assumption was correct, plot the pairwise Pearson correlation coefficient as a number for each pair of variables in the upper diagonal of the scatterplot matrix.  
To plot the matrix you may refer to the function `pairs`.

```{r}
# Solution of task 1...
data(trees)

draw_corr <- function(x, y) {
  # As a backup, store the current value of par("usr")
  usr <- par("usr")
  # Optional example print to see what the original values look like
  print(usr)
  # Ensure that the original value is re-established even if the function results in an error (https://adv-r.hadley.nz/functions.html?q=on.exit#on-exit)
  on.exit(par(usr), add = TRUE)
  # Define the limits of the user coordinates of the plotting region in the form c(x1, x2, y1, y2)
  par(usr = c(0, 1, 0, 1))
  # Determine correlation coefficient
  corr_coeff <- round(cor(x, y), digits=2)
  # Define text to be displayed
  txt <- paste0("R = ", corr_coeff)
  # The absolute of the correlation coefficient
  abs_corr_coeff <- abs(corr_coeff)
  # Vary the text size between 0.7 * the usual size for correlation 0 and 1.7 * the usual size for correlation 1
  text_size = 0.7 + abs_corr_coeff
  # Place text, depending on the coordinates defined in the beginning
  # If text_size = 1, the text is displayed in the usual size
  text(0.5, 0.5, txt, cex = text_size)
}

# Plot SPM
pairs(trees,
      upper.panel = draw_corr)

```

2. Below you can find a data table that shows which students are playing which video games in their free time. Determine whether there are clusters of students playing the same games by using Biclustering (you may refer to the library `biclust`). Display two Heatmaps to inspect the results: The first heatmap should display the original data, i.e., which students play which games. The second heatmap should display which table cell belongs to which bicluster by giving the cells corresponding fill colors. Which visual problem occurs in the second heatmap?  
Note: If you use `biclust`, you can extract the associations of the games and student names to the different clusters from the result object via `result_obj@NumberxCol` and `result_obj@RowxNumber`.

```{r}
# Create bipartite graph data
game_matrix <- matrix(c(1, 0, 0, 1, 1, 1, 0,
                        0, 1, 1, 0, 1, 0, 0,
                        0, 0, 0, 0, 1, 0, 1,
                        1, 1, 0, 0, 0, 0, 0,
                        0, 0, 1, 0, 0, 0, 1,
                        0, 1, 0, 1, 1, 0, 0,
                        0, 0, 0, 0, 0, 1, 0),
                        nrow = 7,
                        ncol = 7,
                        byrow = TRUE)
colnames(game_matrix) <- c("The Witcher 3","Terraria","Hollow Knight","Cities: Skylines","GTA V","Divinity","XCOM 2")
rownames(game_matrix) <- c("Tarek", "Sam", "Ebele", "Kim", "Ali", "Aiko", "Parvati")
```

```{r}
# Solution of task 2...

# Calculate biclustering results
res <- biclust(game_matrix, method=BCBimax())

# Reshape the game matrix to display it with ggplot
game_tibble_melt <- melt(game_matrix) %>% 
  as_tibble() %>% 
  dplyr::rename(students = X1) %>% 
  dplyr::rename(games = X2)

# Number of biclusters found
nr_clusters <- res@Number

# Append a column to store the cluster info in 
game_tibble_melt$cluster <- 0

# Iterate over the biclusters
for(i in 1:nr_clusters) {
  games_in_cluster <- colnames(game_matrix)[res@NumberxCol[i,]]
  students_in_cluster <- rownames(game_matrix)[res@RowxNumber[,i]]
  
  # Enter the cluster number for the filtered subset into the corresponding column
  game_tibble_melt[game_tibble_melt$games %in% games_in_cluster & 
                   game_tibble_melt$students %in% students_in_cluster, ]$cluster <- i
}

# Turn cluster column into factors  for appropriate color scale
game_tibble_melt <- game_tibble_melt %>% mutate(cluster = factor(cluster))

# Plot who plays which games
ggplot(game_tibble_melt, aes(games, students)) +
  geom_tile(aes(fill = value))

#Plot the biclusters
ggplot(game_tibble_melt, aes(games, students)) +
  geom_tile(aes(fill = cluster))

```

3. For the 2D point data set given below, determine visually how many modes, i.e., peaks, the distribution of the points has. To do so, draw a standard scatterplot as well as a Hexbin scatterplot. Which one allows to answer the question better? How does changing the visual parameters help you in answering the question?  
For plotting you may refer to the package `hexbin`.
```{r}
# Define data
# Set seed
set.seed(48)
get_rand <- function(d) {
  return(rnorm(1, mean = d, sd = 1))
}
m <- round(runif(50000, 0, 1)) * 2 + 1
point_data = tibble(x = sapply(m, get_rand), y = sapply(m, get_rand))
```

```{r}
# Solution of task 3...

# Default plot
ggplot(point_data, aes(x, y)) +
    geom_point(pch = 21)

# Plot with alpha adjustment
ggplot(point_data, aes(x, y)) +
    geom_point(pch = 21, alpha = 0.03)

# Plot with 10 bins
ggplot(point_data, aes(x, y)) + 
    # aes() here to ensure that also the stroke of the hexagons is filled to avoid white artifacts
    geom_hex(aes(colour = after_stat(count)), bins = 10) + # after_stat(count) to refer to the aggregated count values derived from an underlying binning
    guides(color = "none") # To hide the (redundant) legend for the outline

# Plot with 30 bins
ggplot(point_data, aes(x, y)) + 
    geom_hex(aes(colour = after_stat(count)), bins = 30) +
    guides(color = "none")

# Plot with 100 bins
ggplot(point_data, aes(x, y)) + 
    geom_hex(aes(colour = after_stat(count)), bins = 100) +
    guides(color = "none")
    

```


------