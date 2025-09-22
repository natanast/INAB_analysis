

# load libraries----------
library(readxl)
library(circlize)
library(data.table)



# import data -----------------
df <- read_excel(
  "Light Chains analysis10072023.xlsx", 
  sheet = "IG V-J Combination lamda"
)

# select columns -------------
new_df = subset(
  df, select = c(`V-GENE/J GENE`, IGLJ1, IGLJ2, IGLJ3)
)


# remove rows----------
new_df <- new_df[-c(30, 31), ]


new_df = as.data.frame(new_df)


# rename rows----------
row.names(new_df) = new_df$`V-GENE/J GENE`


# remove column--------
new_df$`V-GENE/J GENE` = NULL


new_df = as.matrix(new_df)


# change row with columns----------
new_df <- t(new_df) |>
          as.data.frame()

new_df$IGHL_genes <- row.names(new_df) 
new_df <- new_df[, c(ncol(new_df), 1:(ncol(new_df)-1))]



writexl::write_xlsx(new_df, "chordDiagram.xlsx", col_names = TRUE)




# import data----------
df1 <- read_excel("chordDiagram.xlsx") |>
       as.data.frame()

# rename rows----------
rownames(df1) <- df1$IGHL_genes

# remove column--------
df1$IGHL_genes = NULL




# colours-----------------
grid.col1 = c(IGLJ3 = "#FDAE61",
  IGLJ2 = "#5E4FA2", IGLJ1 ="#66C2A5"
  )

# Define specific colors for the IGJV sectors----------
iglj_colors <- c(IGLJ3 = "#FDAE61",
                 IGLJ2 = "#5E4FA2", IGLJ1 ="#66C2A5")

# Extract sector names from the data frame that start with "IGJV"--------------
iglj_sectors <- colnames(new_df)[startsWith(colnames(new_df), "IGLJ")]

# Exclude sectors that are already in grid.col1------------------
iglj_sectors <- setdiff(iglj_sectors, names(grid.col1))

# Assign specific colors to the IGJV sectors------------------
for (i in seq_along(iglj_sectors)) {
  grid.col1[iglj_sectors[i]] <- iglj_colors[i]
}




# Extract entity names from the data frame that start with "IGLV"-------------
iglv_entities <- colnames(new_df)[startsWith(colnames(new_df), "IGLV")]

# Exclude entities that are already in grid.col1--------------
iglv_entities <- setdiff(iglv_entities, names(grid.col1))

# Assign them the color "grey"-------------------------
for (entity in iglv_entities) {
  grid.col1[entity] <- "grey10"
}




# vertical symmetric---------
circos.par(start.degree = -90)


 
# empty track------------
chordDiagram(new_df, grid.col = grid.col1, annotationTrack = c("grid", "names"),
             annotationTrackHeight = c(0.01, 0.001),
             preAllocateTracks = list(track.height = 0.2))
            

# customize labels-----------
circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
        facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
        cex = 0.5)
}, bg.border = NA) 







