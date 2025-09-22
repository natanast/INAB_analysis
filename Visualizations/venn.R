


rm(list = ls())
gc()


# library -------

library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggforce)
library(extrafont)


# data cleaning  --------

df = "TNBC.xlsx" |> readxl::read_excel(sheet = 1) |> setDT()



mean_tumor <- mean(df$Tumor_clonotypes) |> round()
mean_germline <- mean(df$Germline_clonotypes) |> round()
mean_public <- mean(df$Public_clonotypes) |> round()


clonotype_data <- list(
    Tumor = mean_tumor,      
    Germline = mean_germline, 
    Public = mean_public     
)



# venn -----------

circle_data <- data.frame(
  x = c(1, 3),       
  y = c(1, 1),       
  r = c(0.8, 1.35)    
)


dataset_sizes <- data.frame(
  set = c("Tumor", "Germline", "Public"),
  size = c(365, 9881, 9)
)


gr = ggplot() +
    
    geom_circle(data = circle_data, aes(x0 = x, y0 = y, r = r, fill = factor(x)), alpha = 0.6) +
    
    coord_fixed() + 
    
    scale_fill_manual(values = c("#CD534C", "#7AA6DC"), labels = c("Tumor", "Germline")) +
    
    theme_minimal() +  
    
    # labs(title = "Puplic_clonotypes",
    #      x = "",
    #      y = "") +
    
    
    geom_text(aes(x = 1, y = 1, label = "iTR"), size = 8, color = "black", family = "Calibri") +  
    geom_text(aes(x = 3, y = 1, label = "pTR"), size = 8, color = "black", family = "Calibri") +  
    
    
    geom_segment(aes(x = 1.73, y = 1, xend = 1.73, yend = 2.3), size = 0.75, color = "black", linetype = "solid") +  
    
    
    geom_label(aes(x = 1.73, y = 2.3, label = "Jaccard Index = 0.03"), size = 7.5, color = "black", family = "Calibri") +
    
    theme(
        
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        panel.border = element_rect(colour = "grey35", fill = NA, size = 0.5),
        
        # panel.grid.major = element_line(linewidth = .15, color = "grey96"),
        # panel.grid.minor = element_line(linewidth = .15, color = "grey96"),

        # plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        
        plot.margin = margin(20, 20, 20, 20)
        
    )
    
    
gr

ggsave(plot = gr, filename = "Venn_diagramm.jpeg", width = 10, height = 10, units = "in", dpi = 600)
