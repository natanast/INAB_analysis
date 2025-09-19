

rm(list = ls())
gc()


# load libraries ----

library(data.table)
library(stringr)


# clustering ----

fls = "GSEA/[archived]/Per_entity_treatments/" |> 
    list.files(full.names = TRUE, pattern = "gsea_genekitr_") |>
    str_subset("xlsx")



for(i in seq_len(length(fls))){
    
    comparison = fls[i] |> str_sub(39, -1) |> str_sub(1, -6)
    
    df = fls[i] |> readxl::read_xlsx() |> setDT()
    
    df <- df[Count > 30 & abs(NES) > 1.5 & abs(enrichmentScore) > 0.4]
    
    df1 <- df[, .(Description, geneID)]
    
    gene_list <- df1$geneID |> str_split("/") |> unlist()
    
    
    # long format
    df_long <- df1[
        , .(Gene = geneID |> str_split("/", simplify = FALSE) |> unlist()),
        by = Description
    ]
    
    df_long[, Gene := str_trim(Gene)]
    df_long <- unique(df_long)
    
    
    # Wide format
    df_wide <- dcast(
        df_long,
        Description ~ Gene,
        fun.aggregate = length,   
        value.var = "Gene",
        fill = 0
    )
    
    
    mat <- as.matrix(df_wide[, -1, with = FALSE])     
    rownames(mat) <- df_wide$Description            
    
    
    
    d  <- dist(mat, method = "binary")    
    hc <- hclust(d, method = "ward.D2")   
    # plot(hc, cex = 0.7, hang = -1)  # dendrogram

    
    # Clusters ------
    
    # Cut into k clusters
    clusters <- cutree(hc, k = 150)
    
    # Put into a nice data.table
    cluster_dt <- data.table(
        Description = names(clusters),
        Cluster     = as.integer(clusters)
    )

    # Merge clusters into your df
    df <- merge(df, cluster_dt, by = "Description", all.x = TRUE)

    # final df --------- 
    
    ord_desc <- hc$labels[hc$order] 

    df[, Description := factor(Description, levels = ord_desc)]
    setorder(df, Description)
    
    writexl::write_xlsx(df, paste0("gsea_genekitr_", comparison, "_clusters.xlsx"))
    
}

