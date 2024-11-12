clrs <- c("#4575b4", "#41ab5d", "#ffbc42")
names(clrs) <- c("lasR", "lidR", "lastools")

# Define custom palettes for when there are 1-2, 3, or 4-6 levels
opts <- options(
  ggplot2.discrete.colour = list(
    c("#4575b4", "#66bd63"),
    RColorBrewer::brewer.pal(3, "Set2"),
    c("#4053d3", "#ddb310", "#b51d14", "#00beff", "#fb49b0"),
    RColorBrewer::brewer.pal(6, "Accent")
  )
)
opts <- options(
  ggplot2.discrete.fill = list(
    c("#4575b4", "#66bd63"),
    RColorBrewer::brewer.pal(3, "Set2"),
    c("#4053d3", "#ddb310", "#b51d14", "#00beff", "#fb49b0"),
    RColorBrewer::brewer.pal(6, "Accent")
  )
)

# col_pal <- RColorBrewer::brewer.pal(3, "Set2")
# col_pal <- c("#4053d3", "#ddb310", "#b51d14", "#00beff", "#fb49b0")
# plot(1:length(col_pal), 1:length(col_pal), col = col_pal, cex = 5, pch=19)

opts

thefont <- "Segoe UI"

theme_clean <- theme_light(base_family = "Segoe UI") %+replace% 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = rel(1)),
        panel.grid = element_blank(), 
        axis.ticks = element_line(colour = "black", size = rel(0.5)),
        legend.position = "bottom", strip.text = element_text(colour = "black"),
        strip.background = element_rect(fill = NA, colour = NA))

ggplot2::theme_set(theme_clean)


custom_number_format <- function(x){
  ifelse(x > 999999999,
         paste0(format(round((x/1000000000), 2), 
                       nsmall=1, big.mark=","), "B"),
         ifelse(x > 999999, 
                paste0(format(round((x/1000000), 1), 
                              nsmall=1, big.mark=","),"M"), 
                format(round(x), nsmall=0, big.mark=",")))
}