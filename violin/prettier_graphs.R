library(ggplot2)
library(readxl)
library(PTCA4CATA)
library(Hmisc)
library(grid)

grp <- c(1,2,3,4) # how many groups
violin_plot <- function(file_name=YOUR_FILE, sheet_name=YOUR_FILE, y_name=COLUMMN_YOU_WANT_TO_PLOT, title =PLOT_TITLE, alpha = 0.5, order =2){
  G <- read_excel(file_name, sheet = sheet_name)
  G_means <- aggregate(x = as.matrix(G[,y_name]), by = G[,"groups"], FUN = mean) # groups is used as index, included in your dataframe
  model <- lm(get(y_name) ~ poly(grp, order, raw = TRUE),  
              data = G_means)
  sum_model <- summary(model)
  r2 <- bquote(R^2 ~ "= "~ .(round(sum_model$r.squared,3)))
  if(order ==2){
    eq <- bquote("y = "~.(round(sum_model$coefficient[3],3)) ~ x^2 ~" + " ~ .(round(sum_model$coefficient[2],3)) ~ "x + "~ .(round(sum_model$coefficient[1],3)) ~ "; " ~R^2 ~ "= "~ .(round(sum_model$r.squared,3)))
  }else{
    eq <- bquote("y = "~ .(round(sum_model$coefficient[2],3)) ~ "x + "~ .(round(sum_model$coefficient[1],3)) ~ "; " ~R^2 ~ "= "~ .(round(sum_model$r.squared,3)))
  }
  # Compute the analysis of variance
  res.aov <- aov(as.matrix(G[, y_name]) ~ groups, data = G)
  # Summary of the analysis
  f_value = summary(res.aov)[[1]][1,4]
  pvals <- summary(res.aov)[[1]][["Pr(>F)"]]
  sigSymbols <- symnum(pvals, na = FALSE, 
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                       symbols = c("***", "**", "*", ".", " "),
                       legend = FALSE)
  png(
    filename = paste(y_name,".png", sep = ""),
    type = "cairo", 
    res = 300, # 300ppi 
    width = 1500, height = 1500,
    bg = "transparent" 
  )
  # Add basic box plot
  p <- ggplot(G, aes_string(x="groups", y=y_name, fill="groups")) +
    #geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=0.5, notch=TRUE, width = 0.3) +
    geom_violin(width=0.5, position = "dodge", trim=TRUE, color = "black", alpha = 0.7) + 
    #geom_dotplot(binaxis='y', stackdir='center', binpositions="all", dotsize = 0.3) + 
    geom_point(pch = 21, position = position_jitterdodge(dodge.width = 0.3), alpha = 0.4, stroke = 0.5, show.legend = FALSE)+
    #geom_boxplot(width = 0.1, color = "black")+
    #geom_jitter(shape=16, position=position_jitter(0.2), alpha = alpha, aes(color = )) + 
    scale_x_discrete(labels = c("SNC", "NC", "aMCI", "AD")) +
    stat_boxplot(geom ='errorbar', width = 0.01) + 
    stat_summary(fun=mean, colour="darkred", geom="point", shape=15, size=2, show.legend=FALSE) +
    #stat_summary(fun.data = mean_sdl, geom = "crossbar", width = 0.05) +
    theme_classic() + 
    scale_fill_manual(labels = c("SNC", "NC", "aMCI", "AD"),values = c("#66CDAA","#4682B4","#AB63FA","#FFA15A")) +
    labs(title=title,x="Groups", y=y_name, subtitle = paste("F-Stat:", round(f_value, 2), sigSymbols), caption = eq )+
    theme(legend.position = "right",
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         plot.caption = element_text(hjust = 0))

  
    #stat_regline_equation(mapping = aes(x = c(0,1,2,3,4), y = get(y_name), label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), data = G_means, formula = y ~ poly(x, order, raw = TRUE), )
    #geom_line(data = G_means, mapping = aes_string(x="groups", y=y_name, group=1))
    #p_line <- p + 
    #geom_point(inherit.aes = FALSE, data = G_means, mapping = aes(x = groups, y = get(y_name))) + 
    #stat_smooth(inherit.aes = FALSE, data = G_means, mapping = aes(x = c(1,2,3,4), y = get(y_name)), method='lm', formula = y~poly(x,order), se = FALSE, colour = "black")
    
  print(p)
  dev.off()
}
violin_plot()


