library(tidyverse)
library(cowplot)

n <- 10^3
x <- seq(-1,11,length.out=n)
colors<- c(rgb(0,165,220,maxColorValue = 255), rgb(250,177,5, maxColorValue = 255), rgb(240,125,23,maxColorValue = 255)) 
graphic_tbl <- bind_rows(tibble(Name="Gleichverteilung", Color=colors[1], Linetype=1, Size=1, Display="Gleichverteilung", Type="Dichte"), 
                         tibble(Name=c("Median","Mittelwert"), Color=colors[2:3], Linetype=c(2,3), Size=c(1,1), Display=c("Median","Mittelwert"), Type="Information"))

y <- dunif(x, min=0, max=10)
my_mean <- 5
my_median <- 5
df <- bind_cols(x=x,y=y,Type="Gleichverteilung")


main <- ggplot(df, aes(x=x,y=y,color=Type,linetype=Type, size=Type)) +
  geom_area(fill=colors[1], alpha=0.2) +
  ggtitle("Dichte der Gleichverteilung, min=0, max=10") +
  geom_segment(aes(x=my_mean,xend=my_mean,y=0,yend=0.1, color="Mittelwert", size="Mittelwert",linetype="Mittelwert")) +
  annotate(geom="text", x=my_mean, y=0.1025, label=sprintf("%.2f",my_mean),color=graphic_tbl[graphic_tbl$Name=="Mittelwert","Color"], fontface=2) +
  geom_segment(aes(x=my_median,xend=my_median,y=0,yend=0.1, color="Median", size="Median",linetype="Median")) +
  annotate(geom="text", x=my_median, y=0.105, label=sprintf("%.2f",my_median),color=graphic_tbl[graphic_tbl$Name=="Median","Color"], fontface=2) +
  scale_color_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Color, labels=graphic_tbl$Display, name="Information") +
  scale_size_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Size, labels=graphic_tbl$Display, name="Information") +
  scale_linetype_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Linetype, labels=graphic_tbl$Display, name="Information") +
  scale_x_continuous(breaks=seq(0,10,by=0.5)) +
  theme(legend.position = "none",text=element_text(size=21)) +
  xlab("x") +
  ylab("Dichte")

dichte_legend_gg <- df %>%
  ggplot(aes(x=x,y=y,color=Type,linetype=Type, size=Type)) +
  geom_area(fill=colors[1], alpha=0.2) +
  scale_color_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Color, labels=graphic_tbl$Display, name="Dichte") +
  scale_size_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Size, labels=graphic_tbl$Display, name="Dichte") +
  scale_linetype_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Linetype, labels=graphic_tbl$Display, name="Dichte") +
  theme(legend.justification="right",legend.direction = "vertical", legend.box.just = "left",legend.box.margin = margin(-46, -20, 0, 0),text=element_text(size=21))
dichte_legend <- get_legend(dichte_legend_gg)

info_legend_gg <- df %>%
  ggplot(aes(x=x,y=y,color=Type,linetype=Type, size=Type)) +
  geom_segment(aes(x=my_median,xend=my_median,y=0,yend=1, color="Median", size="Median",linetype="Median")) +
  geom_segment(aes(x=my_mean,xend=my_mean,y=0,yend=1, color="Mittelwert", size="Mittelwert",linetype="Mittelwert")) +
  scale_color_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Color, labels=graphic_tbl$Display, name="Lagemaße") +
  scale_size_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Size, labels=graphic_tbl$Display, name="Lagemaße") +
  scale_linetype_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Linetype, labels=graphic_tbl$Display, name="Lagemaße") +
  theme(legend.justification="left",legend.direction = "vertical", legend.box.just = "right",legend.box.margin = margin(-30, 0, 0, 20), legend.key.width=unit(1.5,"cm"),text=element_text(size=21))
info_legend <- get_legend(info_legend_gg)

full_plot <- plot_grid(main,
                       plot_grid(plotlist=list(dichte_legend,info_legend), nrow=1, axis="t"),
                       nrow=2,
                       rel_heights = c(10,2))
full_plot
