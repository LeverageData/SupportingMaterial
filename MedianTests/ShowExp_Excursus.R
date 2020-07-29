library(tidyverse)
library(cowplot)

n <- 10^3
x <- seq(0,8,length.out=n)
my_lambda <- 0.693



colors<- c(rgb(0,165,220,maxColorValue = 255), rgb(250,177,5, maxColorValue = 255), rgb(240,125,23,maxColorValue = 255)) 
graphic_tbl <- bind_rows(tibble(Name="Exponentialverteilung", Color=colors[1], Linetype=1, Size=1, Display="Exponentialverteilung", Type="Dichte"), 
                         tibble(Name=c("Median","Mittelwert"), Color=colors[2:3], Linetype=c(2,3), Size=c(1,1), Display=c("Median","Mittelwert"), Type="Information"))

y <- dexp(x, rate=my_lambda)
my_mean <- 1/my_lambda #mean(rexp(100000, rate=my_lambda))
my_median <- 1 # median(rexp(100000, rate=my_lambda))
df <- bind_cols(x=x,y=y,Type="Exponentialverteilung")

yend <- max(y)

main <- ggplot(df, aes(x=x,y=y,color=Type,linetype=Type, size=Type)) +
  geom_area(fill=colors[1], alpha=0.2) +
  ggtitle("Dichte der Exponentialverteilung, lambda=0.693") +
  geom_segment(aes(x=my_mean,xend=my_mean,y=0,yend=yend, color="Mittelwert", size="Mittelwert",linetype="Mittelwert")) +
  annotate(geom="text", x=my_mean, y=yend+0.025, label=sprintf("%.2f",my_mean),color=graphic_tbl[graphic_tbl$Name=="Mittelwert","Color"], fontface=2) +
  geom_segment(aes(x=my_median,xend=my_median,y=0,yend=yend+0.025, color="Median", size="Median",linetype="Median")) +
  annotate(geom="text", x=my_median, y=yend+0.05, label=sprintf("%.2f",my_median),color=graphic_tbl[graphic_tbl$Name=="Median","Color"], fontface=2) +
  scale_color_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Color, labels=graphic_tbl$Display, name="Information") +
  scale_size_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Size, labels=graphic_tbl$Display, name="Information") +
  scale_linetype_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Linetype, labels=graphic_tbl$Display, name="Information") +
  scale_x_continuous(breaks=seq(0,8,by=1)) +
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
