library(tidyverse)
library(cowplot)
library(restorepoint)

####
# Variables
####

#Main variables
use_distr <- "unif" #supported: gamma, exp, unif, norm, valley
tests <- c("t.test","wilcox","sign")

#Graphical Variables
displaynames_tests <- c("t-Test","Wilcoxon-Vorzeichen-Rang-Test","Vorzeichentest")
color_tests <- c(rgb(0,165,220,maxColorValue = 255), rgb(0,75,120,maxColorValue = 255), rgb(100,100,100,maxColorValue = 255)) 
linetypes_tests <- c(1,1,1)
sizes_tests <- c(1.25,1,0.75)
add_info_tests <- rep(NA, length(displaynames_tests))

alpha <- 0.05
information <- c("uniform_p","alpha")
displaynames_information <- c("Gleichverteilung p-Werte",paste0("Irrtumswahrscheinlichkeit ",round(alpha*100, digits=2),"%"))
add_info_information <- c(NA, alpha)
color_information <- c(rgb(0,0,0, maxColorValue = 255), rgb(250,177,5, maxColorValue = 255))
sizes_information <- c(0.5,1)
linetypes_information <- c(3,2)

name_tests <- "Test"
name_information <- "Information"

graphic_tbl <- bind_rows(tibble(Name=tests, Color=color_tests, Linetype=linetypes_tests, Size=sizes_tests, Display=displaynames_tests, Type=name_tests, Add_Info=add_info_tests), tibble(Name=information, Color=color_information, Linetype=linetypes_information, Size=sizes_information, Display=displaynames_information, Type=name_information, Add_Info=add_info_information))

#General
n <- 100 #Stichprobengröße
repeats <- 10000 #Anzahl der Stichproben

####
# Functions
####

#Special Valley distribution function
rvalley <- function(n, rate){
  separation <- runif(n)
  n_left <- sum(separation<0.5)
  n_right <- n-n_left
  if(n_left>0){
    exp_left <- rexp(n_left,rate)
    to_repeat <- exp_left>1
    while(any(to_repeat)){
      exp_left[to_repeat] <- rexp(sum(to_repeat),rate)
      to_repeat <- exp_left>1
    }
  } 
  if(n_right>0){
    exp_right <- 1-rexp(n_right,rate)
    to_repeat <- exp_right<0
    while(any(to_repeat)){
      exp_right[to_repeat] <- 1-rexp(sum(to_repeat),rate)
      to_repeat <- exp_right<0
    }
  }
  res <- rep(NA,n)
  if(any(separation<0.5)) res[separation<0.5] <- exp_left
  if(any(separation>=0.5)) res[separation>=0.5] <- exp_right
  return(res)
}

# Distribution Variables encapsulation function
GetDefaultParams <- function(distribution){
  
  switch(distribution,
         gamma={
           mu0_1 <-10
           res <- list(distr_name=distribution,
                       distr_params=list(shape=10.332,scale=1),
                       mu_0=c(mu0_1, mu0_1+0.15, mu0_1+0.332, mu0_1+0.5),
                       display_name="Gammaverteilung",
                       display_params=c("k","theta")
                       )
         },
         exp={
           mu0_1 <- 1
           res <- list(distr_name=distribution,
                       distr_params=list(rate=0.693),
                       mu_0=c(mu0_1, mu0_1+0.221, mu0_1+0.443, mu0_1+0.664),
                       display_name="Exponentialverteilung",
                       display_params=c("lambda"))
         },
         unif={
           mu0_1 <- 0.5
           res <- list(distr_name=distribution,
                       distr_params=list(min = 0, max = 1),
                       mu_0=c(mu0_1, mu0_1+0.025, mu0_1+0.05, mu0_1+0.075),
                       display_name="Gleichverteilung",
                       display_params=c("min","max"))
         },
         norm={
           mu0_1 <- 0
           res <- list(distr_name=distribution,
                       distr_params=list(mean = 0, sd = 1),
                       mu_0=c(mu0_1, mu0_1+0.05, mu0_1+0.1, mu0_1+0.25),
                       display_name="Normalverteilung",
                       display_params=c("Mittelwert","Standardabweichung"))
         },
         valley={
           mu0_1 <- 0.5
           res <- list(distr_name=distribution,
                       distr_params=list(rate=10),
                       mu_0=c(mu0_1, mu0_1+0.05, mu0_1+0.1, mu0_1+0.25),
                       display_name="Talverteilung",
                       display_params=c("lambda"))
         },
         {
           res<-list()
         } 
         )
  res$n_m0 <- length(res$mu_0)
  return(res)
}

#Generates a Sample based on Distribution Parameters
GenSample <- function(n, distr_specific){
  restore.point("GenSample")
  name_distr_fun <- paste0("r",distr_specific$distr_name)
  if(!existsFunction(name_distr_fun)){
    stop("Distribution not found!")
  }
  args <- rep(NA,length(distr_specific$distr_params))
  distr_arg_names <- names(distr_specific$distr_params)
  for(i in 1:length(distr_specific$distr_params)){
    args[i] <- paste0(distr_arg_names[i],"=",distr_specific$distr_params[[i]])
  }
  fun_call <- paste0(name_distr_fun,"(n=",n,",",paste(args,collapse=","),")")
  
  res <- eval(parse(text=fun_call))
  return(res)
}

#Provides histogram and mean of medians
CheckEmpiricalMedian <- function(n, repeats, distr_specific){
  medians <- rep(NA, repeats)
  for(i in 1:repeats){
    medians[i] <- median(GenSample(n=n, distr_specific=distr_specific))
  }
  hist(medians)
  
  return(mean(medians))
}

GenerateTitle <- function(title, distr_specific, n=NULL, add_title=NULL){
  if(!is.null(title)){
    return(title)
  } else {
    args <- rep(NA,length(distr_specific$distr_params))
    distr_arg_names <- distr_specific$display_params
    for(i in 1:length(distr_specific$distr_params)){
      args[i] <- paste0(distr_arg_names[i],"=",distr_specific$distr_params[[i]])
    }
    base_wo_n <- paste0(distr_specific$display_name,", ",paste0(args,collapse=", "))
    if(!is.null(n)){
      res <- paste0(base_wo_n, ", n=",n)
    } else {
      res <- base_wo_n
    }
    if(!is.null(add_title)){
      res <- paste0(res,add_title)
    }
    return(res)
  }
}

DrawSampleDistribution <- function(n, distr_specific, title=NULL, color="black", alpha=0.2, xlab="x",ylab="Dichte", adjust_bandwidth=0.1){
  sample <- GenSample(n=n, distr_specific=distr_specific)
  ggplot(as_tibble(sample), aes(x=value)) +
     geom_density(size=1, color=color, fill=color, alpha=alpha, adjust=adjust_bandwidth) +
     ggtitle(GenerateTitle(title, distr_specific)) +
     xlab(xlab) +
     ylab(ylab)
}

# Generate Test Dataset. Currently supported: wilcox, sign and t.test
GenerateTestDataset <- function(n, repeats, distr_specific, tests, seed){
  df <- data.frame(Mu_0=rep(distr_specific$mu_0, repeats), Set=rep(1:repeats, each=distr_specific$n_m0))
  
  for(i in 1:length(tests)){
    df[,tests[i]] <- rep(NA, repeats*distr_specific$n_m0)
  }
  
  set.seed(seed)
  set <- 0
  for(i in 1:nrow(df)){
    if(df[i,"Set"]!=set){
      gen_vec <- GenSample(n=n,distr_specific = distr_specific)
      set <- set+1
    }
    if("wilcox" %in% tests){
      df[i,"wilcox"] <- wilcox.test(gen_vec, mu=df[i,"Mu_0"])$p.value      
    }
    if("sign" %in% tests){
      df[i,"sign"] <- DescTools::SignTest(gen_vec, mu=df[i,"Mu_0"])$p.value     
    }
    if("t.test" %in% tests){
      df[i,"t.test"] <- t.test(gen_vec, mu=df[i,"Mu_0"])$p.value     
    }
  }
  df_long <- pivot_longer(df, all_of(tests), names_to = "Test", values_to = "P_Value")
  
  df_long_adapted <- df_long %>%
    mutate(Test = fct_relevel(Test,tests))
  
  return(df_long_adapted)
}

ThinOutDataFrame <- function(df, group_vars, order_var=NULL, thinning_factor=10){
  res <- df %>%
    group_by_at(group_vars)
  
  if(!is.null(order_var)){
    res <- res %>%
      arrange_at(order_var)
  }
  
  res <- res %>%
    slice(seq(1,n(),by=thinning_factor)) %>%
    ungroup()
  
  return(res)
}

ShowGraphTestComparison <- function(df, graphic_tbl, n=NULL, distr_specific, test_group=name_tests, info_group=name_information, xlab="p-Wert", ylab="empirische Verteilungsfunktion", plot_grid_val=c(10,2), add_title=NULL){
  restore.point("ShowGraphTestComparison")
  
  info_rel <- FALSE
  
  # Filter based on graph_tbl
  exist_test <- graphic_tbl %>%
    filter(Type=="Test") %>%
    dplyr::select(Name) %>%
    unique() %>%
    unlist()
  
  df <- df %>%
    filter(Test %in% exist_test)
  
  gg <- ggplot(df, aes(x=P_Value, color=Test, size=Test, linetype=Test)) +
    stat_ecdf(pad=FALSE)
  if("uniform_p" %in% graphic_tbl$Name){
    gg <- gg +
      geom_segment(aes(x=0,xend=1,y=0,yend=1, color="uniform_p", size="uniform_p",linetype="uniform_p"))
    info_rel <- TRUE
  }
  if("alpha" %in% graphic_tbl$Name){
    gg <- gg +
      geom_segment(aes(x=unlist(graphic_tbl[graphic_tbl$Name %in% "alpha","Add_Info"][1]),xend=unlist(graphic_tbl[graphic_tbl$Name %in% "alpha","Add_Info"][1]),y=0,yend=1,color="alpha", size="alpha", linetype="alpha"))
    info_rel <- TRUE
  }
  if(length(unique(df$Mu_0))>1){
    gg <- gg + facet_grid(.~Mu_0)
  }
  gg <- gg +
    scale_color_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Color, labels=graphic_tbl$Display) +
    scale_size_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Size, labels=graphic_tbl$Display) +
    scale_linetype_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Linetype, labels=graphic_tbl$Display) +
    scale_y_continuous(breaks=seq(0,1,by=0.1)) +
    scale_x_continuous(breaks=seq(0,1,by=0.1)) +
    theme(text=element_text(size=21), legend.position = "bottom", legend.direction = "vertical") +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(GenerateTitle(title=NULL,distr_specific = distr_specific, n=n, add_title=add_title))
  
  if(!info_rel){
    return(gg)
  } else {
    gg <- gg + 
      theme(legend.position="none")
  }
  
  #For alignment purposes
  test_legend_count <- df %>% summarize(unique(Test)) %>% nrow()
  info_legend_count <- graphic_tbl %>% filter(Type=="Information") %>% count(Type) %>% select(n) %>% unlist()
  offset_base <- -16
  offset_margin_smaller <- -16*abs(info_legend_count - test_legend_count)
  if(test_legend_count<info_legend_count){
    offset_margin_test <- offset_margin_smaller + offset_base
    offset_margin_info <- 0 + offset_base
  } else {
    offset_margin_test <- 0 + offset_base
    offset_margin_info <- offset_margin_smaller + offset_base
  }
  
  test_legend_gg <- df %>%
    ggplot(aes(P_Value, color=Test, size=Test, linetype=Test)) +
    stat_ecdf() +
    scale_color_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Color, labels=graphic_tbl$Display) +
    scale_size_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Size, labels=graphic_tbl$Display) +
    scale_linetype_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Linetype, labels=graphic_tbl$Display) +
    theme(legend.justification="right",legend.direction = "vertical", legend.box.just = "top",legend.box.margin = margin(offset_margin_test, -20, 0, 0),text=element_text(size=21))
  test_legend <- get_legend(test_legend_gg)
  

  
  info_legend_gg <- df %>%
    ggplot(aes(P_Value, color=Test, size=Test, linetype=Test))
  
  if("uniform_p" %in% graphic_tbl$Name){
    info_legend_gg <- info_legend_gg +
      geom_segment(aes(x=0,xend=1,y=0,yend=1, color="uniform_p", size="uniform_p",linetype="uniform_p"))
  }
  if("alpha" %in% graphic_tbl$Name){
    info_legend_gg <- info_legend_gg +
      geom_segment(aes(x=unlist(graphic_tbl[graphic_tbl$Name %in% "alpha","Add_Info"][1]),xend=unlist(graphic_tbl[graphic_tbl$Name %in% "alpha","Add_Info"][1]),y=0,yend=1,color="alpha", size="alpha", linetype="alpha"))
    info_rel <- TRUE
  }
  info_legend_gg <- info_legend_gg +
    scale_color_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Color, labels=graphic_tbl$Display, name="Information") +
    scale_size_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Size, labels=graphic_tbl$Display, name="Information") +
    scale_linetype_manual(breaks=graphic_tbl$Name, values=graphic_tbl$Linetype, labels=graphic_tbl$Display, name="Information") +
    theme(legend.justification="left",legend.direction = "vertical", legend.box.just = "bottom",legend.box.margin = margin(offset_margin_info, 0, 0, 20), legend.key.width=unit(1.5,"cm"),text=element_text(size=21))
  info_legend <- get_legend(info_legend_gg)
  
  full_plot <- plot_grid(
    gg
    , plot_grid(plotlist = list(test_legend,info_legend),
                nrow = 1
    )
    , nrow = 2
    , rel_heights = plot_grid_val
  )
  return(full_plot)
}

####
# Skript
####

#Get Parameters
default_params <- GetDefaultParams(use_distr)

#If wanted check median
#CheckEmpiricalMedian(n=n, repeats=repeats, distr_specific=default_params)

### Drawing Sample Distribution if wanted
# DrawSampleDistribution(n=1000000, distr_specific=default_params, color=color_tests[1], alpha=0.2, xlab="x",ylab="Dichte", adjust_bandwidth=0.0001)

# Dataset 
df <- GenerateTestDataset(n=n, repeats=repeats, distr_specific = default_params, tests=tests, seed=42) 

## Displaying the Graph is a lot faster if thinned out
df_thinned <- df %>%
  ThinOutDataFrame(group_vars = c("Test","Mu_0"), order_var = c("P_Value"))

ShowGraphTestComparison(df=df_thinned, graphic_tbl = graphic_tbl, distr_specific = default_params, n=n)

## optional: Show only part
#ShowGraphTestComparison(df=df_thinned, graphic_tbl = graphic_tbl %>% filter(Name %in% c("wilcox","sign", "alpha")), distr_specific = default_params)

#ShowGraphTestComparison(df=df_thinned %>% filter(Mu_0==0.1), graphic_tbl = graphic_tbl %>% filter(Name %in% c("t.test","alpha","uniform_p")), distr_specific = default_params, add_title=", Test auf 0.1 (H0 nicht erfüllt)")

# Schnittpunkte berechnen
df %>%
  group_by(Mu_0,Test) %>%
  arrange(P_Value) %>%
  mutate(P_Value_Dist = cume_dist(P_Value), Smaller_5_Perc = if_else(P_Value<0.05,TRUE,FALSE)) %>%
  filter(Smaller_5_Perc==TRUE) %>%
  summarise(Last_Value = last(P_Value_Dist), Diff_One = 1-Last_Value)
