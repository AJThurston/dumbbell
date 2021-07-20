library(ggplot2)
library(Cairo)
library(scales)
library(openxlsx)
library(forcats)
library(tidyverse)
library(grid)
library(gridExtra)

df <- read.csv("values.csv") #https://news.gallup.com/poll/352316/americans-confidence-major-institutions-dips.aspx
df[,c(2:4)] <- df[,c(2:4)]/100 # format numeric data as decimal
df <- df[order(df$Overall), ] # sort highest to lowest trusted institutions
df$order <- order(df$Overall) # record sort order
df$Institution <- fct_reorder(df$Institution, df$order) # store institution as ordered factor

colnames(df) <- c("cont","overall","x1","x2","order")
df$diff <- abs(df$x2-df$x1)

# Store numbers as percent for labels
df$x1pr <- percent(df$x1, accuracy = 1) # repub = x1
df$x2pr <- percent(df$x2, accuracy = 1) # democ = x2
df$dipr <- percent(df$diff, accuracy = 1)
df$ovpr <- percent(df$overall, accuracy = 1)

# Nudges to offset the value labels from the geom_point
df$x1nu <- ifelse(df$x1 > df$x2, 1, ifelse(df$x1 < df$x2,-1, -1))/16
df$x2nu <- -1*df$x1nu

# Data for highlighting every other row
df2 <- df %>% 
  filter(row_number() %% 2 == 0) %>%
  mutate(xmin = -2) %>%
  mutate(xmax = 2)
df <- left_join(df,df2)
rm(df2)


# ggplot ----
sub.col <- "black"
x1.col <- "#d61b2b"
x2.col <- "#1d3e6e"
txt.sz  <- 3.5

theme_custom <-
  theme(text = element_text(size = 14),
        panel.background = element_blank(),
        axis.text.y = element_text(color = 'black', hjust = 0, size = 14/5*txt.sz),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = rel(.8), hjust = -1),
        plot.margin = unit(c(.3, .1, 0, .25), "cm"),
        plot.caption = element_text(size = rel(.65))
  )

p <- ggplot(df, aes(y = cont))
p <- p + geom_rect(aes(xmin=xmin, xmax=xmax, ymin=as.numeric(cont)-1.5, ymax=as.numeric(cont)-.5), fill="#f6f6f6")
p <- p + geom_text(label = "Institution", x = -.675, y = dim(df)[1]+1, size = txt.sz, color = sub.col, fontface = 2, hjust = 0)
p <- p + geom_text(label = "Democrat/Leans D", x = .15, y = dim(df)[1]+1, size = txt.sz, color = x2.col, fontface = 2)
p <- p + geom_text(label = "Republican/Leans R", x = .65, y = dim(df)[1]+1, size = txt.sz, color = x1.col, fontface = 2)
p <- p + geom_text(label = "Overall", x = 1.075, y = dim(df)[1]+1, size = txt.sz, color = sub.col, fontface = 2)
p <- p + geom_text(label = "\u0394", x = 1.2, y = dim(df)[1]+1, size = txt.sz, color = sub.col, fontface = 2)
p <- p + geom_text(label = "@AJThurston", x = .75, y = 7, size = txt.sz, color = "white")
p <- p + geom_text(aes(x = x1, label = x1pr), size = txt.sz, color = x1.col, nudge_x = df$x1nu)
p <- p + geom_text(aes(x = x2, label = x2pr), size = txt.sz, color = x2.col, nudge_x = df$x2nu)
p <- p + geom_text(aes(x = x1*0+1.075, label = ovpr), size = txt.sz, color = sub.col)
p <- p + geom_text(aes(x = x1*0+1.2, label = dipr), size = txt.sz, color = sub.col)
p <- p + geom_segment(aes(x = x1, xend = x2, yend = cont), color = "gray40", size=1)
p <- p + geom_point(aes(x=x1), shape = 16, size = 2.5, color = x1.col)
p <- p + geom_point(aes(x=x2), shape = 16, size = 2.5, color = x2.col)
p <- p + labs(caption = "Source: https://news.gallup.com/poll/352316/americans-confidence-major-institutions-dips.aspx")
p <- p + scale_y_discrete(expand = c(0, 0))
p <- p + scale_x_continuous()
p <- p + coord_cartesian(ylim = c(0.5, 17.5), xlim = c(0,1.2), clip = 'off')
p <- p + theme_custom
p

# Making and appending a separate title to justify it outside of the original plot area
title <- textGrob(
  label = "Gallup: Confidence in Institutions 2021 (by political affiliation)",
  x = unit(.28, "cm"), 
  y = unit(-.1, "cm"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 12))

p1 <- arrangeGrob(p, top = title)

ggsave(plot = p1,
       "dumbbell.png",
       scale = 1,
       width = 6.5,
       height = 4,
       units = "in",
       type = "cairo",
       dpi = 300)


