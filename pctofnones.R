## This is just making the big demo chart

t1 <- tibble("group" =c("No Religion", "Protestants", "Catholics", "Everyone Else"), count = c(16162, 24835 , 11433, 2912 ))
t1$group <- as_factor(t1$group)

t1 <- t1 %>% mutate(pct = count/sum(count)) 

t1$group <- fct_relevel(t1$group, "Protestants" , "No Religion" , "Catholics", "Everyone Else")

t1$label <- c("Distribution")

p1 <- ggplot(t1, aes(x=label, y=pct*100, fill = forcats::fct_rev(group))) + geom_col(color = "black") + coord_flip() +
  theme(legend.position = "bottom") + guides(fill = guide_legend(reverse=TRUE)) +
  scale_x_discrete(labels = c("")) + xlab("") + 
  flip_bar_rb() +
  ylab("Percent of Sample") + theme(legend.title=element_blank())  +
  ggtitle("Major Religious Traditions in the U.S.") 

ggsave(file="D://cces_panel/none_dist.png", type = "cairo-png", width = 10, height = 3)

## This is just making the big demo chart

t2 <- tibble("group" =c("Nothing in Particular", "Atheist", "Agnostic"), count = c(10162, 3009 , 2991))
t2$group <- as_factor(t2$group)

t2 <- t2 %>% mutate(pct = count/sum(count)) 

# t2$group <- fct_relevel(t2$group, "Nothing in Particular" , "Atheist" , "Agnostic")
# t2$group <- factor(t2$group , levels=unique(t2$group ))


t2$label <- c("Distribution")

p2 <- ggplot(t2, aes(x=reorder(label, -pct), y=pct*100, fill = factor(group, levels =c("Nothing in Particular", "Agnostic", "Atheist")))) + geom_col(color = "black") + coord_flip() +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_x_discrete(labels = c("")) + xlab("") + 
  ylab("Percent of Nones")  +
  ggtitle("Who are the 'No Religion'?") +
  labs(caption = "Data from CCES 2016") +
  flip_bar_rb()

ggsave(file="D://cces_panel/none_dist_2.png", type = "cairo-png", width = 10, height = 3)

p1 + p2 + plot_layout(ncol = 1)

ggsave(file="D://cces_panel/both_nones.png", type = "cairo-png", width = 10, height = 6)