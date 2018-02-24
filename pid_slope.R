g72 <- gss %>% 
  group_by(reltrad, year) %>% 
  summarise(mean = mean(partyid, na.rm = TRUE)) %>% 
  filter(year ==1972) 



g16 <- gss %>% 
  group_by(reltrad, year) %>% 
  summarise(mean = mean(partyid, na.rm = TRUE)) %>%
  filter(year ==2016)

com <- bind_cols(g72, g16) %>% na.omit()

com <- com %>% filter(reltrad != "Other Faith")


com$reltrad <- Recode(com$reltrad, "1='Evangelical Protestants';
                       2='Mainline Protestants';
                      3='Black Protestants';
                      4='Catholic'; 
                      5='Jewish';
                      6= 'Other Faith';
                      7= 'No Faith'", as.factor = TRUE)

com <- as.tibble(com)
com <- com %>% ungroup(year)
com <- add_row(com, reltrad = "Entire Sample", year = 1972, mean = 2.51, reltrad1 = "Entire Sample", year1 = 2016, mean1 = 2.76, .before = 1)


com$class <- ifelse((com$mean1 - com$mean) < 0, "blue", "red")


com <- com %>% 
  mutate(mean = round(mean, 2)) %>% 
  mutate(mean1 = round(mean1, 2)) 



com <- com %>% filter(reltrad != "Other Faith")

left_label <- paste(com$reltrad, com$mean, sep=" - ")
right_label <- paste(com$reltrad, com$mean1 ,sep=" - ")

windowsFonts(Times=windowsFont("Product Sans"))

p <- ggplot(com) + geom_segment(aes(x=1, xend=2, y=mean, yend=mean1, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("blue"="dodgerblue3", "red"="#f8766d")) +  # color of lines
  labs(x="", y="Party Identification") +  # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(com$mean, com$mean1)))) + 
  scale_y_continuous(limits = c(1,3.9), breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Ind., Near Dem.", "Independent", "Not Strong Republican", "Moderate Republican", "Strong Republican")) +
  theme(text=element_text(size=32, family="Product Sans"))  # X and Y axis limits

p <- p + geom_text(label=left_label, y=com$mean, x=rep(1, NROW(com)), hjust=1.1, size=10.5, family= "Product Sans")
p <- p + geom_text(label=right_label, y=com$mean1, x=rep(2, NROW(com)), hjust=-0.1, size=10.5, family= "Product Sans")


p <- p + geom_text(label="1972", x=1, y= 1.1*(max(com$mean, com$mean1)), size=16, family= "Product Sans")  # title
p <- p + geom_text(label="2016", x=2, y= 1.1*(max(com$mean, com$mean1)), size=16, family= "Product Sans")  # title




p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm")) +
  labs(title = "Overall Shifts in Party Identification (1972-2016)", subtitle = "0 = Strong Democrat, 3 = Independent, 6 = Strong Republican", caption = "Data: GSS (1972-2016)") + 
  theme(plot.title = element_text(size=72)) 


ggsave(file="slope_pid.png", type = "cairo-png", width = 30, height = 21)

