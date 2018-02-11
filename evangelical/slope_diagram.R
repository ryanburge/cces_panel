cces %>% 
  tabyl(ba_10)

#26.0

cces %>% 
  tabyl(ba_14)

#26.1

cces %>% 
  tabyl(baprot_10)

#22.7

cces %>% 
  tabyl(baprot_14)

##22.5

cces %>% 
  tabyl(nbaprot_10)

## 21.0

cces %>% 
  tabyl(nbaprot_14)

## 19.5%

a1 <- cces %>% 
  tabyl(religpew_10)


a2 <- cces %>% 
  tabyl(religpew_14) %>% 
  filter(religpew_14 != 13)


a <- bind_cols(a1, a2) %>% 
  rename(year10 = percent, year14 = valid_percent, religpew = religpew_10) %>% 
  select(religpew, year10, year14) %>% 
  mutate(religpew = recode(religpew, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'")) %>% 
  filter(religpew != "12") %>% 
  filter(religpew == "Catholic" | religpew == "Jewish" | religpew == "Atheist" | religpew == "Agnostic" | religpew == "Nothing")

a <- as.tibble(a)

a <- add_row(a, religpew = "Born Again Protestant", year10 = .2275, year14 = .2247, .before = 1)
a <- add_row(a, religpew = "Not Born Again Protestant", year10 = .2098, year14 = .1945, .before = 3)

a <- a %>% 
  mutate(pct10 = round(year10*100, 2)) %>% 
  mutate(pct14 = round(year14*100, 2)) %>% 
  select(-year10, -year14)

a$pct10c <- paste(a$pct10, "%", sep = "")
a$pct14c <- paste(a$pct14, "%", sep = "")


left_label <- paste(a$religpew, a$pct10c,sep=", ")
right_label <- paste(a$religpew, a$pct14c,sep=", ")


a$class <- ifelse((a$pct14 - a$pct10) < 0, "red", "green")


windowsFonts(Times=windowsFont("Product Sans"))

p <- ggplot(a) + geom_segment(aes(x=1, xend=2, y=pct10, yend=pct14, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
  labs(x="", y="Percent of the Population") +  # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(a$pct10, a$pct14)))) +
  theme(text=element_text(size=32, family="Product Sans"))  # X and Y axis limits

p <- p + geom_text(label=left_label, y=a$pct10, x=rep(1, NROW(a)), hjust=1.1, size=10.5, family= "Product Sans")
p <- p + geom_text(label=right_label, y=a$pct14, x=rep(2, NROW(a)), hjust=-0.1, size=10.5, family= "Product Sans")


p <- p + geom_text(label="2010", x=1, y=1.1*(max(a$pct10, a$pct14)), hjust=1.2, size=16, family= "Product Sans")  # title
p <- p + geom_text(label="2014", x=2, y=1.1*(max(a$pct10, a$pct14)), hjust=-0.1, size=16, family= "Product Sans")  # title

p + theme(panel.background = element_blank(), 
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            panel.border = element_blank(),
            plot.margin = unit(c(1,2,1,2), "cm")) +
  labs(title = "Overall Shifts in Religious Population Size (2010-2014)", subtitle = "Does Not Consider Gains/Losses Through Birth or Death", caption = "Data: CCES Panel (2010-2014)")

ggsave(file="slop.png", type = "cairo-png", width = 30, height = 21)
