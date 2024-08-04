####load packages####
library(pacman)
pacman::p_load(ggplot2, ggthemes, dplyr, magrittr, purrr, zoo, ggtext, paletteer)

####setting####
Sys.setlocale("LC_TIME", "C")

####work####
# create a data frame
set.seed(5)
dt<-data.frame(dates=seq(as.Date('2011-01-01'), as.Date('2020-12-31'), by = 'days'),var1=rnorm(3653))
years=seq(as.Date('2011-01-01'), as.Date('2020-12-31'), by = 'years')
dt %<>% mutate(var1=ifelse(dates>=years[2] & dates<years[3], var1-0.4, var1)) %>%
  mutate(var1=ifelse(dates>=years[3] & dates<years[4], var1-0.3, var1)) %>%
  mutate(var1=ifelse(dates>=years[4] & dates<years[5], var1-0.2, var1)) %>%
  mutate(var1=ifelse(dates>=years[5] & dates<years[6], var1-0.1, var1)) %>%
  mutate(var1=ifelse(dates>=years[6] & dates<years[7], var1, var1)) %>%
  mutate(var1=ifelse(dates>=years[7] & dates<years[8], var1+0.1, var1)) %>%
  mutate(var1=ifelse(dates>=years[8] & dates<years[9], var1+0.25, var1)) %>%
  mutate(var1=ifelse(dates>=years[9] & dates<years[10], var1+0.33, var1)) %>%
  mutate(var1=ifelse(dates>=years[10], var1+0.5, var1))

# modify 
df<-mutate(dt,date=format(dates,"%m-%d")) %>%
  mutate(Month=format(dates,"%b")) %>%
  mutate(years=format(dates,"%Y")) %>%
  mutate(avg_var = rollmean(var1, k=30, fill=NA, align='right'))

#set date seq for plot
df$date<-as.Date(df$date, "%m-%d")

# color palettes
color_green<-paletteer_c("pals::ocean.algae", 15)
mycolor<-rev(color_green[5:(nrow(distinct(df,years))+4)])

# ggplot2
p30<-ggplot(df, aes(x = date,
                    y = avg_var,
                    group = factor(years),
                    color = factor(years)))+
  geom_hline(yintercept=0, linetype='dashed', col = alpha("black", 0.7))+
  geom_line(linewidth=1.1)+
  scale_colour_manual(values = mycolor)+
  scale_x_date(date_labels = "%b", breaks = "1 month",
               limits=c(as.Date("2024-01-01"), as.Date("2024-12-31")))+
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 1),)+
  labs(x="",y="",title = "Average var, backward 30 days", color='Year')+
  coord_radial(r.axis.inside = T, expand = F)+
  theme(panel.grid.minor = element_blank(),
        plot.title = element_markdown(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        panel.background = element_rect(fill = alpha("grey95", 0.7)))
p30

ggsave(filename = paste0("coord_radial_demo.jpg"), width = 15, height = 15, units = "cm", dpi = 300)

