library(dplyr)
library(ggplot2)
library(readxl)
library(cowplot)
library(gridExtra)

df <- read_excel("~/Documents/Sheffield_University/Yixiang.publication/wheat.morphospace/journals/check.modernwheat.years.xlsx")

df <- df %>% select(-bio.status)
df_filtered <- na.omit(df)

df_filtered <- df_filtered %>%
  mutate(Event = case_when(
    Year >= 1960 ~ "After Green Revolution",
    Year < 1960 ~ "Before Green Revolution",
  ))

p1<-ggplot(data=df_filtered, aes(x=Year, y=Tiller_number_June))+geom_smooth(method=lm, se=F)+
  ylab("Final plant height (cm)")+
  geom_point()+theme_bw()
p2<-ggplot(data=df_filtered, aes(x=Year, y=Flower_proportion))+geom_point()+geom_smooth(method=lm, se=F)+theme_bw()

legend <- get_legend(p1)


grid.arrange(p1, p2,  ncol=2, widths=c(2.3, 2.3))

grid.arrange(p1, p2, legend, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.6))

plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)

p3<-ggplot(data=df, aes(x=Year, y=Plant_height_July))+geom_point()+geom_smooth(method=lm)+ylab("Final plant height (cm)")
p4<-ggplot(data=df, aes(x=Year, y=Flower_proportion))+geom_point()+geom_smooth(method=lm)
grid.arrange(p3, p4, ncol=2, widths=c(2.3, 2.3))

model<-lm(data=df, Tiller_June~Year)
summary(model)
anova(model)


model<-lm(data=df, Flower_proportion~Year)
summary(model)
anova(model)

