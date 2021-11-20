rm(list=ls())    # clear the workspace

############ sankey_analysis ###########

library(readxl)
library(networkD3)
library(ggalluvial)
library(viridis)


code_configurations <- readxl::read_excel("data/code_configurations_1.xlsx")


human_capital_2 <- ggplot(code_configurations, aes(Human_capital, Economic_activities_general, fill= Segments)) +
  geom_tile() +
  geom_text(aes(label = Segments, color = Segments),  size = 3, show.legend = FALSE)+
  scale_color_viridis(option = "inferno", direction = 1,
                      begin = 0, end = 1, discrete=FALSE)+
  scale_fill_viridis(option = "viridis", direction = -1,
                     begin = 0, end = 1, discrete=FALSE)+
  theme(axis.title.x = element_text(colour="black", size=7, hjust=0.5),
        axis.title.y = element_text(colour="black", size=7, hjust=0.5),
        axis.text.x=element_text(angle = 90,  hjust=1, size=7, colour="black"),
        axis.text.y=element_text(colour="black", size=7),
        plot.caption = element_text(size=7, colour="black", hjust=1),
        plot.title=element_text(colour="black", size=7,  hjust=0),
        legend.title=element_text(size=6),
        legend.text=element_text(size=7),
        legend.position="right",
        legend.key.width = unit(1, "line"),
        strip.text = element_text(size = 7))+
  xlab("")+
  ylab("Economic activity")+ 
  labs(fill = "Number of \n Codes")+
  scale_x_discrete()+
  facet_wrap(vars(Round),  scales = "free_x")



ggsave("visualization/Fig3.JPG", 
       plot = human_capital_2,
       width = 170,
       height = 125,
       units = "mm")




