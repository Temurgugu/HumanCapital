rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 

library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggthemes)
library(extrafont)
library(scales)
library(grid)
library(GGally)

tg_rural <- readxl::read_excel("data/economic_rural.xlsx")

tg_rural_fl <- dplyr::filter(tg_rural, type != "Not-identified worker" &
                                       rural_urban == "Rural")

tg_p_3 <- ggplot2::ggplot(data=tg_rural_fl, aes(year, number, color = type))+
          geom_line()+
          geom_point()+
          theme_gray()+
          theme_classic()+
          theme(axis.title.x = element_text(colour="black", size=7, hjust=0.5),
                axis.title.y = element_text(colour="black", size=7, hjust=0.5),
                axis.text.x=element_text(angle = 0, hjust=0.5, size=7, colour="black"),
                axis.text.y=element_text(angle = 0, hjust=0.5, size=7, colour="black"),
                plot.title=element_text(colour="black", size=8),
                plot.caption = element_text(size=6, colour="black", hjust=0),
                legend.title=element_text(size=6), 
                legend.text=element_text(size=5),
                legend.position = "right")+
          labs(title = "Distribution of Population Aged 15 and Older by Economic Status in Rural Areas",
               subtitle ="",
               caption = "*Time period 2002-2016 is recalculated according to 2014 general population census; The Sampling Frame from 2017 is the 2014 Population Census Database.",
               color="")+
          xlab("")+
          ylab("Thousand persons*")+
          scale_color_discrete(name = "Economic Status",
                              labels = c("Hired","Population outside labour force", "Self-employed", "Unemployed"))+
          scale_x_continuous(breaks=seq(1998, 2020, 2))



tg_rural_fl_1 <- dplyr::filter(tg_rural, type != "Not-identified worker",
                                 year >= 2006)

income_2 <- ggplot2::ggplot(data=tg_rural_fl_1, aes(year, number))+
            geom_col(aes(fill = type), position = "fill")+
            theme(axis.title.x = element_text(colour="black", size=6, hjust=0.5),
                  axis.title.y = element_text(colour="black", size=6, hjust=0.5),
                  axis.text.x=element_text(angle = 90, hjust=0.5, size=6, colour="black"),
                  axis.text.y=element_text(angle = 0, hjust=0.5, size=6, colour="black"),
                  plot.caption = element_text(size=6, colour="black", hjust=0),
                  plot.title=element_text(colour="black", size=7),
                  legend.title=element_text(size=6), 
                  legend.key.size = unit(4, 'mm'),
                  legend.text=element_text(size=5),
                  legend.position = "bottom",
                  strip.text = element_text(size = 6))+
            labs(title = "Distribution of Population Aged 15 and Older by Economic Status")+
            xlab("")+
            ylab("Percentage")+
            scale_x_continuous(breaks=seq(2006, 2020, 1))+
            scale_y_continuous(labels = scales::percent)+
            scale_fill_discrete(name = "",
                                labels = c("Hired","Population outside labour force", "Self-employed", "Unemployed")) +
            facet_grid(rows=vars(rural_urban), scales = "free")


tg_data_1 <- readxl::read_excel("data/incomeshh_rural.xlsx")
tg_data_fil <- dplyr::filter(tg_data_1, group_2_en == "Cash income and transfers" &
                                  group_1_en == "Wages" | 
                                  group_1_en == "Self-employment" | 
                                  group_1_en == "Selling agricultural production"| 
                                  group_1_en == "Non-cash income")


income_3 <- ggplot2::ggplot(data=tg_data_fil, aes(year, number))+
            geom_col(aes(fill = group_1_en), 
                     position = "fill")+
            theme(axis.title.x = element_text(colour="black", size=6, hjust=0.5),
                  axis.title.y = element_text(colour="black", size=6, hjust=0.5),
                  axis.text.x=element_text(angle = 90, hjust=0.5, size=6, colour="black"),
                  axis.text.y=element_text(angle = 0, hjust=0.5, size=6, colour="black"),
                  plot.caption = element_text(size=9, colour="black", hjust=0),
                  plot.title=element_text(colour="black", size=7),
                  legend.title=element_text(size=6),
                  legend.key.size = unit(4, 'mm'),
                  legend.text=element_text(size=5),
                  legend.position = "bottom",
                  strip.text = element_text(size = 6))+
            #guides(fill=guide_legend(nrow=2, byrow=TRUE))+
            labs(title = "Distribution of Average Monthly Incomes per Household")+
            xlab("")+
            ylab("")+
            scale_x_continuous(breaks=seq(2006, 2019, 1))+
            scale_y_continuous(labels = scales::percent)+
            scale_fill_discrete(name = "",
                                labels = c("Non-cash income","Self-employment", "Selling agriculture products", "Wages")) +
            facet_grid(rows=vars(rural_urban), scales = "free")


grid::pushViewport(grid::viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
print(tg_p_3, vp = vplayout(1, 1:2))
print(income_2, vp = vplayout(2, 1))
print(income_3, vp = vplayout(2, 2))



  
bottom_row <- cowplot::plot_grid(income_2, income_3, ncol = 2, labels = c("b", "c"))

tg <- cowplot::plot_grid(tg_p_3, bottom_row, nrow = 2, labels = c("a", "b", "c"))
tg_1 <- cowplot::add_sub(tg, "",
                         size = 6,
                         x = 0.5,
                         y = 0.5,
                         hjust = 0.5,
                         vjust = 0.5)
       
ggsave("tg_1.JPG", 
       plot = tg_1,
       width = 170,
       height = 125,
       units = "mm")



