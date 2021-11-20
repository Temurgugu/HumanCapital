rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 
#=== Economic status and income in rural areas === 

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


#import data 

tg_EconomicStatus <- readr::read_csv("data/economic_rural.csv")
tg_IncomeshhRural <- readr::read_csv("data/incomeshh_rural.csv")


#filter data

tg_rural_filter <- dplyr::filter(tg_EconomicStatus, type != "Not-identified worker" &
                                           rural_urban == "Rural")

EconomicStatus <- ggplot2::ggplot(tg_rural_filter, aes(year, number, color = type))+
                  geom_line()+
                  geom_point()+
                  theme_gray()+
                  theme_classic()+
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_text(colour="black", size=7, hjust=0.5),
                        axis.text.x=element_text(angle = 0, hjust=0.5, size=7, colour="black"),
                        axis.text.y=element_text(angle = 0, hjust=0.5, size=7, colour="black"),
                        plot.title=element_text(colour="black", size=8),
                        plot.caption = element_text(size=6, colour="black", hjust=0),
                        legend.title=element_text(size=6), 
                        legend.text=element_text(size=6),
                        legend.position = "right")+
                  labs(title = "Distribution of Population Aged 15 and Older by Economic Status in Rural Areas",
                       caption = "*Time period 2002-2016 is recalculated according to 2014 general population census; The Sampling Frame from 2017 is the 2014 Population Census Database.")+
                  xlab("")+
                  ylab("Thousand persons*")+
                  scale_color_discrete(name = "Economic Status",
                                       labels = c("Hired","Population outside labour force", "Self-employed", "Unemployed"))+
                  scale_x_continuous(breaks=seq(1998, 2020, 2))



tg_rural_filter_2 <- dplyr::filter(tg_EconomicStatus, type != "Not-identified worker",
                                                      year >= 2006)

EconomicStatusPer <- ggplot2::ggplot(tg_rural_filter_2, aes(year, number))+
                     geom_col(aes(fill = type), position = "fill")+
                     theme(axis.title.x = element_blank(),
                           axis.title.y = element_text(colour="black", size=6, hjust=0.5),
                           axis.text.x=element_text(angle = 90, hjust=0.5, size=6, colour="black"),
                           axis.text.y=element_text(angle = 0, hjust=0.5, size=6, colour="black"),
                           plot.caption = element_text(size=6, colour="black", hjust=0),
                           plot.title=element_text(colour="black", size=7),
                           legend.title=element_text(size=6), 
                           legend.key.size = unit(4, 'mm'),
                           legend.text=element_text(size=6),
                           legend.position = "bottom",
                           strip.text = element_text(size = 6))+
                     guides(fill=guide_legend(nrow=2,byrow=TRUE))+
                     labs(title = "Distribution of Population Aged 15 and Older by Economic Status")+
                     xlab("")+
                     ylab("Percentage")+
                     scale_x_continuous(breaks=seq(2006, 2020, 1))+
                     scale_y_continuous(labels = scales::percent)+
                     scale_fill_discrete(name = "",
                                         labels = c("Hired","Population outside labour force", "Self-employed", "Unemployed")) +
                     facet_grid(rows=vars(rural_urban), scales = "free")



tg_incomeshh_rural_filter <- dplyr::filter(tg_IncomeshhRural,  group_2_en == "Cash income and transfers" &
                                                               group_1_en == "Wages" | 
                                                               group_1_en == "Self-employment" | 
                                                               group_1_en == "Selling agricultural production"| 
                                                               group_1_en == "Non-cash income")


MonthlyIncomes <- ggplot2::ggplot(tg_incomeshh_rural_filter, aes(year, number))+
                  geom_col(aes(fill = group_1_en), position = "fill")+
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_text(colour="black", size=6, hjust=0.5),
                        axis.text.x=element_text(angle = 90, hjust=0.5, size=6, colour="black"),
                        axis.text.y=element_text(angle = 0, hjust=0.5, size=6, colour="black"),
                        plot.caption = element_text(size=9, colour="black", hjust=0),
                        plot.title=element_text(colour="black", size=7),
                        legend.title=element_text(size=6),
                        legend.key.size = unit(4, 'mm'),
                        legend.text=element_text(size=6),
                        legend.position = "bottom",
                        strip.text = element_text(size = 6))+
                  guides(fill=guide_legend(nrow=2, byrow=TRUE))+
                  labs(title = "Distribution of Average Monthly Incomes per Household")+
                  xlab("")+
                  ylab("")+
                  scale_x_continuous(breaks=seq(2006, 2019, 1))+
                  scale_y_continuous(labels = scales::percent)+
                  scale_fill_discrete(name = "",
                                      labels = c("Non-cash income","Self-employment", "Selling agriculture products", "Wages")) +
                  facet_grid(rows=vars(rural_urban), scales = "free")


groupFig <- cowplot::plot_grid(EconomicStatusPer, MonthlyIncomes, ncol = 2, labels = c("b", "c"))

groupFig2 <- cowplot::plot_grid(EconomicStatus, groupFig, nrow = 2, labels = c("a", "b", "c"))

       
ggplot2::ggsave("visualization/Fig1.JPG", 
                plot = groupFig2,
                width = 170,
                height = 125,
                units = "mm")



