install.packages("ggrepel")
install.packages("Rcpp")
update.packages("ggrepel")
#no overlapping text in ggplot2
library(ggrepel)
library(tidyverse)
library(scales)


#color palette

nature <- c("#445E72", "#5A90BF", "#A09A8C","#C6C0B3","#FDE9D3", "#BCCC76", "#7DB269")


#### table 9 botanical gardens####

df <- data.frame (period  = c("19.5 ~ 19.12","20.2","20.6","20.7","20.8","20.9","20.10","20.11"),
                  noUsers = c("268223", "109933", "331766","305907","348974","313856","602063", "437443"),
                  covid = c("Before (monthly ave. of 8 months since opening)", "Initial Outbreak", "During","During","During","During","During","During"))


df$period <- factor(df$period, levels = df$period)
df$covid <- factor(df$covid, levels = c("Before (monthly ave. of 8 months since opening)", "Initial Outbreak", "During"))
df$noUsers <- as.numeric(df$noUsers)

fig <- df %>% 
  ggplot(aes(period, noUsers, fill = covid))+
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(scales::comma(noUsers, 1))),
            vjust = -0.5,
            inherit.aes = TRUE,
            size = 3.5)+
  labs(x = "Period (year.month)", y = "Number of Visitors", fill = "Pandemic Timeline")+
  scale_fill_manual(values = nature[c(1,2,7)]) +
  scale_y_continuous(limits = c(0, 650000), breaks = seq(0, 650000, 200000),labels = comma)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,
                                   size = 10,
                                   vjust = 0.5,
                                   hjust=0.5),
        text = element_text(size = 15),
        panel.grid.minor.x =element_blank(),
        panel.grid.major.x=element_blank())



ggsave(fig, filename = "fig.png", dpi = 600, width = 10, height = 4, units = "in")        
        


### figure 7: factors in cosidering future housing ####

df7 <- data.frame(
  factor = c("Comfort","Convenient Public Transportation", "Convenience Facilities", "Educational Environment", "Distance between Home and Job"),
  value = c(38, 25, 19, 11,7)
)

df7$factor <- factor(df7$factor, levels = df7$factor)


fig7 <- ggplot(df7, aes(x="", y=value, fill=factor)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(x = 1.7, label = paste(value,"%", sep = "")),
            size = 5,
            position = position_stack(vjust = 0.5))+
  coord_polar("y", start=0) +
  labs(fill ="")+
  scale_fill_manual(values = nature[6:1]) +
  theme(text = element_text(size = 15))+
  theme_void(base_size = 15)  # remove background, grid, numeric labels


ggsave(fig7, filename = "fig7.png", dpi = 600, width = 8, height = 4, units = "in") 


### figure 8: Conceptual Range of UGS ####

df8 <- data.frame(
  concept = c("Other", "Reservoirs", "Amusement Parks", "National Parks", "Indoor Gardens", "Green Spaces in Private Residences", "Roadside Green Buffer Zones", "Green Spaces in Public Institutions", "Parks By Commute Routes", "Green Spaces Near Urban Streams", "Mountain Hiking Courses", "Rooftop Gardens", "Urban Forests"),
  value = c(0.93, 43.93, 50.93, 57.01, 65.89, 67.29, 68.69, 72.90, 75.23, 75.70, 78.50, 81.78, 86.45)
) %>% 
  arrange(desc(value, concept))

df8$concept <- factor(df8$concept, levels = df8$concept)



fig8 <- df8 %>% 
  ggplot(aes(value, concept))+
  geom_bar(stat = "identity", fill = nature[7], width = .75) +
  geom_text(aes(label = value), 
                hjust = -0.5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE ) +
  labs(x ="Percentage (%)", y = NULL) +
  theme_minimal(base_size = 12)+
  theme(axis.title=element_text(size=8),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank())+
  expand_limits(x=c(0,100))



ggsave(fig8, filename = "fig8.png", dpi = 600, width = 8, height = 4, units = "in") 

### figure 9: Importance of UGS  before and with COVID19 ####

df9 <- data.frame(
  imp = c("Very Unimportant", "Very Unimportant", "Unimportant", "Unimportant", "Average", "Average", "Important", "Important", "Very Important", "Very Important"),
  covid = c("Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19"),
  value = c(0.0, 0.5, 3.3, 2.3, 17.3, 7.9, 43.5, 25.2, 36.0, 64.0))


df9$covid <- factor(df9$covid, levels = c("With COVID-19", "Before COVID-19"))
df9$imp <- factor(df9$imp, levels = c("Very Important", "Important", "Average", "Unimportant", "Very Unimportant"))


fig9 <- df9 %>%
  ggplot(aes(value, imp, fill = covid, group = covid))+
  geom_bar(position = "dodge", stat = "identity", width = .75) +
  geom_text(aes(label = value, group = covid),
            hjust = -0.5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE )+
  scale_fill_manual(values = nature[c(7,4)])+
  labs(x ="Percentage (%)", y = NULL, fill = "") +
  theme_minimal(base_size = 15)+
  theme(axis.title=element_text(size=10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank())+
  guides(fill = guide_legend(reverse=TRUE))+ expand_limits(x=c(0,70))



ggsave(fig9, filename = "fig9.png", dpi = 600, width = 8, height = 4, units = "in") 


### fig 10. Frequency of utilization of UGS before and in wake of COVID-19 ####

df10 <- data.frame(
  ans = c("Never", "Never", "Rarely", "Rarely", "1-2 Times per Month", "1-2 Times per Month", "1-2 Times per Week", "1-2 Times per Week", "Almost Daily", "Almost Daily", "Daily","Daily"),
  covid = c("Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19"),
  value = c(0.5, 3.3, 11.2, 19.6, 24.8, 18.2, 43.0, 33.6, 15.0, 21.0, 5.6, 4.2))


df10$covid <- factor(df10$covid, levels = c("With COVID-19", "Before COVID-19"))
df10$ans <- factor(df10$ans, levels = c("Daily", "Almost Daily", "1-2 Times per Week","1-2 Times per Month", "Rarely", "Never"))


fig10 <- df10 %>%
  ggplot(aes(value, ans, fill = covid, group = covid))+
  geom_bar(position = "dodge", stat = "identity", width = .75) +
  geom_text(aes(label = value, group = covid),
            hjust = -0.5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE )+
  scale_fill_manual(values = nature[c(7,4)])+

  labs(x ="Percentage (%)", y = NULL, fill = "") +
  theme_minimal(base_size = 15)+
  theme(axis.title=element_text(size=10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank())+
  guides(fill = guide_legend(reverse=TRUE))+ expand_limits(x=c(0,50))



ggsave(fig10, filename = "fig10.png", dpi = 600, width = 8, height = 4, units = "in") 

#### figure 11 reasons for increasing the use of UGS in the wake of COVID-19 (PIE CHART) #### 


# Create Data
df11 <- data.frame(
  method=c("Replace Restricted Facilities", "Improve Psychological Health", "Replace Exercise Facilities", "Prevent Disease Transmission", "Increased Leisure Time", "Enhance Immune System", "Other"),
  value=c(27.78,22.22,21.11,16.67,6.67, 4.44,1.11)
)

df11$method <- factor(df11$method, levels = df11$method)



fig11 <- ggplot(df11, aes(x="", y=value, fill=method)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text_repel(aes(x = 1.7, label = paste(value, "%", sep = "")),
                  size = 5,
                  position = position_stack(vjust = 0.5))+
  coord_polar("y", start=0) +
  labs(fill ="")+
  scale_fill_manual(values = nature[7:1]) +
  theme(text = element_text(size = 15))+
  theme_void(base_size = 15)  # remove background, grid, numeric labels


ggsave(fig11, filename = "fig11.png", dpi = 600, width = 8, height = 4, units = "in") 

### fig 12 reason for use before and after ####

df12 <- data.frame(
  ans = c("Exercise", "Exercise", "Relaxation or Meditation", "Relaxation or Meditation", "Connecting with Nature", "Connecting with Nature", "Spending Time with Family and Friends", "Spending Time with Family and Friends",  "En Route to Destination", "En Route to Destination", "Walking Pets", "Walking Pets", "Other", "Other"),
  covid = c("Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19", "Before COVID-19", "With COVID-19"),
  value = c(46.8, 38.7, 57.3, 51.6, 41.1, 45.2, 54.8,  45.2, 19.4, 23.4, 7.3, 5.6, 0.8, 1.6))


df12$covid <- factor(df12$covid, levels = c("With COVID-19", "Before COVID-19"))
df12$ans <- factor(df12$ans, levels = c("Other", "Walking Pets", "En Route to Destination", "Spending Time with Family and Friends", "Connecting with Nature", "Relaxation or Meditation","Exercise"
))


 fig12 <- df12 %>%
  ggplot(aes(value, ans, fill = covid, group = covid))+
  geom_bar(position = "dodge", stat = "identity", width = .75) +
  geom_text(aes(label = value, group = covid),
            hjust = -0.5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE )+
  scale_fill_manual(values = nature[c(7,4)])+
  
  labs(x ="Percentage (%)", y = NULL, fill = "") +
  theme_minimal(base_size = 15)+
  theme(axis.title=element_text(size=10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank())+
  guides(fill = guide_legend(reverse=TRUE))+ expand_limits(x=c(0,70))



ggsave(fig12, filename = "fig12.png", dpi = 600, width = 8, height = 4, units = "in") 

### fig 13 reasons for reducing use of UGS during COVID-19 ####

# Create Data
df13 <- data.frame(
  method=c("Reduced Outdoor Activity", "Fewer Gatherings with Acquaintances", "Reduced Need for Outdoor Movement", "Other", "Cleanliness of Facilities", "New Activities/Alternatives"),
  value=c(78.23, 11.29, 4.03, 3.23, 1.61, 1.61)
)

df13$method <- factor(df13$method, levels = df13$method)



fig13 <- ggplot(df13, aes(x="", y=value, fill=method)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text_repel(aes(x = 1.7, label = paste(value, "%", sep = "")),
                  size = 3,
                  position = position_stack(vjust = 0.5))+
  coord_polar("y", start=0) +
  labs(fill ="")+
  scale_fill_manual(values = nature[c(7:1)]) +
  theme(text = element_text(size = 15))+
  theme_void(base_size = 15)  # remove background, grid, numeric labels


ggsave(fig13, filename = "fig13.png", dpi = 600, width = 8, height = 4, units = "in") 


#### figure 14 preferred transportation method for visiting UGS (PIE CHART) #### 


# Create Data
df14 <- data.frame(
  method=c("Walking", "Public Transportation", "Car", "Bicycle", "Other"),
  value=c(73.83,12.62,6.54,6.07,0.93)
)

df14$method <- factor(df14$method, levels = df14$method)



fig14 <- ggplot(df14, aes(x="", y=value, fill=method)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text_repel(aes(x = 1.7, label = paste(value, "%", sep = "")),
            size = 5,
            position = position_stack(vjust = 0.5))+
  coord_polar("y", start=0) +
  labs(fill ="")+
  scale_fill_manual(values = nature[c(7,6,3,5,1)]) +
  theme(text = element_text(size = 15))+
  theme_void(base_size = 15)  # remove background, grid, numeric labels
  


ggsave(fig14, filename = "fig14.png", dpi = 600, width = 8, height = 4, units = "in") 


#### figure 15 preferred Time needed to visit UGS (PIE CHART) #### 


# Create Data
df15 <- data.frame(
  method=c("30 Minutes or Less", "1 Hour or Less", "Between 1 and 2 Hours", "Between 2 and 3 Hours"),
  value=c(72.90,17.76,7.01,2.34)
)

df15$method <- factor(df15$method, levels = df15$method)



fig15 <- ggplot(df15, aes(x="", y=value, fill=method)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text_repel(aes(x = 1.7, label = paste(value, "%", sep = "")),
                  size = 5,
                  position = position_stack(vjust = 0.5))+
  coord_polar("y", start=0) +
  labs(fill ="")+
  scale_fill_manual(values = nature[c(7,6,3,5,1)]) +
  theme(text = element_text(size = 15))+
  theme_void(base_size = 15)  # remove background, grid, numeric labels



ggsave(fig15, filename = "fig15.png", dpi = 600, width = 8, height = 4, units = "in") 

#### figure 16 facilities to encourage use of UGS (PIE CHART) #### 


# Create Data
df16 <- data.frame(
  method=c("Rest and Recreational (Outdoor Chairs and Benches)", "Nature 'Healing' or Therapeutic (Forest-bathing)", "Nature/Cultural Immersion (Camp Sites)", "Sports-related", "Nature-Related Education and Exhibitions"),
  value=c(38.94, 26.11, 16.59, 11.50, 6.86)
)

df16$method <- factor(df16$method, levels = df16$method)



fig16 <- ggplot(df16, aes(x="", y=value, fill=method)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text_repel(aes(x = 1.7, label = paste(value, "%", sep = "")),
                  size = 5,
                  position = position_stack(vjust = 0.5))+
  coord_polar("y", start=0) +
  labs(fill ="")+
  scale_fill_manual(values = nature[c(7,6,3,5,1)]) +
  theme(text = element_text(size = 15))+
  theme_void(base_size = 15)  # remove background, grid, numeric labels



ggsave(fig16, filename = "fig16.png", dpi = 600, width = 8, height = 4, units = "in") 

#### figure 17 Services to encourage use of UGS  #### 


# Create Data
df17 <- data.frame(
  method=c("Comfortable Places for Relaxation", "Tackling Environmental Issues", "Health and Well-being", "Increased Biodiversity", "Arts and Culture-Related Activities", "Food and Drink", "Interaction with Diverse Members of the Community", "Senior Citizens' Welfare", "Job Creation", "Child Care and Educating Youth"),
  value=c(31.36, 18.23, 16.18, 11.93, 7.16, 5.45, 3.24, 2.04, 2.04, 1.87)
)

df17$method <- factor(df17$method, levels = df17$method)

 fig17 <- df17 %>%
  ggplot(aes(method, value, fill = method))+
  geom_bar(stat = "identity", width = .50) +
  geom_text(aes(label = value),
            vjust = -0.5,
            inherit.aes = TRUE )+
  scale_fill_manual(values = c(nature[c(7:1)], "#003E5C", "#011926", "#010101"))+
  labs(x = NULL, y = "Percentage (%)", fill = "") +
  theme_minimal(base_size = 15)+
  theme(axis.title=element_text(size=5),
        legend.position = "left",
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        panel.grid.minor.x =element_blank(),
        panel.grid.major.x=element_blank())+
  guides(fill = guide_legend(reverse=FALSE))+ expand_limits(y=c(40,0))

ggsave(fig17, filename = "fig17.png", dpi = 600, width = 8, height = 4, units = "in") 

