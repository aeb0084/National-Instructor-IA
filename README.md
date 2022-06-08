# How and why instructors discuss controversial topics in biology.
 
Abby E. Beatty, Jeremiah Henning, Emily P. Driessen, Amanda D. Clark, Robin Costello, Sharday Ewell, Randy Klabacka, Todd Lamb, Kimberly Mulligan, Sheritta Fagbodum, & Cissy J. Ballen  

*Corresponding author: aeb0084@auburn.edu

This repository holds all supplemental materials for "How and why instructors discuss controversial topics in biology".

## Abstract: 

"Traditional biology curricula depict science as an objective field, overlooking the important influence that human values and biases have on what is studied and who can be a scientist. We can address this shortcoming by incorporating Ideological Awareness (IA) into the curriculum, which is an understanding of biases, stereotypes, and assumptions that shape contemporary and historical science. We surveyed a national sample of lower-level biology instructors to determine their views of the (1) purpose of biology education, (2) value of IA in biology, and (3) hesitations to teaching IA in biology. Most instructors reported that “understanding the world” was the main goal of biology education. Despite the perceived benefits of IA, such as increasing student engagement and dispelling misconceptions, instructors were hesitant to implement IA modules due to potential professional consequences. We address how instructor values, purpose, and hesitancies affect teaching practices, and propose systemic approaches to increasing instructor support."


### Quick Key to File Directory: Detailed Descriptions of file use can be found below.

Note: The final data set used in analysis is available for public use. Additionally, deidentified raw survey data is available here. Due to IRB Restrictions all data files used in analysis that contain institutional information prior to the final merged and deidientified data are available upon direct request. Note, some data columns are redacted from the raw data, as they contain identifiable information. Those columns are shaded in gray, are are not rqeuired for replicating primary results. 


Analysis and File Names| Brief Description | Link to File
-------------------------------------|------------------------------------ | -----------------------------------------------------
Suvey Instrument            | Survey given to participants |   [Survey File](National_Survey.pdf)
Final Data File- Quantitative            | Final quantitative data file used in survey analysis |   [Quant Deidentified Data File](National_Survey_Quantitative_deident.csv)
Final Data File- Qualitative            | Final qualitative data file used in survey analysis |   [Qual Deidentified Data File](National_Survey_qualitative_deident.xlsx)

## Supplemental Tables

<img src="SF1.jpg" width="800">

<img src="SF2_Page_1.jpg" width="800">
<img src="SF2_Page_2.jpg" width="800">
<img src="SF2_Page_3.jpg" width="800">
<img src="SF2_Page_4.jpg" width="800">
<img src="SF2_Page_5.jpg" width="800">
<img src="SF2_Page_6.jpg" width="800">

## Statistical and Data Visualization Code

```ruby
library(ggplot2)
library(tidyverse)
library(sp)
library(rgeolocate)
library(usmap)
map=read.csv("National_map.csv")
```

#Map of survey participants by state
```{r}

plot_usmap(data = map, values = "Number", color = "black", labels=TRUE, size=0.5) + 
  scale_fill_continuous(low = "lightgray", high = "plum4", name = "Number of Participants", label = scales::comma) + 
  labs(title = "Participants",
       subtitle = "Distribution of Survey Participants by State.") + 
  theme(legend.position = "right",
        text = element_text(size = 20))

ggsave("participant_map.png", plot=last_plot(), dpi=300, width=15, height=10)

```
> https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html

#Statistical Analysis 
```{r}
library(nlme)
dat=read.csv("National_Survey_Quantitative.csv")
#Demographics
anova(lm(IA_how_often.num ~  state + perc_lecture + rank + inst_type + gender.1 + race.1, data=dat, na.action=na.omit))
#IA 
anova(lm(IA_how_often.num ~  biases_student.num + IA_importance + perc_developed + perc_PEERs + comfort.average, data=dat, na.action=na.omit))
#Teaching Habits
anova(lm(IA_how_often.num ~ Years_teaching + Largest_class + Smallest_class + Perc_lowerdiv + Modifications.num, data=dat, na.action=na.omit))

anova(lm(IA_importance ~  state + perc_lecture + rank + inst_type + gender.1 + race.1, data=dat, na.action=na.omit))

anova(lm(IA_importance ~  biases_student.num  + perc_developed + perc_PEERs + comfort.average, data=dat, na.action=na.omit))

anova(lm(IA_importance ~ Years_teaching + Largest_class + Smallest_class + Perc_lowerdiv + Modifications.num, data=dat, na.action=na.omit))

anova(lm(IA_should_be ~  state + perc_lecture + rank + inst_type + gender.1 + race.1, data=dat, na.action=na.omit))
anova(lm(IA_should_be ~  biases_student.num  + perc_developed + perc_PEERs + comfort.average, data=dat, na.action=na.omit))

summary(lm(IA_should_be ~ Years_teaching + Largest_class + Smallest_class + Perc_lowerdiv + Modifications.num, data=dat, na.action=na.omit))

anova(lm(explicit_link ~  IA_importance, data=dat, na.action=na.omit))
summary(lm(explicit_link ~  IA_importance, data=dat, na.action=na.omit))
cor.test(dat$explicit_link, dat$IA_importance)

anova(lm(IA_how_often.num ~  state  + rank + inst_type + gender.1 + race.1  + Years_teaching + first.gen, data=dat, na.action=na.omit))

anova(lm(IA_importance ~  state  + rank + inst_type + gender.1 + race.1  + Years_teaching + first.gen, data=dat, na.action=na.omit))

anova(lm(IA_should_be ~  state  + rank + inst_type + gender.1 + race.1  + Years_teaching + first.gen, data=dat, na.action=na.omit))

library(emmeans)
em=(lm(IA_how_often.num ~  inst_type, data=dat, na.action=na.omit))
emmeans(em, list(pairwise ~ inst_type), adjust = "tukey")


ggplot(dat, aes(x=IA_importance, y=explicit_link)) + 
  geom_smooth(method=lm)+
  xlab("Viewed Importance of IA") +
  ylab("Need for Explicit Linking of Society to Biology")
```

> How often instructors use IA: Modifications,  IA_importance, perc_developed
> How often IA should be used: perc_lecture, perc_lowerdiv
>IA importance dictates whether or not they feel they need to make explicit links for students; positively and significantly correlated


```{r}
library(ggplot2)
library(ggridges)
library(dplyr)
library(Rmisc)

views=read.csv("IA_views.csv")

dat.av=summarySE(data = views, measurevar="Values", groupvars = c("Category"), na.rm = T, conf.interval = 0.95, .drop = TRUE)

#Density Plot
ggplot(views, aes(x=Values, fill=Category, group=Category))+
  geom_density(alpha=0.4) +
  scale_fill_manual(values=c("#2e4057", "#edae49", "#66a182")) +
  scale_colour_manual(values=c("#2e4057", "#edae49", "#66a182")) +
geom_vline(aes(xintercept = Values, color = Category), data = dat.av, linetype = "dashed") +
  ylab("Density") +
  xlab("")+
  theme(axis.title = element_text(size=12, face="bold"),
        legend.title = element_text(size=12, face="bold"),
        plot.title = element_text(size=15, face = "bold")) +
  ggtitle("Instructor Views of IA Classroom Implementation")



views.sub.often=subset(views, Category == "IA_how_often")
table(views.sub.often$Values)

ggplot(views.sub.often, aes(x=Values)) +
  geom_histogram()

views.sub=subset(views, Category != "IA_how_often")

ggplot(views, aes(x=Values, fill=Category$IA_how_often, group=Category$IA_how_often)) +
  geom_histogram()

dat.av.sub=summarySE(data = views.sub, measurevar="Values", groupvars = c("Category"), na.rm = T, conf.interval = 0.95, .drop = TRUE)


ggplot(views.sub, aes(x=Values, fill=Category, group=Category))+
  geom_density(alpha=0.4) +
  scale_fill_manual(values=c("#2e4057",  "#66a182")) +
  scale_colour_manual(values=c("#2e4057", "#66a182")) +
geom_vline(aes(xintercept = Values, color = Category), data = dat.av.sub, linetype = "dashed") +
  ylab("Density") +
  xlab("")+
  theme(axis.title = element_text(size=12, face="bold"),
        legend.title = element_text(size=12, face="bold"),
        plot.title = element_text(size=15, face = "bold")) +
  ggtitle("Instructor Views of IA Classroom Implementation")

ggsave("instruc_views.png", plot=last_plot(), dpi=300, width=5, height=5)

```

```{r}
dat.av=summarySE(data = dat, measurevar="comfort.average", groupvars = c("gender.1"), na.rm = T, conf.interval = 0.95, .drop = TRUE)

ggplot(dat, aes(x=comfort.average, fill=gender.1, group=gender.1))+
  geom_density(alpha=0.4) +
geom_vline(aes(xintercept = comfort.average, color = gender.1), data = dat.av, linetype = "dashed") +
  ylab("Density") +
  xlab("")+
  theme(axis.title = element_text(size=12, face="bold"),
        legend.title = element_text(size=12, face="bold"),
        plot.title = element_text(size=15, face = "bold")) 

dat.av=summarySE(data = dat, measurevar="comfort.average", groupvars = c("race.1"), na.rm = T, conf.interval = 0.95, .drop = TRUE)

ggplot(dat, aes(x=comfort.average, fill=race.1, group=race.1))+
  geom_density(alpha=0.4) +
geom_vline(aes(xintercept = comfort.average, color = race.1), data = dat.av, linetype = "dashed") +
  ylab("Density") +
  xlab("")+
  theme(axis.title = element_text(size=12, face="bold"),
        legend.title = element_text(size=12, face="bold"),
        plot.title = element_text(size=15, face = "bold")) 

table(dat$how_often_cat)
table(dat$IA_how_often.num)

mean(dat$genome_editing, na.rm=T) 
mean(dat$unethical_exp, na.rm=T) 
mean(dat$eugenics, na.rm=T) 
mean(dat$med_rights, na.rm=T) 
mean(dat$representation, na.rm=T) 
mean(dat$religion, na.rm=T) 
mean(dat$disp_healthcare, na.rm=T) 
mean(dat$env_racism, na.rm=T) 

mean(dat$traditional, na.rm=T) 
mean(dat$comfort.average, na.rm=T) 

cor.test(dat$race, dat$explicit_link)

anova(lm(explicit_link ~  state, data=dat, na.action=na.omit))
anova(lm(explicit_link ~  Largest_class, data=dat, na.action=na.omit))
anova(lm(explicit_link ~  Perc_lowerdiv, data=dat, na.action=na.omit))
anova(lm(explicit_link ~  Perc_upperdiv, data=dat, na.action=na.omit))
anova(lm(explicit_link ~  biases_student.num, data=dat, na.action=na.omit))
anova(lm(explicit_link ~  IA_importance, data=dat, na.action=na.omit))
anova(lm(explicit_link ~  race.1, data=dat, na.action=na.omit))
anova(lm(explicit_link ~  gender.1, data=dat, na.action=na.omit))
anova(lm(explicit_link ~  first.gen, data=dat, na.action=na.omit))
anova(lm(comfort.average ~  race.1, data=dat, na.action=na.omit))
anova(lm(comfort.average ~  first.gen, data=dat, na.action=na.omit))

cor.test(dat$IA_importance, dat$explicit_link)
cor.test(dat$IA_importance, dat$comfort.average)

cor.test(dat$IA_should_be, dat$Perc_lowerdiv)
cor.test(dat$IA_should_be, dat$explicit_link)
```


```{r}
library(emmeans)
gen=(lm(comfort.average ~  gender.1, data=dat, na.action=na.omit))
anova(gen)
emmeans(gen, list(pairwise ~ gender.1), adjust = "tukey")



ggplot(dat, aes(y=explicit_link, x=Years_teaching))+
  geom_smooth(alpha=0.4) +
  ylab("Response") +
  xlab("Years Teaching")+
  theme(axis.title = element_text(size=12, face="bold"),
        legend.title = element_text(size=12, face="bold"),
        plot.title = element_text(size=15, face = "bold")) 

ggplot(dat, aes(x=explicit_link))+
  geom_density(alpha=0.4, fill="green") +
  ylab("Density") +
  xlab("")+
  theme(axis.title = element_text(size=12, face="bold"),
        legend.title = element_text(size=12, face="bold"),
        plot.title = element_text(size=15, face = "bold")) 
```


#Demographics
```{r}
library(janitor)
library(tidyr)

#calculate distribution of institution types

tabyl(dat, first.gen) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2)
(table(dat$first.gen))


tabyl(dat, race.1) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2)

inst= dat %>% 
    mutate(inst_type = strsplit(as.character(inst_type), ",")) %>% 
    unnest(inst_type)
(table(inst$inst_type))

rank= dat %>% 
    mutate(rank = strsplit(as.character(rank), ",")) %>% 
    unnest(rank)
(table(rank$rank))

gender= dat %>% 
    mutate(gender.1 = strsplit(as.character(gender.1), ",")) %>% 
    unnest(gender.1)
(table(gender$gender.1))


(table(dat$Years_teaching))

race.1= dat %>% 
    mutate(race.1 = strsplit(as.character(race.1), ",")) %>% 
    unnest(race.1)
(table(race.1$race.1))

```

#Figures based on feedback in manuscript
```{r}

ggplot(dat, aes(y=IA_should_be, x=perc_lecture)) +
    geom_violin()

ggplot(dat, aes(y=IA_should_be, x=Perc_lowerdiv)) +
    geom_smooth() 

ggplot(dat, aes(y=IA_importance, x=explicit_link)) +
    geom_smooth() +
  geom_point()

ggplot(dat, aes(y=IA_how_often.num, x=perc_developed)) +
    geom_smooth() 
ggplot(dat, aes(y=IA_how_often.num, x=Modifications.num)) +
    geom_smooth() 
ggplot(dat, aes(y=IA_how_often.num, x=IA_importance)) +
    geom_smooth() 
ggplot(dat, aes(y=IA_how_often.num, x=comfort.average)) +
    geom_smooth() 

```



```{r}

dat_fig=subset(dat, IA_importance >5)

ggplot(dat_fig, aes(y=IA_how_often.num, x=IA_importance)) +
    geom_smooth() 


ggplot(dat, aes(x=IA_how_often.num, y=perc_developed)) +
    geom_smooth() 
ggplot(dat, aes(x=IA_how_often.num, y=Modifications.num)) +
    geom_smooth() 
ggplot(dat, aes(x=IA_how_often.num, y=IA_importance)) +
    geom_smooth() 
ggplot(dat, aes(x=IA_how_often.num, y=comfort.average)) +
    geom_smooth() 


#sometimes, often, and always
dat.do=subset(dat, IA_how_often.num >2)
ggplot(dat.do, aes(x=as.factor(IA_how_often.num), y=perc_developed)) +
    geom_violin(trim=F) +
  geom_boxplot(width=0.2)

#rarely or never
dat.donot=subset(dat, IA_how_often.num <3)

dat=subset(dat, how_often_cat != "" | how_often_cat == "NA")

ggplot(dat, aes(x=how_often_cat, y=perc_developed)) +
    geom_violin(trim=T) +
  geom_boxplot(width=0.2)

ggplot(dat, aes(x=how_often_cat, y=comfort.average)) +
    geom_violin(trim=T) +
  geom_boxplot(width=0.2)

ggplot(dat, aes(x=how_often_cat, y=IA_importance)) +
    geom_violin(trim=T) +
  geom_boxplot(width=0.2)


ggplot(dat, aes(x=how_often_cat, y=Modifications.num)) +
    geom_violin(trim=T) +
  geom_boxplot(width=0.2)


library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggpubr)

imp.av=summarySE(data = dat, measurevar="IA_importance", groupvars = c("how_often_cat"), na.rm = T, conf.interval = 0.95, .drop = TRUE)

ggdotchart(imp.av, y = "IA_importance", x = "how_often_cat",
           color = "how_often_cat",     # Color by groups
           palette = c("lightgray", "turquoise4"), # Custom color palette
           sorting = "descending",  # Sort value in descending order
           rotate = TRUE,  # Rotate vertically
           dot.size = 7,    # Large dot size'
          #label = round(dat$IA_importance,1), repel=T, # Add dot labels
          #label.rectangle=T,
          font.label = list(color = "how_often_cat", size = 10, face="bold", vjust=-1.5), 
           ggtheme = theme_pubr()) +
  ylab("Views of IA Importance") +
  ggtitle("Title") +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold")) +
   theme(axis.title.y=element_text(face="bold"),
            strip.text=element_text(face="bold", size=12),
            legend.title =element_text(size=10),
         axis.title.x =element_text(face="bold")) +
  ylim(0,10) +
  coord_flip() +
    theme_cleveland() 

com.av=summarySE(data = dat, measurevar="comfort.average", groupvars = c("how_often_cat"), na.rm = T, conf.interval = 0.95, .drop = TRUE)

ggdotchart(com.av, y = "comfort.average", x = "how_often_cat",
           color = "how_often_cat",     # Color by groups
           palette = c("lightgray", "turquoise4"), # Custom color palette
           sorting = "descending",  # Sort value in descending order
           rotate = TRUE,  # Rotate vertically
           dot.size = 7,    # Large dot size'
          #label = round(dat$IA_importance,1), repel=T, # Add dot labels
          #label.rectangle=T,
          font.label = list(color = "how_often_cat", size = 10, face="bold", vjust=-1.5), 
           ggtheme = theme_pubr()) +
  ylab("Comfort with IA Materials") +
  ggtitle("Title") +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold")) +
   theme(axis.title.y=element_text(face="bold"),
            strip.text=element_text(face="bold", size=12),
            legend.title =element_text(size=10),
         axis.title.x =element_text(face="bold")) +
  ylim(0,10) +
  coord_flip() +
    theme_cleveland() 

mod.av=summarySE(data = dat, measurevar="Modifications.num", groupvars = c("how_often_cat"), na.rm = T, conf.interval = 0.95, .drop = TRUE)

ggdotchart(mod.av, y = "Modifications.num", x = "how_often_cat",
           color = "how_often_cat",     # Color by groups
           palette = c("lightgray", "turquoise4"), # Custom color palette
           sorting = "descending",  # Sort value in descending order
           rotate = TRUE,  # Rotate vertically
           dot.size = 7,    # Large dot size'
          #label = round(dat$IA_importance,1), repel=T, # Add dot labels
          #label.rectangle=T,
          font.label = list(color = "how_often_cat", size = 10, face="bold", vjust=-1.5), 
           ggtheme = theme_pubr()) +
  ylab("Frequency of Course Modification") +
  ggtitle("Title") +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold")) +
   theme(axis.title.y=element_text(face="bold"),
            strip.text=element_text(face="bold", size=12),
            legend.title =element_text(size=10),
         axis.title.x =element_text(face="bold")) +
  ylim(0,4) +
  coord_flip() +
    theme_cleveland() 

dev.av=summarySE(data = dat, measurevar="perc_developed", groupvars = c("how_often_cat"), na.rm = T, conf.interval = 0.95, .drop = TRUE)

ggdotchart(dev.av, y = "perc_developed", x = "how_often_cat",
           color = "how_often_cat",     # Color by groups
           palette = c("lightgray", "turquoise4"), # Custom color palette
           sorting = "descending",  # Sort value in descending order
           rotate = TRUE,  # Rotate vertically
           dot.size = 7,    # Large dot size'
          #label = round(dat$IA_importance,1), repel=T, # Add dot labels
          #label.rectangle=T,
          font.label = list(color = "how_often_cat", size = 10, face="bold", vjust=-1.5), 
           ggtheme = theme_pubr()) +
  ylab("Percept of Course Developed") +
  ggtitle("Title") +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold")) +
   theme(axis.title.y=element_text(face="bold"),
            strip.text=element_text(face="bold", size=12),
            legend.title =element_text(size=10),
         axis.title.x =element_text(face="bold")) +
  ylim(0,100) +
  coord_flip() +
    theme_cleveland() 
```

#Example code from open note paper
```{r}
q.pl=read.csv("qual_r_coded.csv")
fear=subset(q.pl, Question == "fear")
often_rn=subset(q.pl, Question == "how_often" & Subquestion == "rarely_never")
often_sa=subset(q.pl, Question == "how_often" & Subquestion == "some_always")
purpose=subset(q.pl, Question == "purpose")
benefits=subset(q.pl, Question == "benefits")

library(ggplot2)

ggplot(fear, aes(x = reorder(Code, -Percentage), y = Percentage)) +
         geom_bar(stat = "identity", fill="lightgray") +
  coord_flip() +
  ggtitle("Fears") 

ggplot(often, aes(x = reorder(Code, -Percentage), y = Percentage, fill=Subquestion)) +
         geom_bar(position="stack", stat = "identity") +
  coord_flip() +
    scale_fill_manual(values=c("turquoise4", "turquoise3")) +
  ggtitle("How often: Stacked") 

ggplot(often_rn, aes(x = reorder(Code, -Percentage), y = Percentage)) +
         geom_bar(stat = "identity", fill="blue") +
  coord_flip() +
  ggtitle("How often_ rarely never") 

ggplot(often_sa, aes(x = reorder(Code, -Percentage), y = Percentage)) +
         geom_bar(stat = "identity", fill="blue") +
  coord_flip() +
  ggtitle("How often_ some always") 

ggplot(purpose, aes(x = reorder(Code, -Percentage), y = Percentage)) +
         geom_bar(stat = "identity", fill="tomato3") +
  coord_flip() +
  ggtitle("Purpose") 

ggplot(benefits, aes(x = reorder(Code, -Percentage), y = Percentage)) +
         geom_bar(stat = "identity", fill="blue") +
  coord_flip() +
  ggtitle("Benefits") 

ggplot(dat, aes(y = how_often_cat)) +
         geom_bar(position="stack", stat = "identity") +
  coord_flip() +
    scale_fill_manual(values=c("turquoise4", "turquoise3")) +
  ggtitle("How often: Stacked")

dat2=table(dat$how_often_cat)
ggplot(dat, aes(x=how_often_cat)) + geom_bar(position="dodge")

dat2=subset(dat, IA_how_often.num != "NA")

p3=ggplot(dat2, aes(x=how_often_cat, fill=how_often_cat)) +  
  geom_bar(stat="count")+
    scale_fill_manual(values=c("turquoise4", "turquoise3")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x= element_blank())

ggsave(p3, file="subimage.png", units="in", width=1, height=2.25, dpi=600)





```


```{r}
q1=ggplot(purpose, aes(x = reorder(Code, -Percentage), y = Percentage)) +
         geom_bar(stat = "identity", fill="tomato3") +
  coord_flip() +
  ggtitle("") +
    theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) 

color=c("turquoise4", "turquoise3")

q2=ggplot(often, aes(x = reorder(Code, -Percentage), y = Percentage, fill=Subquestion)) +
         geom_bar(position="stack", stat = "identity") +
  coord_flip() +
  ggtitle("") +
  scale_fill_manual(values=c("gray67", "gray29")) +
    theme_bw()+
    theme(axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

q2b=ggplot(benefits, aes(x = reorder(Code, -Percentage), y = Percentage)) +
         geom_bar(stat = "identity", fill="turquoise4") +
  coord_flip() +
  ggtitle("") +
    theme_bw()+
    theme(axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

q3=ggplot(fear, aes(x = reorder(Code, -Percentage), y = Percentage)) +
         geom_bar(stat = "identity", fill="lightgray") +
  coord_flip() +
  ggtitle("")+
  theme_bw()+
    theme(axis.text.y=element_blank(),
        axis.title.y=element_blank()) 

q1
q2
q2b
q3

library("cowplot")
p=ggdraw() +
  draw_plot(q1, x = 0, y = 0.75, width = 1, height = 0.25) +
  draw_plot(q2, x = 0, y = 0.50, width = 1, height = 0.25) +
  draw_plot(q2b, x = 0, y = 0.25, width = 1, height = 0.25) +
  draw_plot(q3, x = 0, y = 0, width = 1, height = 0.25) 
p
   
  
p2= p + theme(plot.margin=unit(c(10,10,10,10),"mm")) 
p2
ggsave(p2, file="publication.image.png", units="in", width=3, height=6, dpi=600)

p4=ggplot(dat, aes(x=IA_importance, fill=IA_importance))+
  geom_density(alpha=0.7, fill="turquoise4") +
  ylab("Density") +
  xlab("Scale (1-10)") +
  theme_bw() +
  theme(axis.title = element_text(face="bold") )

p4
ggsave(p4, file="density.png", units="in", width=4, height=3, dpi=600)


p5=ggplot(dat, aes(x=IA_should_be, fill=IA_should_be))+
  geom_density(alpha=0.7, fill="turquoise4") +
  ylab("Density") +
  xlab("Percentage") +
  theme_bw() +
  theme(axis.title = element_text(face="bold") )

p5
ggsave(p5, file="density2.png", units="in", width=4, height=3, dpi=600)


# the frequency at which instructors felt IA should be incorporated increased as the percentage of their classes that were lower division decreased

p6=ggplot(dat, aes(x=IA_should_be, y=Perc_lowerdiv)) + 
  geom_point(alpha=0.4) +
  geom_smooth(data=dat, aes(x=IA_should_be, y = Perc_lowerdiv), method=lm, color="turquoise4") +
  theme(legend.position="none") +
  ylim(0,100) +
  theme_bw() 
ggsave(p6, file="cor1.png", units="in", width=5, height=2, dpi=600)


#instructors who reported increased value of IA topics were also more likely to report it was the instructor’s responsibility (rather than the students’) to explicitly link those topics to class content

p7=ggplot(dat, aes(x=IA_should_be, y=explicit_link)) + 
  geom_point(alpha=0.4) +
  geom_smooth(data=dat, aes(x=IA_should_be, y = explicit_link), method=lm, color="turquoise4") +
  theme(legend.position="none") +
  theme_bw() 
ggsave(p7, file="cor2.png", units="in", width=5, height=2, dpi=600)



p10=ggplot(dat, aes(x=IA_importance, y=comfort.average)) + 
  geom_point(alpha=0.4) +
  geom_smooth(data=dat, aes(x=IA_importance, y = comfort.average), method=lm, color="turquoise4") +
  theme(legend.position="none") +
  theme_bw() 

p10

ggsave(p10, file="cor3.png", units="in", width=5, height=2, dpi=600)

p11=ggplot(dat, aes(x=IA_importance, y=explicit_link)) + 
  geom_point(alpha=0.4) +
  geom_smooth(data=dat, aes(x=IA_importance, y = explicit_link), method=lm, color="turquoise4") +
  theme(legend.position="none") +
  theme_bw() 

p11

ggsave(p11, file="cor4.png", units="in", width=5, height=2, dpi=600)
```


```{r}

library(ggpubr)

"gray67", "gray29"

often.pl=ggdotchart(often, y = "Percentage", x = "Code",
           color = "Subquestion",     # Color by groups
           palette = c("rarely_never" = "gray67",
                                "some_always" = "gray29"), # Custom color palette
           sorting = "ascending",  # Sort value in descending order
           rotate = TRUE,  # Rotate vertically
           dot.size = 7,    # Large dot size'
          label = round(often$Percentage,1), repel=T, # Add dot labels
          label.rectangle=T,
          font.label = list(color = "Subquestion", size = 10, face="bold", vjust=-1.5), 
           ggtheme = theme_pubr()) +
  ylab("Percentage") +
  ylim(0,55) +
  ggtitle("TITLE") +
    theme(plot.title = element_text(hjust = 0.5, size=14, face="bold")) +
   theme(axis.title.y=element_text(face="bold"),
            strip.text=element_text(face="bold", size=12),
            legend.title =element_text(size=10),
         axis.title.x =element_text(face="bold")) +
    theme_cleveland() 
ggsave(often.pl, file="often_bubplot.png", width=11, height=6, dpi=600)

```
```{r}
mean(dat$Perc_lowerdiv)
mean(dat$Perc_upperdiv)
min(dat$Perc_lowerdiv)
max(dat$Perc_upperdiv)
```

