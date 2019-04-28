
###1. Prepare data
####read data in
```{r}
fifa_cleaned <- read.csv("~/Desktop/SPRING 19/R/final project/fifa_cleaned.csv")
```


####loading packages
```{r results='hide', message=FALSE, warning=FALSE}
library(ggplot2)
library(tidyverse)
library(scales)
library(ggthemes)
library(kableExtra)
library(plotly)
library(stringr)
library(ggrepel)
library(ellipsis)
```

####Let's take a look at some famous players and their values
```{r}
fifa_cleaned$Value = as.numeric(fifa_cleaned$Value)
fifa_cleaned$Value[fifa_cleaned$Name == "Neymar Jr"]
fifa_cleaned$Value[fifa_cleaned$Name == "L. Messi"]
fifa_cleaned$Value[fifa_cleaned$Name == "E. Hazard"]
fifa_cleaned$Value[fifa_cleaned$Name == "P. Dybala"]
summary(fifa_cleaned$Value)
```



###2. Exploring top players

#####Finding top 10 players and their clubs whose value is highest

```{r fig.width = 10, fig.height=5}
fifa_cleaned %>%
  select(Name,Value,Club) %>%
  group_by(Club) %>%
  filter(Value==max(Value)) %>%
  arrange(-Value) %>%
  head(10) %>%
  ggplot(aes(x=Name,y=Value,fill=factor(Club)))+geom_bar(stat='identity') + 
  labs(x='Player Name',y='Value',title='Top 10 most valuable players & their respective clubs')
```




###Let's look at some Superstars in FIFA 

#####we consider playerw with overall rating over 85 as a superstart
```{r echo=FALSE}
fifa_cleaned %>%
  mutate(Superstar = ifelse(Overall> 86, "Superstar","Non - Superstar"))%>%
  group_by(Club)%>%
  filter(Superstar=="Superstar")%>%
  summarise(Player.Count = n())%>%
  arrange(-Player.Count)%>%
  ggplot(aes(x = as.factor(Club) %>%
               fct_reorder(Player.Count), y = Player.Count, label = Player.Count))+
  geom_text(hjust = 0.01,inherit.aes = T, position = "identity")+
  geom_bar(stat = "identity", fill = "pink")+
  coord_flip()+
  xlab("Club")+
  ylab("Number of Superstars")
```


###3. Exploring the overall ratings

#####Plotting the distribution of overall ratings
```{r}
fifa_cleaned %>%
  ggplot(aes(x= Overall)) +
  geom_histogram(color = "white", fill = I("blue") , alpha=I(.4))+
  ggtitle("Player Ratings Are Normally Distributed") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5))

```


###Age vs Overall Rating

#####frist need to group different positions
```{r}
positions <- unique(fifa_cleaned$Position)


gk <- "GK"
defs <- positions[str_detect(positions, "B$")]
mids <- positions[str_detect(positions, "M$")]
f1 <- positions[str_detect(positions, "F$")]
f2 <- positions[str_detect(positions, "S$")]
f3 <- positions[str_detect(positions, "T$")]
f4 <- positions[str_detect(positions, "W$")]
fwds <- c(as.character(f1), as.character(f2), as.character(f3), as.character(f4))



fifa_cleaned <- fifa_cleaned %>% 
  mutate(PositionGroup = ifelse(Position %in% gk, "GK", ifelse(Position %in% defs, "DEF", ifelse(Position %in% mids, "MID", ifelse(Position %in% fwds, "FWD", "Unknown")))))
```

```{r}
fifa_cleaned %>%
  filter(!PositionGroup %in% c("GK", "Unknown")) %>%
  group_by(Age) %>%
  summarise(Rating = mean(Overall)) %>%
  ggplot(aes(x= Age, y= Rating, group = 1)) +
  geom_line(color = "darkblue",alpha = 0.6, size = 1) +
  ggtitle("The Age Curve Flattens Off") +
  xlab("Age") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_text(), axis.title.y = element_blank(), axis.title.x = element_text(face = "bold"))
```
<br />Player ratings tend not to get better after the age of 30



###Age vs Overall rating by positions

```{r}
fifa_cleaned %>%
  filter(!PositionGroup %in% c("GK", "Unknown")) %>%
  group_by(PositionGroup, Age) %>%
  summarise(Rating = mean(Overall)) %>%
  ggplot(aes(x= Age, y= Rating, group = PositionGroup)) +
  geom_line(size = 1, color = "darkblue",alpha = 0.6) +
  theme_fivethirtyeight() +
  facet_wrap(~ PositionGroup, ncol = 1) +
  theme(strip.background = element_rect(fill = "darkgrey"), strip.text = element_text(colour = "white", face = "bold"))
```
<br />By looking at the above plot, we notice that the decline for defenders’ ratings starts earliest at around 33 years of age, and the decline for both attackers and midfielders starts around 35 years of age.


###Is player jersey number related to Overall ratings?
```{r echo=FALSE, warning=FALSE}
fifa_cleaned %>%
  group_by(Jersey.Number) %>%
  summarise(Avg.Overall = sum(Overall)/length(Jersey.Number),
            Player.Count = sum(Jersey.Number))%>%
  arrange(-Avg.Overall)%>%
  ggplot(aes(x = Jersey.Number, y = Avg.Overall,col = ifelse(Avg.Overall < 70,"darkgrey", "Red")))+
  geom_point(position = "jitter")+
  theme(legend.position = "none")+
  geom_text_repel(aes(label = ifelse(Avg.Overall > 70, Jersey.Number, "")))
```
<br />Jersey No 79 is an outlier with only two players making up the overall average. Number 10 appears to be the most popular number that has been awarded to the best player in the teams.



###4. Exploring the players values

####Plotting players values and we can look at the outliers from the plot
```{r}
p <- fifa_cleaned %>%
  ggplot(aes(x= Value)) +
  geom_histogram(color = "white", fill = "darkgrey") +
  scale_x_continuous(labels = dollar_format(prefix = "€")) +
  ggtitle("Player Values Are Heavily Skewed to the Right", subtitle = "There are some extreme outliers") +
  theme_fivethirtyeight()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))

p +
  geom_text(data = subset(fifa_cleaned, Name == "Neymar Jr"), aes(x= Value, y= 1300, label=Name), color = "red") +
  geom_text(data = subset(fifa_cleaned, Name == "L. Messi"), aes(x= Value, y= 2000, label=Name), color = "red") +
  geom_text(data = subset(fifa_cleaned, Name == "K. De Bruyne"), aes(x= Value, y= 500, label=Name), color = "red") +
  geom_text(data = subset(fifa_cleaned, Name == "E. Hazard"), aes(x= Value, y= 1300, label=Name), color = "red") +
  geom_text(data = subset(fifa_cleaned, Name == "P. Dybala"), aes(x= Value, y= 2000, label=Name), color = "red")

```



###Plotting players age vs players values
#####First need to group players by age
```{r}
fifa_cleaned <- fifa_cleaned %>%
  mutate(AgeGroup = ifelse(Age <= 20, "20 and under", ifelse(Age > 20 & Age <=25, "21 to 25", ifelse(Age > 25 & Age <= 30, "25 to 30", ifelse(Age > 30 & Age <= 35, "31 to 35", "Over 35")))))
```

```{r}
fifa_cleaned %>%
  ggplot(aes(x= AgeGroup, y= Value)) +
  geom_boxplot(fill="#4271AE") +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  ggtitle("Players Are In High Demand In Their Mid-20s") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_fivethirtyeight()
```




###Plotting players positions vs players values

```{r,fig.width = 8, fig.height=8}
a <- fifa_cleaned %>%
  filter(PositionGroup != "Unknown") %>%
  ggplot(aes(x= PositionGroup, y= Value)) +
  geom_boxplot(fill = "blue", alpha=0.4) +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  ggtitle("Positions vs Values") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_fivethirtyeight()



b <- fifa_cleaned %>%
  filter(PositionGroup != "Unknown") %>%
  ggplot(aes(x= Position, y= Value))+
  geom_boxplot(fill= "pink",alpha=0.8)+
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  coord_flip() +
  theme_fivethirtyeight() +
  facet_wrap(~ PositionGroup, scales = "free") +
  theme(strip.background = element_rect(fill = "darkgrey"), strip.text = element_text(colour = "white", face = "bold"))



gridExtra::grid.arrange(a, b)
```
<br />As we expected, Forwards and Midfielders are going to cost you more than Defenders and Goalkeepers.



###5. Exploring the relationship between Ratings and Value

#### Comparing Overall ratings with Value of players
```{r}
ggplot(fifa_cleaned,aes(x=Overall,y=Value)) + geom_point(alpha=0.3) + 
  labs(x='Overall Rating',y='Value',title='Comparing Market Value of Players with Overall Rating')
```  


###6. Exploring player attributes


####Now let's compare the percentage of different attributes between two players

#####first we need to group similar attributes
```{r}
fifa_enhanced  <- fifa_cleaned %>% mutate(totscore = Crossing + Finishing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + Curve + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Penalties + Composure + Marking + StandingTackle + SlidingTackle) %>%
  mutate(percross = Crossing / totscore, perfin = Finishing / totscore, perHeading = HeadingAccuracy/totscore, perSP  = ShortPassing/totscore, pervol = Volleys/totscore, perdrib = Dribbling/totscore, perCur = Curve/totscore, perFKA = FKAccuracy/totscore, perLP = LongPassing/totscore, perBallCont = BallControl / totscore, perAcc = Acceleration / totscore, perSprSpe = SprintSpeed / totscore, perAg = Agility / totscore, perRec = Reactions/totscore, perbal = Balance/totscore, perShotP = ShotPower/totscore, perJump = Jumping/totscore, perstam = Stamina/totscore, perStren = Strength/totscore, perLS = LongShots / totscore, perAgr = Aggression/totscore, perInts = Interceptions/totscore, perPos = Positioning/totscore, perVis = Vision/totscore, perPen = Penalties /totscore, perCom = Composure/totscore, perMark = Marking/totscore, perStandT = StandingTackle/totscore, perSlideT = SlidingTackle/totscore)
```  


```{r}
selectedplay<-fifa_enhanced %>% filter(Name %in% c("L. Messi","B. Gibson")) %>%
  select(Name, percross, perfin, perHeading, perSP, pervol, perdrib, perCur, perFKA, perLP, perBallCont, perAcc, perSprSpe, perAg, perRec, perbal, perShotP, perJump, perstam, perStren, perLS, perAgr, perInts, perPos, perVis, perPen, perCom, perMark, perStandT, perSlideT) 
```

```{r}
colnames(selectedplay)[2]  <- "Crossing"                                           
colnames(selectedplay)[3]  <- "Finishing"
colnames(selectedplay)[4]  <- "Heading Accuracy"
colnames(selectedplay)[5]  <- "Short Passing"
colnames(selectedplay)[6]  <- "Volleys"
colnames(selectedplay)[7]  <- "Dribbling"
colnames(selectedplay)[8]  <- "Curve"
colnames(selectedplay)[9]  <- "FKAccuracy"
colnames(selectedplay)[10]  <- "Long Passing"
colnames(selectedplay)[11]  <- "Ball Control"
colnames(selectedplay)[12]  <- "Acceleration"
colnames(selectedplay)[13]  <- "Sprint Speed"
colnames(selectedplay)[14]  <- "Agility"
colnames(selectedplay)[15]  <- "Reactions"
colnames(selectedplay)[16]  <- "Balance"
colnames(selectedplay)[17]  <- "Shot Power"
colnames(selectedplay)[18]  <- "Jumping"
colnames(selectedplay)[19]  <- "Stamina"
colnames(selectedplay)[20]  <- "Strength"
colnames(selectedplay)[21]  <- "Long Shots"
colnames(selectedplay)[22]  <- "Aggression"
colnames(selectedplay)[23]  <- "Interceptions"
colnames(selectedplay)[24]  <- "Positioning"
colnames(selectedplay)[25]  <- "Vision"   
colnames(selectedplay)[26]  <- "Penalties"
colnames(selectedplay)[27]  <- "Composure"
colnames(selectedplay)[28]  <- "Marking"
colnames(selectedplay)[29]  <- "Standing Tackle"
colnames(selectedplay)[30]  <- "Sliding Tackle"
```

```{r}
selectedplay2 <- selectedplay %>% gather("attribute", "value", -Name)

cols2 <- c("B. Gibson" = "purple", "L. Messi" = "lightgreen")

ggplot(selectedplay2, aes(x = attribute, y = value, fill = Name)) + 
  geom_col() + 
  coord_flip() + 
  facet_wrap(~Name) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = cols2) +
  theme(panel.background = element_blank())
```


###Positions VS Attributes
#####Analyzing the median ratings for each of the attributes for \neach position for players with and overall rating over 75
```{r,fig.width = 12, fig.height=10}
tile_data <- fifa_cleaned %>%
  select_if(is.numeric) %>%
  left_join(fifa_cleaned %>% select(ID, Position, PositionGroup), by = "ID") %>%
  filter(Overall >=75) %>%
  select(-ID, -Special, -Age, -Overall, -Potential, -starts_with("Wage"),-LS,-ST,-RS,-LW,-LF,-CF,-RF,-RW,-LAM,-CAM,-RAM,-LM,-LCM,-CM,-RCM,-RM,-LWB,-LDM,-CDM,-RDM,-RWB,-LB,-LCB,-CB,-RCB,-RB,-Contract.Valid.Until,-GKDiving,-GKHandling,-GKKicking,-GKPositioning,-GKReflexes,-International.Reputation,-Jersey.Number,-Release.Clause,-Skill.Moves,-Weak.Foot,-Weight,-Height,-Value)


tile_data <- tile_data %>%
  filter(Position != "GK") %>%
  gather(key = Attribute, value = Value, -Position, -PositionGroup) %>%
  group_by(PositionGroup, Position, Attribute) %>%
  summarise(MedianValue = median(Value, na.rm = T))

tile_data %>%
  filter(Position != "Unknown") %>%
  ggplot(aes(x= Attribute, y= Position)) +
  geom_tile(aes(fill = MedianValue), colour = "black") +
  geom_text(aes(label = MedianValue)) +
  scale_fill_gradient(low = "blue", high = "green") +
  ggtitle("Center Backs Are Strong, Left Forwards Are Agile") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(face = "bold", size = 12), legend.position = "none") +
  facet_wrap(~ PositionGroup, scales = "free", ncol = 1)
```
<br />By drawing a heat map of players with an overall rating of 75 or higher other than goalkeepers, we found that Right and Left Wingers has great acceleration and aggression, center backs are good at strength, and Left and Right Midfielders are agile. 
<br />After doing this exploration, we start to think about building a predictive model to help team manager arrange the most suitable position for each player.









