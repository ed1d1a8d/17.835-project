library(ggplot2)
library(readxl)

###########load primary data
load("SCDB_2017_01_caseCentered_Citation.Rdata")
load("scdbSalience.rdata")

salience_8_data <- read_excel("CSI-1953-2014.xls") #8 news sources
case_data <-SCDB_2017_01_caseCentered_Citation  #complete data about cases
salience_2_data <- scdbSalience #2 news sources

###########create salience_data and absolute_diff
salience_data <- merge(salience_8_data,salience_2_data[c("caseId","cqSalience")]) #augmented 9 news source salience measure
salience_data$cqSalience <- salience_data$cqSalience * 2 # match cqSalience measure with other news sources
salience_data$CSI <- salience_data$CSI + salience_data$cqSalience #include cqSalience in CSI measurement

maj_min_vote <- subset(case_data, select = c(majVotes, minVotes))
sum <- Reduce(function (x,y) x + y , maj_min_vote) # total votes
diff <- Reduce(function (x,y) x - y , maj_min_vote)# difference in majority and minority votes for a case
abs_diff <- abs(diff)
vote_pattern = data.frame(term=case_data$term, caseId = case_data$caseId, absVoteDifference = abs_diff, totalVotes = sum) #term,caseId,voteDifference,totalVotes

###########final data_frame
total <- merge(vote_pattern,salience_data) #case_data and salience_data
total_8 <- merge(vote_pattern,salience_8_data) #case_data and salience_8_data

dem_years <- c(1937:1952,1961:1968, 1977:1980, 1993:2000, 2009:2015)
years <- c(1953,2010)
years_8 <- c(1953:2014)

total_8_per_yr <- Map(function(x) total_8[total_8$term == x, ],years_8)#spit total_8 to years
CSI_8_per_yr <- unlist(unlist(Map(function(total_8_per_yr) mean(total_8_per_yr$CSI), total_8_per_yr)))#mean CSI_8 per year
abs_diff_8_per_yr<- unlist(unlist(Map(function(total_8_per_yr) mean(total_8_per_yr$absVoteDifference), total_8_per_yr)))#mean absDiff_8 per year

final_8_data <- data.frame(year = years_8 , salience = CSI_8_per_yr, voteRecord = abs_diff_8_per_yr) #final for 8 news sources

###########v
mean_votes = mean(total_8$totalVotes)
total_8$corr_abs_vote_diff <- total_8$absVoteDifference * sqrt(total_8$totalVotes / mean_votes) #correct for difference in totalvotes

###########analysis
sum(total_8$absVoteDifference  > total_8$totalVotes - 1)/length(total_8$absVoteDifference)
cor.test(total_8$absVoteDifference, total_8$CSI)
cor(final_8_data)

###########visualization
# ggplot(data=total_8, aes(corr_abs_vote_diff,fill=CSI))+
#   geom_histogram(bins = 40)
#
# sum(total_8$CSI == 0)
#
# ggplot(data=total_8, aes(x=totalVotes))+
#   geom_histogram()
# theme_bw()
#
# ggplot(data=total_8, aes(x=CSI))+
#   geom_histogram()
#   theme_bw()
#
#   ggplot(data=total_8, aes(x=absVoteDifference))+
#     geom_histogram()
#   theme_bw()
#
#   ggplot() +
#     geom_bar(data=total_8, aes(x=absVoteDifference, color = CSI, fill=CSI))+
#     guides(fill=guide_legend(title="Vote Difference")) +
#     xlab("Salience") + ylab("Number of cases") +
#     ggtitle("Cumulative Vote Difference of Cases by Salience")+
#     theme_bw()
#
#   ggplot() +
#     geom_bar(data=total_8, aes(x=CSI, y=corr_abs_vote_diff, fill=corr_abs_vote_diff),stat = "identity")+
#     guides(fill=guide_legend(title="Voting Pattern")) +
#     xlab("Salience") + ylab("Number of cases") +
#     ggtitle("Cumulative Vote Difference of Cases by Salience")+
#     theme_bw()
#
###visualization slaiency vs vote margin
ggplot() +
  geom_jitter(data=total_8, aes(x=CSI, y = absVoteDifference, color=totalVotes))+
  labs(title="",x="Saliency", y="Vote margin", colour="Total votes")+
  scale_y_continuous(breaks =seq(0,9,1),limits=c(-0.5,9.5)) +
  scale_x_continuous(breaks =seq(0,9,1),limits=c(-0.5,8.5))  +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
  )

#visulaization incumbent party
ggplot(data=final_8_data, aes(x=year, y=salience, fill=salience)) +
  scale_y_continuous(breaks =seq(0,9,1),limits=c(-0.5,8.5)) +
  scale_x_continuous(breaks =seq(1950,2017,10),limits=c(1950,2017))  +
  geom_line(colour="black", stat="identity")+
  labs(x="Year", y= "Saliency")+
  guides(fill=FALSE)+
  geom_rect(aes(xmin = 1952, xmax = 1961, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 1961, xmax = 1968, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  geom_rect(aes(xmin = 1968, xmax = 1977, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 1977, xmax = 1980, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  geom_rect(aes(xmin = 1980, xmax = 1993, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 1993, xmax = 2000, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  geom_rect(aes(xmin = 2000, xmax = 2009, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 2009, xmax = 2015, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  ggtitle("Average Saliency of Cases per Year")+

  theme_bw()

ggplot(data=final_8_data, aes(x=year, y=voteRecord, fill=voteRecord)) +
  scale_y_continuous(breaks =seq(0,9,1),limits=c(-0.5,8.5)) +
  scale_x_continuous(breaks =seq(1950,2017,10),limits=c(1950,2017))  +
  geom_line(colour="black", stat="identity")+
  labs(x="Year", y= "Vote Margin")+
  guides(fill=FALSE)+
  geom_rect(aes(xmin = 1952, xmax = 1961, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 1961, xmax = 1968, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  geom_rect(aes(xmin = 1968, xmax = 1977, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 1977, xmax = 1980, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  geom_rect(aes(xmin = 1980, xmax = 1993, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 1993, xmax = 2000, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  geom_rect(aes(xmin = 2000, xmax = 2009, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 2009, xmax = 2015, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  ggtitle("Average Vote margin of Cases per Year")+
  theme_bw()

library(reshape2)
final_8_data.long<-melt(final_8_data,id.vars="year")

ggplot(data=final_8_data.long, aes(year,value , fill=variable)) +
  scale_y_continuous(breaks =seq(0,9,1),limits=c(-0.5,8.5)) +
  scale_x_continuous(breaks =seq(1950,2017,10),limits=c(1950,2017))  +
  geom_line(colour="black", stat="identity")+
  labs(x="Year", y= "Measure")+
  geom_rect(aes(xmin = 1952, xmax = 1961, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 1961, xmax = 1968, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  geom_rect(aes(xmin = 1968, xmax = 1977, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 1977, xmax = 1980, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  geom_rect(aes(xmin = 1980, xmax = 1993, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 1993, xmax = 2000, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  geom_rect(aes(xmin = 2000, xmax = 2009, ymin = -0, ymax = Inf),
            fill = "red", alpha = 0.002)+
  geom_rect(aes(xmin = 2009, xmax = 2015, ymin = -0, ymax = Inf),
            fill = "blue", alpha = 0.002)+
  ggtitle("Average vote margin and salience of Cases per Year")+
  theme_bw()
