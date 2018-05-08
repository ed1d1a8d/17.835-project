library(ggplot2)
library(readxl)
###########load data
load("SCDB_2017_01_caseCentered_Citation.Rdata") 
load("scdbSalience.rdata")

CSI_data<- read_excel("CSI-1953-2014.xls")
case_data <-SCDB_2017_01_caseCentered_Citation
salience <- scdbSalience


#################define interesting values on case_data
maj_min_vote <- subset(case_data, select = c(majVotes, minVotes))
avg_salience <- 1/2 *(salience$cqSalience + salience$nytSalience) #decent model for saliency by avging (2)
sum <- Reduce(function (x,y) x + y , maj_min_vote)
diff <- Reduce(function (x,y) x - y , maj_min_vote)#1
absloute_diff <- abs(diff)



#########for case_data and salience
# #length(case_data$caseId[case_data$splitVote == 1])
# ind1 = 1 #diff, absloute_idff, maj_min_vote, sum, case_data
# ind2 = 1 #salience, avg_salience

# caseId_filtered = c()
# diff_filtered = c()
# absloute_diff_filtered = c()
# term_filtered = c()

# cq_filtered=c()
# ny_filtered=c()
# avg_salience_filtered = c()

# #m = unlist(Map(function(x) Reduce(function(j,k) j ||k, salience$caseId == xFilter(function(y) y==x, salience$caseId[1:10])),case_data$caseId[1:10]))
# #m =  unlist(Map(function(x) Reduce(function(j,k) j ||k, salience$caseId == x),case_data$caseId[1:1000]))
# 
# #for case_data and salience
# while(ind1 <= length(case_data$caseId) && ind2 <= length(salience$caseId)){
#   while(case_data$caseId[ind1] < salience$caseId[ind2] && ind1 <= length(case_data$caseId)){
#      ind1 = ind1 + 1
#   }
#   while(case_data$caseId[ind1] > salience$caseId[ind2] && ind2 <= length(salience$caseId)){
#         ind2 = ind2 + 1
#   }
#   if(case_data$caseId[ind1] == salience$caseId[ind2]){
#     caseId_filtered = append(caseId_filtered, case_data$caseId[ind1])
#     diff_filtered = append(diff_filtered, diff[ind1])
#     absloute_diff_filtered  = append(absloute_diff_filtered, absloute_diff[ind1])
#     term_filtered = append(term_filtered, case_data$term[ind1])
#     
#     cq_filtered=append(cq_filtered,salience$cqSalience[ind2])
#     ny_filtered=append(ny_filtered,salience$nytSalience[ind2])
#     avg_salience_filtered = append(avg_salience_filtered, avg_salience[ind2])
#     ind1 = ind1 + 1
#     ind2 = ind2 + 1
#   }
# }
# 
# #case_data_filtered <- data.frame(caseId_filtered, )
# total_filtered <- data.frame(caseId_filtered, cq_filtered,  ny_filtered, absloute_diff_filtered, term_filtered)
# 
# df1 <- data.frame(caseId = caseId_filtered, salience= avg_salience_filtered, vote_diff = diff_filtered)
# df2 <- data.frame(caseId = salience$caseId, salience = avg_salience)
# df3 <- data.frame(nyt = salience$cqSalience, cq = salience$nytSalience )
# 
# vote_year 
# 
# #334233#3## 
# dem_years <- c(1937:1952,1961:1968, 1977:1980, 1993:2000, 2009:2015)
# years <- c(1946:2009) 
# 
# total_per_year <- Map(function(x) total_filtered[total_filtered$term_filtered == x, 1:ncol(total_filtered)],years)
# vote_year <- unlist(unlist(Map(function(per_year) mean(per_year$cq_filtered), total_per_year)))
# 
# vote_cq_0 <- case_data_filtered[salience_filtered$cq_filtered == 0]
# vote_cq_1 <- case_data_filtered[salience_filtered$cq_filtered == 1]
# vote_ny_0 <- case_data_filtered[salience_filtered$ny_filtered == 0]
# vote_ny_1 <- case_data_filtered[salience_filtered$ny_filtered == 1]
# 
# final_vote <- data.frame(year = years, salience = vote_year)
# ggplot(data=final_vote, aes(x=year, y=salience, fill=salience)) +
#   geom_bar(stat="identity")
# 
# 
# #avg_vote_cq_0 <- Reduce(function(x) term_filtered[Position(function(y) y=x), ]))
# mean_diff_0 = mean(absloute_diff_filtered[avg_salience_filtered==0])
# mean_diff_0.5 = mean(absloute_diff_filtered[avg_salience_filtered==0.5])
# mean_diff_1 = mean(absloute_diff_filtered[avg_salience_filtered==1])
# 
# dat <- data.frame(
#   salience = c(0,0.5,1),
#   vote_record = c(mean_diff_0, mean_diff_0.5, mean_diff_1)
# )
# ggplot(data=dat, aes(x=salience, y=vote_record, fill=salience)) +
#   geom_line()




#for case_data and (this is the CSI metric)
#length(case_data$caseId[case_data$splitVote == 1])
ind1 = 1 #diff, absloute_idff, maj_min_vote, sum, case_data
ind2 = 1 #salience, avg_salience
caseId_filtered_CSI = c()
diff_filtered_CSI = c()
absloute_diff_filtered_CSI = c()
term_filtered_CSI = c()

filtered_CSI=c()
ny_filtered_CSI=c()

while(ind1 <= length(case_data$caseId) && ind2 <= length(CSI_data$caseId)){
while(case_data$caseId[ind1] < CSI_data$caseId[ind2] && ind1 <= length(case_data$caseId)){
  ind1 = ind1 + 1
}
while(case_data$caseId[ind1] > CSI_data$caseId[ind2] && ind2 <= length(CSI_data$caseId)){
  ind2 = ind2 + 1
}
if(case_data$caseId[ind1] == CSI_data$caseId[ind2]){
  caseId_filtered_CSI = append(caseId_filtered_CSI, case_data$caseId[ind1])
  diff_filtered_CSI = append(diff_filtered_CSI, diff[ind1])
  absloute_diff_filtered_CSI  = append(absloute_diff_filtered_CSI, absloute_diff[ind1])
  term_filtered_CSI = append(term_filtered_CSI, case_data$term[ind1])
  
  filtered_CSI =append(filtered_CSI,CSI_data$CSI[ind2])
  ny_filtered_CSI =append(ny_filtered_CSI,CSI_data$nyScore[ind2])
  ind1 = ind1 + 1
  ind2 = ind2 + 1
}
}
total_filtered_CSI <- data.frame(absloute_diff_filtered_CSI, term_filtered_CSI, filtered_CSI)

#334233#3## 
dem_years <- c(1937:1952,1961:1968, 1977:1980, 1993:2000, 2009:2015)
years <- c(1953:2014) 

total_per_year_CSI <- Map(function(x) total_filtered_CSI[total_filtered_CSI$term_filtered == x, 1:ncol(total_filtered_CSI)],years)
filtered_per_year_CSI <- unlist(unlist(Map(function(per_year) mean(per_year$filtered_CSI), total_per_year_CSI)))
absloute_diff_filtered_per_year_CSI <- unlist(unlist(Map(function(per_year) mean(per_year$absloute_diff_filtered_CSI), total_per_year_CSI)))

final_vote_salience_CSI <- data.frame(year = years, salience = filtered_per_year_CSI, voteRecord = absloute_diff_filtered_per_year_CSI)

ggplot(data=final_vote_salience_CSI, aes(x=year, y=salience, fill=salience)) +
  geom_bar(colour="black", stat="identity",  position=position_dodge(),
           size=.3, fill = "white")+ 
  xlab("Year") + ylab("Salience") +
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
  ggtitle("Average Salience of Cases per Year")+
  theme_bw()

ggplot(data=final_vote_salience_CSI, aes(x=year, y=voteRecord, fill=voteRecord)) +
  geom_bar(colour="black", stat="identity",  position=position_dodge(),
           size=.3, fill = "white")+ 
  guides(fill=guide_legend(title="vote Difference")) + 
  xlab("Year") + ylab("vote Difference") +
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
  ggtitle("Average vote Difference of Cases per Year")+
  theme_bw()
# 
# library(reshape2)
# final_vote_salience_CSI.long<-melt(final_vote_salience_CSI,id.vars="year")
# 
# ggplot(data=final_vote_salience_CSI.long, aes(year,value , fill=variable)) +
#   geom_bar(colour="black", stat="identity",  position=position_dodge(),
#            size=.3)+
#   geom_rect(aes(xmin = 1952, xmax = 1961, ymin = -0, ymax = Inf),
#             fill = "red", alpha = 0.002)+
#   geom_rect(aes(xmin = 1961, xmax = 1968, ymin = -0, ymax = Inf),
#             fill = "blue", alpha = 0.002)+
#   geom_rect(aes(xmin = 1968, xmax = 1977, ymin = -0, ymax = Inf),
#             fill = "red", alpha = 0.002)+
#   geom_rect(aes(xmin = 1977, xmax = 1980, ymin = -0, ymax = Inf),
#             fill = "blue", alpha = 0.002)+
#   geom_rect(aes(xmin = 1980, xmax = 1993, ymin = -0, ymax = Inf),
#             fill = "red", alpha = 0.002)+
#   geom_rect(aes(xmin = 1993, xmax = 2000, ymin = -0, ymax = Inf),
#             fill = "blue", alpha = 0.002)+
#   geom_rect(aes(xmin = 2000, xmax = 2009, ymin = -0, ymax = Inf),
#             fill = "red", alpha = 0.002)+
#   geom_rect(aes(xmin = 2009, xmax = 2015, ymin = -0, ymax = Inf),
#             fill = "blue", alpha = 0.002)+
#   ggtitle("Average vote Difference of Cases per Year")+
#   theme_bw()

ggplot(data=total_filtered_CSI, aes(x=filtered_CSI, y=absloute_diff_filtered_CSI, fill=absloute_diff_filtered_CSI)) +
  geom_bar(stat="identity")+
  guides(fill=guide_legend(title="Vote Difference")) + 
  xlab("Salience") + ylab("Number of cases") +
  ggtitle("Cumulative Vote Difference of Cases by Salience")+
  theme_bw()

# geom_rect(aes(xmin = c(1961,1977), xmax = c(1968, 1980), ymin = c(-Inf,-Inf), ymax = c(Inf,Inf)),
#           fill = "pink", alpha = 0.03)+
#   geom_vline(xintercept=c( 1961, 1968, 1977, 1980, 1993, 2000, 2009))+
#   geom_vline(xintercept=c(1952, 1960, 1969, 1976, 1981,1992,2001,2008))+
#########Look at later
vote_cq_0 <- case_data_filtered[salience_filtered$cq_filtered == 0]
vote_cq_1 <- case_data_filtered[salience_filtered$cq_filtered == 1]
vote_ny_0 <- case_data_filtered[salience_filtered$ny_filtered == 0]
vote_ny_1 <- case_data_filtered[salience_filtered$ny_filtered == 1]

#avg_vote_cq_0 <- Reduce(function(x) term_filtered[Position(function(y) y=x), ]))
mean_diff_0 = mean(absloute_diff_filtered[avg_salience_filtered==0])
mean_diff_0.5 = mean(absloute_diff_filtered[avg_salience_filtered==0.5])
mean_diff_1 = mean(absloute_diff_filtered[avg_salience_filtered==1])

dat <- data.frame(
  salience = c(0,0.5,1),
  vote_record = c(mean_diff_0, mean_diff_0.5, mean_diff_1)
)
ggplot(data=dat, aes(x=salience, y=vote_record, fill=salience)) +
  geom_line()

#3333#####
sum(salience$nytSalience[salience$cqSalience == 0])/length(salience$nytSalience[salience$cqSalience == 0])
sum(salience$nytSalience[salience$cqSalience == 1])/length(salience$nytSalience[salience$cqSalience == 1])

sum(salience$cqSalience[salience$nytSalience == 0])/length(salience$cqSalience[salience$nytSalience == 0])
sum(salience$cqSalience[salience$nytSalience == 1])/length(salience$cqSalience[salience$nytSalience == 1])



# hist(salience$nytSalience[salience$cqSalience == 0], 
#      main="salience$nyt[salience$cq == 0", 
#      col="red",
#      breaks=50,
#      ylim=c(0, 8000),
#      xlim = c(0,1),
#      xlab='Ideal Points');
# hist(salience$nytSalience[salience$cqSalience == 1], 
#      main="salience$nyt[salience$cq == 1", 
#      col="red",
#      breaks=50,
#      ylim=c(0, 500),
#      xlim = c(0,1),
#      xlab='Ideal Points');
# 
# hist(salience$cqSalience[salience$nytSalience == 0], 
#      main="salience$cq[salience$nyt == 0]", 
#      col="red",
#      breaks=50,
#      ylim=c(0, 8000),
#      xlim = c(0,1),
#      xlab='Ideal Points');
# 
# hist(salience$cqSalience[salience$nytSalience == 1], 
#      main="salience$cq[salience$nyt == 1", 
#      col="red",
#      breaks=50,
#      ylim=c(0, 800),
#      xlim = c(0,1),
#      xlab='Ideal Points');


# ggplot(data=chol, aes(chol$AGE)) + 
#   geom_histogram(breaks=seq(20, 50, by = 2), 
#                  col="red", 
#                  fill="green", 
#                  alpha = .2) + 
#   labs(title="Histogram for Age") +
#   labs(x="Age", y="Count") + 
#   xlim(c(18,52)) + 
#   ylim(c(0,30))

