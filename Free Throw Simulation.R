#remove "#" from the next three lines if this is your first time running this
#install.packages("googlesheets4")
#install.packages("ggplot2")
#install.packages("ggthemes")
library(googlesheets4)
library(ggplot2)
library(ggthemes)

#create dataframe that automatically links to excel sheet where scores are put in
sheet <- read_sheet('https://docs.google.com/spreadsheets/d/1i0POiQWoaXN1H13zFE2owKJNw-pS9mtVO2kK8gf6jrE/edit#gid=455211504',sheet='Sheet2')
df=as.data.frame(sheet)

#sample from each person's score (drop n/a values) and keep track of # of wins
#ties are split evenly between number of people tied for first
winners=c(0,0,0,0,0,0)
winning_number=c(rep(0,100000))
highest_non_winner=c(rep(0,100000))
for (i in 1:100000) {
  a=sample(na.omit(df[['Zach']]),1)
  b=sample(na.omit(df[['Tyler']]),1)
  c=sample(na.omit(df[['Nick']]),1)
  d=sample(na.omit(df[['Nikhil']]),1)
  e=sample(na.omit(df[['Ed']]),1)
  f=sample(na.omit(df[['Oren']]),1)
  all<-c(a,b,c,d,e,f)
  max=max(all)
  second=max(all[all!=max])
  winning_number[i]=max
  highest_non_winner[i]=second
  num_winners=0
  for (j in c(a,b,c,d,e,f)) {
    if (j==max) {num_winners = num_winners+1}
  }
  if (max==a) {winners[1] = winners[1] + 1/num_winners}
  if (max==b) {winners[2] = winners[2] + 1/num_winners}
  if (max==c) {winners[3] = winners[3] + 1/num_winners}
  if (max==d) {winners[4] = winners[4] + 1/num_winners}
  if (max==e) {winners[5] = winners[5] + 1/num_winners}
  if (max==f) {winners[6] = winners[6] + 1/num_winners}
}

#convert wins into percentage
Zach=winners[1]/100000
Tyler=winners[2]/100000
Nick=winners[3]/100000
Nikhil=winners[4]/100000
Ed=winners[5]/100000
Oren=winners[6]/100000
percentages<-c(Zach, Tyler,Nick,Nikhil,Ed,Oren)
names<-c("Zach","Tyler","Nick","Nikhil","Ed","Oren")
win_percentages<-data.frame(names,percentages)

#plot barchart for number you need to beat to win
winning_number=as.numeric(winning_number)
counts=table(winning_number)
barplot(cumsum(counts/(sum(counts))),main="Chance of Winning(or Tying) if you Score N",col="blue",
        xlab="N",ylab="Probability")

#plot barchart of winning percentages in simulation
ggplot(win_percentages,aes(x=names,y=percentages,fill=names)) + geom_bar(stat="identity")+
  labs(x="",y="Simulated Winning Percentage") + theme(legend.position="none")+
  geom_text(aes(label=paste(round(percentages*100, digits=1),"%",sep="")),size=3,nudge_y=-0.03)
        