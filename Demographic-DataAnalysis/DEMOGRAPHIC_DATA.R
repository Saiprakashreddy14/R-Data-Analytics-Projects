#*__________qno1 import a data of countries statiscs from world bank
#reading the file
imported=read.csv(file.choose())
View(imported)
#------------------------------------------------------------------------------------------
#*_________qno:2 update the data
Regions_2012_Dataset <- c("The Americas","Asia","Africa","Europe","Middle East","The Americas","Asia","The Americas","Oceania","Europe","Asia","Africa","Europe","Africa","Africa","Asia","Europe","Middle East","The Americas","Europe","Europe","The Americas","The Americas","The Americas","The Americas","The Americas","Asia","Asia","Africa","Africa","The Americas","Europe","The Americas","Asia","Africa","Africa","Africa","The Americas","Africa","Africa","The Americas","The Americas","The Americas","Europe","Europe","Europe","Africa","Europe","The Americas","Africa","The Americas","Africa","Africa","Europe","Europe","Africa","Europe","Oceania","Europe","Oceania","Africa","Europe","Asia","Africa","Africa","Africa","Africa","Africa","Europe","The Americas","The Americas","The Americas","Oceania","The Americas","Asia","The Americas","Europe","The Americas","Europe","Asia","Asia","Europe","Middle East","Middle East","Europe","Middle East","Europe","The Americas","Middle East","Asia","Asia","Africa","Asia","Asia","Oceania","Asia","Middle East","Asia","Middle East","Africa","Africa","The Americas","Europe","Asia","Africa","Europe","Europe","Europe","Asia","Africa","Europe","Africa","Asia","The Americas","Europe","Africa","Europe","Asia","Europe","Asia","Africa","Africa","Africa","Africa","Asia","Africa","Oceania","Africa","Africa","The Americas","Europe","Europe","Asia","Oceania","Middle East","Asia","The Americas","The Americas","Asia","Oceania","Europe","The Americas","Europe","The Americas","Oceania","Middle East","Europe","Europe","Africa","Middle East","Africa","Africa","Asia","Oceania","Africa","The Americas","Africa","Europe","Africa","Africa","The Americas","Europe","Europe","Europe","Africa","Africa","Middle East","Africa","Africa","Asia","Asia","Asia","Asia","Oceania","The Americas","Africa","Europe","Africa","Africa","Europe","The Americas","The Americas","Asia","The Americas","The Americas","The Americas","Asia","Oceania","Middle East","Oceania","Middle East","Africa","Africa","Africa","Africa")
imported$region=Regions_2012_Dataset
#------------------------------------------------------------------------------------------
#*_________qno:3>countries with high birth rate
r=imported$Birth.rate > mean(imported$Birth.rate) 
q3=imported[r,c(1,3,6)]
View(q3)
#we observe that most of them are african countries
#------------------------------------------------------------------------------------------

#*________qno:4>print the average birth rate of all regions

extracted=0
lev=levels(factor(imported$region))
for(i in lev){
  cat(i,"'s average birth rate is")
  print('')
  print(mean(imported[imported$region==i,3]))
  #extracted$z=imported[imported$region==i,3]
  extracted[i]=signif(mean(imported[imported$region==i,3]), digits = 4)
  extracted[1]="mean"
  View(extracted)
}

#------------------------------------------------------------------------------------------

#*_________qno:5 investigate the countries with internet less than 10%interenet usage
e5=imported$Internet.users < 10

q5=imported[e5,c(1,4,6)]
View(q5)
#------------------------------------------------------------------------------------------
#*_________qno:6>bar graph representation of average birth rate and region

extracted=(extracted)[c(2:7)]
his=data.frame(region = lev,avgb=extracted)
row.names(his)=c(1:6)
#View(his)
q6=qplot(data=his,x=lev,y=extracted,fill=lev)+geom_col()+xlab("region")+ylab("average birth rate")+scale_fill_discrete(name = "REGION")
q6


#------------------------------------------------------------------------------------------
#*_________qno:7 scatter plot internet and birth rate but use the region that countries belongs to
q7=qplot(data=imported,x=Internet.users,y=Birth.rate,colour=region,size=I(3),shape=I(16))
q7
#------------------------------------------------------------------------------------------
#*_________qno:8 relating internet and birth rate based on income category
q8=qplot(data=imported,x=Internet.users,y=Birth.rate,colour=Income.Group,size=I(4),shape=I(15))
q8

#------------------------------------------------------------------------------------------
#qno:9>>box plot the internet usage based on the regions
p=ggplot(imported,aes(imported$region,Internet.users,colour=region))+geom_boxplot()
p+geom_boxplot(size=1.3)
p  

#-------------------------------------------------------------------------------------------------










