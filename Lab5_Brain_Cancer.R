#LAB5 Brain_cancer 31 Jan 2025


#Ex1
#read the data
data=read.csv("~/R/BrainCancer.csv", header=TRUE)
print((data))

#Ex2
#i)
print(dim(data))
print(length(data)) #no. of columns only
#ii)
print(colnames(data)) #no. of columns that are present
#iii)
print(rownames(data)) #no. of rows that are present
#iv) first 30 columns
print(head(data,30))

#v)frequency table
print(table(data$diagnosis))
print(table(data$diagnosis,data$gtv))

#vi)categorical 
print(colnames(data)) #"sex" "diagnosis" "loc" "stereo" 
print(head(data,3))

#vii)no. of categorical variables
print(unique(data$sex)) #"Female" "Male"  
print(unique(data$diagnosis)) #"Meningioma" "HG glioma"  "LG glioma"  NA  "Other"
print(unique(data$loc)) #"Infratentorial" "Supratentorial"
print(unique(data$stereo)) #"SRS" "SRT"

#viii)no. of levels in each categorical variable

print(length(unique(data$sex)))
print(length(unique(data$diagnosis)))
print(length(unique(data$loc)))
print(length(unique(data$stereo)))

#Ex3

#i) mean of GTV column
print(mean(data$gtv)) #8.660795, as mean is more than median.


#ii) mean of time column
print(mean(data$time))

#iii) median of GTV  #6.51
print(median(data$gtv))

#iv) mode of GTV


#v) standard deviation
print(sd(data$gtv))

#vi) stat summary
print(summary(data$gtv))

#vii) histogram plot
hist(data$gtv)

#viii) skewness
library(moments)
print(skewness(data$gtv))

#ix) kurtsosis
print(kurtosis(data$gtv))


#x) 
boxplot(range =0.1,data$gtv,xlabel="spread of GTV",ylabel="GTV",horizontal = FALSE,border=c("blue"),col=c("red"))
boxplot(range =0.2,data$gtv,xlabel="spread of GTV",ylabel="GTV",horizontal = FALSE,border=c("blue"),col=c("red"))
boxplot(range =0.05,data$gtv,xlabel="spread of GTV",ylabel="GTV",horizontal = FALSE,border=c("blue"),col=c("red"))
#range 0.1,0.2 shows fewer outliers
#range 0.05 shows more outliers


#xi)
boxplot(data$time)
boxplot(data$ki)
boxplot(data$gtv) 

#boxplot(data$time) has the broadest distribution


#Ex4

#i)

filter1 = subset(data, data$gtv > 20)
print(filter1)
print(dim(filter1))

#ii)

filter2 = data[c(1,3,8,9,13,14,18,21),]
print(filter2)

#iii)

filter3_ind = which(data$sex=="Female")
filter3 = data[filter3_ind,]
print(filter3)

#iv)


data$new_col = (data$gtv*data$ki)/234
new_df = data[,c("gtv","ki","new_col")]
print(new_df)

data = data[,c("X",  "sex", "diagnosis" , "loc" , "ki"  , "gtv", "stereo" , "status"  ,"time" )]
print(data)


#v)

write.csv(filter3, file= "Lab5_female_BrainCancer.csv", row.names = FALSE)


