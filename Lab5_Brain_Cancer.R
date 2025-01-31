#read the data
data=read.csv("~/R/BrainCancer.csv", header=TRUE)
print((data))

print(dim(data))
print(length(data)) #no. of columns only
print(colnames(data)) #no. of columns that are present
print(rownames(data))
print(head(data))
print(tail(data))
print(table(data$diagnosis))
print(table(data$gtv))
#print(table(data))

print(data$diagnosis[1:5])
print(mean(data$gtv))
print(sd(data$gtv))


#use attach of data i.e attach(data) and call the function such as mean