#LAB5&6 Brain_cancer 07 Feb 2025




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


#Ex5

#i)

#check the class of the columns first
print(class(data$sex))

#i)
data$X <- as.factor(data$X)
data$sex <- factor(data$sex,levels= c("Male","Female"))
is.factor(data$sex)
is.factor(data$loc)

#ii)
print(levels(data$sex))
print(nlevels(data$sex))

#iii)
print(unique(data$diagnosis))
data$diagnosis <-  factor(data$diagnosis,levels = c("Meningioma", "HG glioma",  "LG glioma",  "Other"))
is.factor(data$diagnosis)
print(levels(data$diagnosis))
print(nlevels(data$diagnosis))

#Ex6
###generate levels using gl() function

#get the num_rows of data
num_rows = nrow(data)
temperature <- gl(3, ceiling(num_rows / 3), num_rows, labels = c("Hot", "Cold", "Lukewarm"))
print(temperature)

brain_cancer_data_new <- cbind(data, Temperature = temperature)

print(brain_cancer_data_new)


#Ex7
###using tapply()

#without trim
tapply(data$gtv, data$ki, mean)

#with trim
tapply(data$gtv, data$ki, mean, trim=0.1) 
#trims/removes the smallest and largest i.e. extreme values from both ends before computing mean. 

#demo using subsets
sub = subset(data, ki==70)
sub = sub[order(sub$gtv), ] 
print(sub)
print(mean(sub$gtv))
ind=floor(nrow(sub)*0.1)
sub2 = sub[(ind+1):(nrow(sub)-ind),]
print(sub2)
print(mean(sub2$gtv))

#Ex8
##pmin, pmax

print(pmin(data$gtv,data$time,data$ki)) #prints the minimum in parallel
print(pmax(data$gtv,data$time,data$ki)) #prints the maximum in parallel 

#Ex9

###sort,order,rank

ranks <- rank(data$gtv)
sorted <- sort(data$gtv)
ordered <- order(data$gtv)
view <- data.frame(data$gtv,ranks,sorted,ordered)
print(view)

#ii)

#ordered_diagnosis <- data$diagnosis[ordered]

#newdf = data.frame(sorted, ordered_diagnosis)

#sorted_sub_gtv = sort(ki_100$gtv)



#Ex 10
#(i)
fil=data[1:6,3:8]
#(ii)
filter_mat=as.matrix(fil)
print(class(filter_mat))
print(mode(filter_mat))
print(attributes(filter_mat))
#(iii)
new_col=data$ki+data$gtv+data$time
#(iv)
new_data2=data.frame(data,new_col)
print(new_data2)
print(colnames(new_data2))
#(v)
new_data3=cbind(data,new_col)
print(colnames(new_data3))
#(vi)
#fil2=data[c(1,7,8,8),]
# Select rows 26 and 35 from the original data
fil2 = data[c(26, 35), ]

# Append the selected rows to the original data
new_data3 = rbind(data, fil2)

# Reset row indices (Fixes incorrect row numbering)
rownames(new_data3) <- NULL

# Print the updated dataframe
print(new_data3)

# Print the new dimensions (number of rows and columns)
print(dim(new_data3))


#Ex 11
x<-matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=4)
print(x)
print(rownames(x))
print(colnames(x))
rownames(x)<-rownames(x,do.NULL = FALSE,prefix='trial.')
drugs<-c("aspirin","paracetamol","nurofen","hedex","placebo")
colnames(x)<-drugs
print(x)
dimnames(x) <- list(rownames(x), paste("drug", 1:5, sep=""))
print(x)
dimnames(x) <- list(NULL, paste("drug", 1:5, sep=""))
print(x)

#Ex 12

X<-matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=4)
print("Matrix X:")
print(X)

# (i) Mean of the 5th column
print("Mean of 5th column:")
print(mean(X[,5]))

# (ii) Variance of the 4th row
print("Variance of 4th row:")
print(var(X[4,]))

# (iii) Row Sums
print("Row Sums:")
print(rowSums(X))

# Alternative method
print("Row Sums (apply function):")
print(apply(X, 1, sum))

# (iv) Column Sums
print("Column Sums:")
print(colSums(X))

# Alternative method
print("Column Sums (apply function):")
print(apply(X, 2, sum))

# (v) Row Means
print("Row Means:")
print(rowMeans(X))

# Alternative method
print("Row Means (apply function):")
print(apply(X, 1, mean))

# (vi) Column Means
print("Column Means:")
print(colMeans(X))

# Alternative method
print("Column Means (apply function):")
print(apply(X, 2, mean))

# (vii) 
# Define group labels for each row
group <- c("A", "B", "B", "A")

# Using rowsum()
print("Summing rows by group (rowsum function):")
print(rowsum(X, group))

# (b) 
print("Row indices:")
print(row(X))

print("Column indices:")
print(col(X))

# (c) 

print("Summing rows using tapply:")
print(tapply(X, list(group[row(X)], col(X)), sum))

# (d) 
print("Summing rows using aggregate:")
print(aggregate(X, list(group), sum))


# (viii)  Shuffling of elements
X2<-matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=4)
print(X2)
Y<-apply(X2,2,sample) #column wise shuffling
print(Y)
Y1<-apply(X2,1,sample) #row wise shuffling
print(t(Y1))

# (ix)
X3 <- rbind(X2, apply(X2,2,mean))
print(X3)
X4 <- cbind(X3,apply(X3,1,var))
print(X4)
headings <- c(paste("drug.",1:5,sep=""),"var")
dimnames(X4)<- list(rownames(X4),headings)
print(X4)
headings2 <- c(paste("Trial-",1:4,sep=""),"Mean")
rownames(X4)<-headings2
print(X4)


# Ex 13
# (i) and (ii)
eg_sweep<-data.frame(data$ki, data$gtv, data$time)
cols<-apply(eg_sweep,2,mean)
print(cols)

# (iii)
cols.means <- matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),
                     nrow=dim(eg_sweep)[1])
print(cols.means)
eg_sweep_alt <- eg_sweep - cols.means
print("Method 1")
print(eg_sweep_alt)


# (iv)
eg_sweep1 <- sweep(eg_sweep,2,cols)

print("Method 2")
print(eg_sweep1)


# Ex 14
# Read the data from the file
data <- read.table("/home/ibab/R/pgfull.txt", header = TRUE)

species <- data[, 1:54]
print(species)

max_indices <- max.col(species)
print(max_indices)

species_names <- names(species)[max_indices]
print(species_names)

species_table <- table(species_names)
print(species_table)


min_indices <- max.col(-species)
print(min_indices)


min_species_names <- names(species)[min_indices]


min_species_table <- table(min_species_names)
print(min_species_table)


# EX 15

# (i) 
apples <- c(4, 4.5, 4.2, 5.1, 3.9)
oranges <- c(TRUE, TRUE, FALSE)
chalk <- c("limestone", "marl", "oolite", "CaCO3")
cheese <- c(3.2 - 4.5i, 12.8 + 2.2i )
items <- list(apples, oranges, chalk, cheese)
print(items)

print(items[[3]])        
print(items[[3]][3])     


print(items[3])   
print(items[[3]])  

# (ii) 
items <- list(first = apples, second = oranges, third = chalk, fourth = cheese)
print(items$fourth)  
print(class(items))   

# (iii) 
print(lapply(items, length))  
print(lapply(items, class))    
print(lapply(items, mean))     

# (iv) 
print(summary(items))  
print(str(items))      









                                        




