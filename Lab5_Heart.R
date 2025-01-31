#Lab5 Heart 31 Jan 2025

#Ex1
# Read the data
data = read.csv("~/R/Heart.csv", header = TRUE)
print(data)

#Ex2
#i)
print(dim(data))  
print(length(data))  

#ii)
print(colnames(data)) 

#iii)
print(rownames(data))  

#iv) First 30 rows
print(head(data, 30))

#v) Frequency table
print(table(data$ChestPain))

#vi) Categorical variables
print(unique(data$ChestPain))  

# Convert ChestPain to a factor
data$ChestPain <- factor(data$ChestPain, levels = c("typical", "asymptomatic", "nonanginal", "nontypical"))
print(levels(data$ChestPain))
print(nlevels(data$ChestPain)) 

#vii) Extract categorical variables
X <- sapply(data, is.numeric)  
print(X)  

y <- data[!X]  # Extracts categorical variables
print(colnames(y))  # Prints the column names of categorical variables

#Ex3

#i) First five values of ChestPain
print(data$ChestPain[1:5])

#ii) Mean 
print(mean(data$RestBP))

#iii) Mean 
print(mean(data$Age))

#iv) Median
print(median(data$RestBP))

#v) Mode 
mode_function <- function(V) {
  tab <- table(V)
  max_count <- max(tab)
  as.numeric(names(tab[tab == max_count]))  # Returns mode(s)
}

print(mode_function(data$RestBP))

#vi) Standard deviation 
print(sd(data$RestBP))

#vii) 
print(summary(data$RestBP))

#viii) Histogram 
hist(data$RestBP)

#ix) Skewness and Kurtosis
library(moments)
print(skewness(data$RestBP))
print(kurtosis(data$RestBP))

#x) Boxplot
boxplot(data$RestBP)
boxplot(data$RestBP, xlab = "Spread of BP", ylab = "BP", horizontal = FALSE, border = "blue", col = "red")

#xi)
boxplot(data$Age)
boxplot(data$Chol)

#Ex4

#i) 
RBP_20 = subset(data, data$RestBP > 20)
print(RBP_20)
print(dim(RBP_20))

#ii) 
specific_rows = data[c(1, 3, 8, 9, 13, 14, 18, 21), ]
print(specific_rows)

#iii) 
ind_fem = which(data$Sex == 1)
ind = data[ind_fem, ]
print(ind)

#iv) 
new_data <- data.frame(
  RestBP = data$RestBP,
  Chol = data$Chol,
  New_Col = data$RestBP * data$Chol / 234
)
print(new_data)

#v) 
female_data <- subset(data, Sex == 1)
write.csv(female_data, "Lab5_Female_Heart.csv", row.names = FALSE)

print(head(female_data))
