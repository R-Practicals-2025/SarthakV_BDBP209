#Lab 2 Jan 10 2025
#Question1
#q1.1
print(round(12.1343,digits=3))
#q1.2
print(round(123.12344,digits=3))
#q1.3
print(round(1234.12344,digits=3))
#q1.4
print(round(12345.12344,digits=3))
#q1.5
x<-round(12345.12344,digits=3)
options(digits = 15)
print(x)
y<-round(12345.12344,digits=3)
options(digits = 20)
print(y)
#q1.6
formatC(round(12345.12344,digits=3),format="f",digits=3)
#q1.7
print(1234.12344)
#q1.8
print(1234.723,digits=3)
#q1.9
print(1234.723,digits=5)
#q1.10
print(round(123456788.123,digits=2),digits=20)
#Q1.11
print(round(123456789.1234,digits=4),digits=20)
#Q1.12
paste("Hello World")
paste("Hello","World", sep = " , ")
#q1.13
paste(1:10)
paste(1:10)[4]
#q1.14
as.numeric(paste(1:10))
#q1.15
paste(1:10,collapose=".")
#q1.16
print(paste(c("Hello","World"),1:10,sep="-"))
print(paste("Hello",1:10,sep="-"))

#Question2
#q2.1
0:10
#q2.2
15:5
#q2.3
seq(0,1.5,0.1)
#q2.4
seq(6,4,-0.2)
#Q2.5
N <- c(55,76,92,103,84,88,121,91,65,77,99)
N
#Q2.6
seq(from=0.04,by=0.01,length=11)
seq(0.04,by=0.01,along=N) #as N is 11
#Q2.7
seq(from=0.04,to=0.14,along=N) #yes, it matches the output of q1.6
#q2.8
sequence(c(4,3,4,4,4,5))
#q2.9
rep(9,5)
rep(1:4,2)
rep(1:4,each=2,times=3)
rep(1:4,1:4)
#q2.10
rep(1:4,c(4,1,4,2))
rep(c("cat","dog","goldfish","rat"), c(2,3,2,3))
#q2.11
seq(-1,1,by=0.1)
seq(-1,1,0.1)    
#q2.12
seq(-1,1,length=7)
#q2.13
-1+(0:20)*0.1

#Question3
#q3.1
3/0
#q3.2
exp(-Inf)
#q3.3
(0:3)**Inf
#q3.4
0/0
#q3.5
Inf - Inf
#q3.6
Inf/Inf
#q3.7
is.finite(10)
#q3.8
is.infinite(10)
#q3.9
is.infinite(Inf)
#q3.10
y <- c(4,NA,7)
y=="NA"
is.na(y)




#LAB3 13 Jan 2025
#Question4
#q4.1
vec <- c(4,7,6,5,6,7)
vec
class(vec) 
length(vec) #Length of the vector
min(vec) #min element of the vector
max(vec) #max element of the vector

#q4.2
vec <- scan()

#q4.3
vec[4] #accessing the element using the index 

#q4.4
ind <- c(2,3,6)
vec[ind]
vec[c(2,3,6)]

#q4.5
vec[-1] #drops the first element from the vector, like dequeue or pop operation
vec[c(-1,-2,-3)]

#q4.6
vec[-length(vec)] #drops the last element
vec[-(length(vec)-1)] #drops the second last element

#q4.7
trim <- function(v)sort(v)[-c(1,2,length(v)-1,length(v))]
trim(v)
trim(vec)
trim(vec5)

#q4.8
vec[1:3]#first to third element
vec[seq(2,length(vec),2)]
seq(2,length(vec),2)
v[seq(2,length(v)-1,2)] 

#different way to perform similar operation
vec[-seq(1,length(vec),2)] #pops elements at index -1 and -3, printing elements at 2,4.
-seq(1,length(vec),2) 

#using modulus operator
vec[1:length(vec) %% 2==0] #access the TRUE boolean from the boolean vector
1:length(vec) %% 2==0 #gives a boolean vector

#q4.9
x <- 0:10
x
#method1
x[x<5] #0,1,2,3,4
sum(x[x<5]) #sum of the selected elements

#method2
#using ifelse command

sum(ifelse(x<5,x,0))

#ifelse creates a new vector with elements less than 5 are retained
#while the ones greater are replaced with zero
ifelse(x<5,x,0)

#q4.10

newvec <- c(43,25,63,92,27,47,83,79)
newvec

#sum of the three largest values
y <- sort(newvec)
y #sorted vector
sum(y[c(length(y)-2,length(y)-1,length(y))])

#q4.11
which.max(newvec)
which.min(newvec)

#4.12
cbind(1:10,10:1) #combines the vectors by columns into a matrix with two cols
rbind(1:10,10:1) #row wise 

#4.13
X <- c(1:10)
X
Y <- c(1:10*5)
Y
X*Y
X+Y
X/Y
X^Y
log(X)
exp(Y)


#Question5

#q5
z <- 1:24
dim(z) <- c(2,4,3)
z

#q5.1
matX <- matrix(c(1,0,0,0,1,0,0,0,1), nrow=3)
matX
matX[1,]
matX[,1]
matX[,2]
matX[2,2]

#q5.2
vector2 <- c(1,2,3,4,4,3,2,1)
matY <- matrix(vector2, byrow=T,nrow=2) #if byrow=T, then elements are filled row wise
matY

dim(vector2) <- c(4,2)
vector2 

is.matrix(vector2) #will return True


#Question6


#q6.1
vec
min(vec)
max(vec)
sum(vec)
range(vec) #returns a vector of min and max element
is.vector(range(vec)) #returns True
sort(vec)

#q6.2
matX
colMeans(matX) #mean of each column of a matrix or dataframe

#q6.3
H <- 1:4
H
I <- 1:3
I
z <- H %o% I #Outer product
z

YoX <- Y[1:3] %o% X[1:4]
YoX

t(z)  #transpose of matrix z
t(YoX)

#dot product methods
X %*% Y
sum(X*Y)

crossprod(X[1:4],z)

#gives identity  matrix
diag(4) 

#q6.4
class(X)
class(matA)
class(vector1)
