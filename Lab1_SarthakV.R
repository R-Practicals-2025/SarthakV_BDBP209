#Lab 1 Jan 3 2025
#QUESTION 1
#q1.1
print(2.7/2)
#q1.2
print(2.7%/%2)
#q1.3
print(10+5i/2)
#q1.4
print(round(2.5))
#q1.5
print(round(-2.5))
#q1.6
#quotient(%/%)
print(2%/%4-1) 
#q1.7
print(3*2**2)
#q1.8
print(3**2*2)
#q1.9
#modulo(%%)
print(7%/%4)
#q1.10
print(7%%4)
#q1.11
print(-7%%4)
#q1.12
trunc(5.7)
#q1.13
trunc(-5.7)


#Q2
fun1 <- function(x)floor(x+1)
fun1(5.7)
fun1(5.3)

#Q3
#3.1
#3.2
a<-1
b<-2
c<-4
a&&b
!a < b | c > b
!(a < b | c > b)

#Q4

x <- c(5,3,7,8) #4.1 #x is a vector which has double data type
is.integer(x) #4.2 function checks whether the vector is explicitly of type integer.
is.numeric(x) #4.3 checks for both int and double
x<-integer(x) #4.4 length error because X is a vector
print(x)
x <- c(5,3,7,8) #4.5
x<-as.integer(x)
is.integer(x)


#Q5

#5.1
x<-sqrt(2) 
x*x == 2 #False,it will not be exactly 2 due to floating point precision error
x*x - 2
all.equal(x*x,2)





