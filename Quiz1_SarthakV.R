#Quiz

#question1
#k and n as the two inputs and perform arithmetic operation

Arithmetic <- function(k,n){a <- k + n #add
b <-k -n #subtract
c <- k/ n #division
d <- k  %%  n  #modulus
z <- c(a,b,c,d) # created an object consisting of all the objects assigned for arithmetic operations 
return (z)
}
Arithmetic

print(Arithmetic(2,4)) #calling the function res with values of k and n assigned to it.


#question2

#input real no. a,b,c.
#find the roots of the quadratic equation! 

#created a root function

root <- function(a,b,c){ x1 <- (((-b + (sqrt(b^2 - (4*a*c)))) / 2*a ))  
x2 <- (((-b - (sqrt(b^2 - (4*a*c)))) / 2*a ))
res <- c(x1,x2) 
return(res)
} #root function takes three parameters and returns roots.

root
print(root(3,6,2))  

 