#ex1 

x = 2
y = 4
plot(x,y,pch=0,col='magenta')
plot(1,3,pch=0,col='magenta')


#ex2

x <-  seq(-pi, pi , 0.1)
y= sin(x)
z = cos(x)

plot(x,y, col='blue',pch=8,type='o', main="Sinx v/s Cosx", main.color="Brown",ylab="cosx/sinx", xlab="pi-values")
lines(x,z, col='red',pch=4,type = 'o')
legend('topleft',inset=0.05,c("Squares","Cubes"),lty=1,col=c('red','dark green'),pch=c(8,4),title="Sinx and Cosx")


#ex3

#x1 <-  seq(1,8,1)
#y1 <- c(0.2088,0.1582,0.131,0.131,0.1953,0.1246,0.0135,0.0370)

#barplot(y1,names.arg=x1,col="pink",main="Biostatistics book plot",col.main="brown",xlab="No. of assistance programs",ylab="Probabilities")

# Data
# Data
x <- 1:8  # Number of assistance programs
probability <- c(0.21, 0.15, 0.13, 0.13, 0.20, 0.13, 0.01, 0.04) # Probabilities

# Adjust margins for better alignment
par(mar = c(5, 5, 2, 2), lwd = 0.7)  # Set line width to thin (lwd = 0.7)

# Create bar plot without default axes
bar_positions <- barplot(probability, names.arg = x, col = "gray", border = "black",
                         ylim = c(0, 0.25), axes = FALSE, space = 0.2)  

# Add y-axis with THIN inward-facing tick marks
axis(2, at = seq(0, 0.25, by = 0.05), las = 1, tck = 0.015, lwd = 0.7)  

# Add x-axis with THIN tick marks at midpoints of bars
axis(1, at = bar_positions, labels = FALSE, tck = 0.015, lwd = 0.7)  

# Add x-axis label with italicized text
mtext(expression(italic(x) ~ "(number of assistance programs)"), side = 1, line = 3)  

# Add y-axis label inside, properly aligned
mtext("Probability", side = 2, line = 3.2, las = 0)

# Add a thin box around the plot for proper axis connection
box(lwd = 0.1)  

# Data
x <- 1:8  # Number of assistance programs
probability <- c(0.21, 0.15, 0.13, 0.13, 0.20, 0.13, 0.01, 0.04) # Probabilities

# Adjust margins for better alignment
par(mar = c(5, 5, 2, 2), lwd = 0.7)  # Use thin lines

# Create bar plot without default axes
bar_positions <- barplot(probability, names.arg = x, col = "gray", border = "black",
                         ylim = c(0, 0.25), axes = FALSE, space = 0.2)  

# Add y-axis with inward-facing tick marks
axis(2, at = seq(0, 0.25, by = 0.05), las = 1, tck = 0.015, lwd = 0.7)  

# Add x-axis tick marks at midpoints of bars
axis(1, at = bar_positions, labels = FALSE, tck = 0.015, lwd = 0.7)  

# Add x-axis label with italicized text
mtext(expression(italic(x) ~ "(number of assistance programs)"), side = 1, line = 3)  

# Add y-axis label inside, properly aligned
mtext("Probability", side = 2, line = 3.2, las = 0)

# Manually draw a line to connect the y-axis bottom with the x-axis front
segments(x0 = par("usr")[1], y0 = 0, x1 = par("usr")[2], y1 = 0, lwd = 0.7)  # Thin line at bottom
segments(x0 = par("usr")[1], y0 = 0, x1 = par("usr")[1], y1 = max(probability), lwd = 0.7)  # Left y-axis

# Data
x <- 1:8  # Number of assistance programs
probability <- c(0.21, 0.15, 0.13, 0.13, 0.20, 0.13, 0.01, 0.04) # Probabilities

# Adjust margins for better alignment
par(mar = c(5, 5, 2, 2), lwd = 0.7)  # Thin lines

# Create bar plot with reduced bar width
bar_positions <- barplot(probability, names.arg = x, col = "lightgray", border = "black",
                         ylim = c(0, 0.25), axes = FALSE, space = 0.5, width = 0.6)  # Reduce width & add spacing

# Add y-axis with inward-facing tick marks
axis(2, at = seq(0, 0.25, by = 0.05), las = 1, tck = 0.02, lwd = 0.7)  

# Add x-axis tick marks at midpoints of bars
axis(1, at = bar_positions, labels = FALSE, tck = 0.02, lwd = 0.7)  

# Add x-axis label with italicized text
mtext(expression(italic(x) ~ "(number of assistance programs)"), side = 1, line = 3)  

# Add y-axis label inside, properly aligned
mtext("Probability", side = 2, line = 3.2, las = 0)

# Manually draw a line to connect the y-axis bottom with the x-axis front
segments(x0 = par("usr")[1], y0 = 0, x1 = par("usr")[2], y1 = 0, lwd = 1.7)  # Thin bottom line
segments(x0 = par("usr")[1], y0 = 0, x1 = par("usr")[1], y1 = max(probability), lwd = 1.2)  # Left y-axis









#ex4

par(mfrow=c(2,3)) #2 rows and 3 columns 
#plot1
x <-  seq(-pi, pi , 0.1)
z = cos(x)
plot(x,z, col='red',pch=4,type = 'o')

#plot2
a <- (x^2/3) + 4.2
lines(x,a,col='violet',lwd=2,lty=1)



