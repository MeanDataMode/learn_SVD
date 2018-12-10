# By Anthony Layton (Tony Layton)
#

library("RColorBrewer")


check_var <- function(SVD, number_of_factors = 4) {
    var1 <- prop.table(SVD$d^2)
    print(sum(var1[1:number_of_factors]))
}

# The following are used to create a 'Random' dataset
g_1 <- function() {
    a <- c()
    a <- c(a, sample(c(0, 4, 4, 5, 5), 4, replace = T)) # Beer
    a <- c(a, sample(c(0, 0, 4, 4), 4, replace = T)) # Micro-Brew
    a <- c(a, sample(c(0, 0, 0, 0, 4), 4, replace = T)) # Wine
    return(a)
}

g_2 <- function() {
    a <- c()
    a <- c(a, sample(c(0, 0, 4, 4), 4, replace = T)) # Beer
    a <- c(a, sample(c(0, 4, 4, 5, 5), 4, replace = T)) # Micro-Brew
    a <- c(a, sample(c(0, 0, 4, 4), 4, replace = T)) # Wine
    return(a)
}

g_3 <- function() {
    a <- c()
    a <- c(a, sample(c(0, 0, 0, 0, 4), 4, replace = T)) # Beer
    a <- c(a, sample(c(0, 0, 0, 0, 4), 4, replace = T)) # Micro-Brew
    a <- c(a, sample(c(0, 4, 4, 5, 5), 4, replace = T)) # Wine
    return(a)
}

names <- data.frame(read_csv("1997names.csv", col_names = FALSE))
name <- c()
for (i in names) {name <- c(name, i)}
data <- data.frame(g_1(), g_1(),g_1(),
                   g_2(), g_2(), g_2(),
                   g_3(), g_3())


rownames(data) <- sample(x = name, nrow(data), replace = F)
colnames(data) <- c("Bud Light", "Coors Light", "Miller Lite", 
                    "Micro-Brew A", "Micro-Brew B", "Micro-Brew C",
                    "Wine A", "Wine B")


data_matrix <- data.matrix(data)
hmcol<-brewer.pal(5,"Blues")
heatmap(data_matrix, Rowv=NA, Colv=NA, col = hmcol, scale= "column", margins=c(10,10))
data


SVD <- svd(x = t(data))
check_var(SVD = SVD, number_of_factors = 3) # Check Variance # Looking for >= 70% explained


number_of_factors = 3
reconstructed_matrix <- (SVD$u[,1:number_of_factors] %*% diag(SVD$d[1:number_of_factors]) %*% t(SVD$v[,1:number_of_factors]))
heatmap(reconstructed_matrix, Rowv=NA, Colv=NA, col = hmcol, scale="column", margins=c(10,10))
reconstructed_matrix


recommendations <- reconstructed_matrix - data
recommendations <- data.matrix(recommendations)
    heatmap(data_matrix, Rowv=NA, Colv=NA, col = hmcol, scale="column", margins=c(10,10))
heatmap(recommendations, Rowv=NA, Colv=NA, col = hmcol, scale="column", margins=c(10,10))
reconstructed_matrix
