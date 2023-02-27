### Pertemuan 2

    data<-c(12, 11, 10, 9, 8)

    ts.plot(data)

    print(data)

    library(lmtest)

    Y<-c("Program", "Studi", "Statistika")
    print(Y)
    
    data<-c(1:10)
    data<-c(10:1)
    print(data)
    print(data[3])
    print(data[c(1, 2, 6)])
    
    data[-3]
    data[-c(1, 7, 10)]
    data[data<5]

### matrix
    ma<-matrix(c(1:4), nrow = 2, ncol = 2)
    print(ma)

    ma[1]
    print(ma[,1])
    print(ma[1,])
    print(ma[1,2])
    print(ma[-2,])
    
    ar<-matrix(c(5:8), nrow = 2, ncol = 2)

 ## menggabungkan baris
    rbind(ma,ar)

 ## menggabungkan kolom
    cbind(ma,ar)

    
### data frame
    kelasA<-data.frame(c(1:5), c("andi", "budi", "cendy", "dian", "eka"), c(81:85))
    colnames(kelasA)<-c("No", "Nama", "Nilai")
    print(kelasA)


### membuat fungsi
    ratarata<-function(x){
      nilai<-sum(x)/length(x)
      nilai
    }

ratarata(data)


### standar deviasi 
 ## v1
    deviasi<-function(x){
      nilai<-sqrt(sum((x - (sum(x)/length(x)))^2)/length(x))
      nilai
    }

 ## v2
    stdev <- function(x) {
      n <- length(x)
      mean_x <- mean(x)
      ss <- sum((x - mean_x)^2)
      s <- sqrt(ss / (n - 1))
      return(s)
    }

    x <- (10:30)
    stdev(x)

 ## v3
    dev1asi <- function(x){
      sd(x)
    }

    
### Pertemuan 3
    
 ## standar deviasi dari rumus
    st1 <- function(x){
      a <- x
      b <- 0
      for(i in 1:length(x)){
        b <- b + ((a[i] - mean(a)))^2
      }
      var <- b/(length(x)-1)
      sdv <- sqrt(var)
      print(sdv)
    }
    
    n <- c(1:10)
    

### fungsi if
    q <- c(-12, -9, 12, 8, 7, 0, 10, -10, 2, 3, 21)
    
    tanda <- function(x){
      tand <- NULL
      for (i in 1:length(x)) {
        if(x [i] > 0) tand[i] = +1
        else if (x [i] < 0) tand[i] = -1
        else tand[i] = 0
      }
      print(tand)
    }

    
### fungsi integral
    coba = function(x) {
      1/((x+1)*sqrt(x))
    }
    
    integrate(coba, lower = 0, upper = Inf)
    
    
### turunan/derivative
    D (expression((x^3)*(y^2)), "x")
    
    
    
### contoh soal 2.1.1
    contoh211 <- function(expr, age, t) {
      sxt <- eval({x = age + t; expr})
      sx <- eval({x = age; expr})
      output <- (sx - sxt)/sx
      print(output)
    }
    
    expr <- expression(1-(x/100))
    
    contoh211(expr, age = 30, t=10)
    
    
    
    
    
    
    