library(corrplot)
library(mice)
data1 = read.csv(file = "/Users/pritishchatterjee/Documents/LamhaR/data.csv")
View(data1)
c <- data1[c("Size_Class","Size_FabIndia")]
c <- ifelse(c == "XXL", 6,
            ifelse(c == "XL", 5, 
                   ifelse(c == "L", 4, 
                          ifelse(c == "M", 3, 
                                 ifelse(c == "S", 2, 
                                        ifelse(c == "XS", 1, NA)
                                 )
                          )
                   )
            )
)

data1[c("Size_Class","Size_FabIndia")] <- c


tempData <- mice(data1,m=5,maxit=50,meth='pmm',seed=500)
data1 <- complete(tempData,1)
View(data1)
summary(data1)
#Multiple Regression lm(y~x1+x2+x3....,data)
model<-lm(Size_Class~City+Height+Weight+Age+Fit_preference+Figure_Type+Body_Shape+Arm_Shape+Abdomen+Hip+Critical_Area_Of_Measurement_1+UK_Size+Size_FabIndia+Satisfaction_level,data = data1[,c("Size_Class","City","Height","Weight","Age","Fit_preference","Figure_Type","Body_Shape","Arm_Shape","Abdomen","Hip","Critical_Area_Of_Measurement_1","UK_Size","Size_FabIndia","Satisfaction_level")])
model
summary(model)
a <- coef(model)[1]
b1 <- coef(model)[2]
b2 <- coef(model)[3]
b3 <- coef(model)[4]
b4 <- coef(model)[5]
b5 <- coef(model)[6]
b6 <- coef(model)[7]
b7 <- coef(model)[8]
b8 <- coef(model)[9]
b9 <- coef(model)[10]
b10 <- coef(model)[11]
b11 <- coef(model)[12]
b12 <- coef(model)[13]
b13 <- coef(model)[14]
b14 <- coef(model)[15]

y=a+b1*2+b2*5.5+b3*75+b4*43+b5*3+b6*3+b7*7+b8*1+b9*3+b10*2+b11*1+b12*14+b13*3+b14*3
y

cor(data1$Size_Class,data1$Body_Shape,method="pearson")
M<-cor(data1)
corrplot(M,method = "number")
