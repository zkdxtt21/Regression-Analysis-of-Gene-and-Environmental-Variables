Y_table <- read.csv("~/Desktop/homework/578/IDYgroup7.csv")
E_table <- read.csv("~/Desktop/homework/578/IDEgroup7.csv")
G_table <- read.csv("~/Desktop/homework/578/IDGgroup7.csv")

Y_table <-  Y_table[order(Y_table$ID), ]
E_table <-  E_table[order(E_table$ID), ]
G_table <-  G_table[order(G_table$ID), ]

whole_data <- cbind(Y = Y_table[,c(-1, -2)], E_table[ ,c(-1,-2)], G_table[ ,c(-1,-2)])

rownames(whole_data) <- as.character(Y_table$ID)

mydata <- whole_data[complete.cases(whole_data), ]
nrow(mydata)/nrow(whole_data)

for(i in 8 : 32){
  mydata1[,i] = as.factor(mydata[,i])
}

summary(mydata1)
cor_matrix = cor(mydata)
cor_matrix[1,]
cor_matrix[1,abs(cor_matrix[1,]) > 0.05]
for(i in 2:32){
  cor_vec = cor_matrix[i, -1]
  print(cor_vec[ abs(cor_vec) > 0.1] )
}

attach(mydata)


fit.linear.full <- lm(formula = Y ~ ., data = mydata)
summary(fit.linear.full)

fit.quad.full <- lm(formula = Y ~ .^2, data = mydata)
summary(fit.quad.full)

anova(fit.linear.full, fit.quad.full)

null <- lm(Y ~ 1, data = mydata)
step(null, scope = list(upper = fit.quan.full), data = mydata, direction = "both")

fit1 <- lm(formula = Y ~ E3 + E4 + E2 + E6 + E1 + G20 + E2:E6 + E4:E1 + 
    E3:E2, data = mydata)
summary(fit1)

fit2 <- lm(formula = Y ~ E3 + E4 + E2 + E6 + E1 + G20 + E2:E6 + E4:E1 + 
             E3:E2 - 1 , data = mydata)
summary(fit2)

fit3 <- lm(formula = Y ~ E3 + E4 + E2 + E6  + G20 + E2:E6 + E4:E1 + 
             E3:E2 - 1, data = mydata)
summary(fit3)

fit4 <- lm(formula = Y ~ E3 + E4 + E6  + G20 + E2:E6 + E4:E1 + 
             E3:E2 - 1, data = mydata)
summary(fit4)

fit5 <- lm(formula = Y ~ E3 + E4 + G20 + E2:E6 + E4:E1 + 
             E3:E2 - 1, data = mydata)
summary(fit5)

fit6 <- lm(formula = Y ~ E3 + E4 + G20 + E2:E6 + E4:E1 - 1, data = mydata)
summary(fit6)

fit7 <- lm(formula = Y ~ E3 + E4 + E2:E6 + E4:E1 - 1, data = mydata)
summary(fit7)

fit8 <- lm(formula = Y ~ E3  + E2:E6 + E4:E1 - 1, data = mydata)
summary(fit8)

final_quad_model <- lm(formula = Y ~ E3  + E2:E6 + E4:E1 - 1, data = mydata)
summary(final_quad_model)
anova(final_quad_model)
par(mfrow = c(2,2))
plot(final_quad_model)

# choose E1, E3, E4, E2, E6, G20

data1 <- as.data.frame(cbind(Y, E1, E2, E3, E4, E6, G20))
fit.cubic.full <- lm(Y ~ (.)^3, data = data1)
summary(fit.cubic.full)
