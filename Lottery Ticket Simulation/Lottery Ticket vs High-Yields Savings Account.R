install.packages("ggplot2")
library(ggplot2)

###Lottery VS Savings Account###
#This simulation is designed to illustrate how improbable winning the lottery is and how using the money
#that would be spent on lottery tickets could instead be put into a high-yields savings account. The
#simulation shows the money a person has after a set number of years for both routes (buying lottery
#tickets vs putting that amount into a high-yields savings account

##Creating the Function###
#CE:The readline function brings up a prompt for the user. The input of the user will
#be used for the value of the simulation. Here the values for the price of the lottery ticket, how often
#tickets are beings purchased, the interest-rate of the savings account, and the number of years the
#simulation will run for. 

Simulation<-function(){
  price<-as.numeric(readline("Enter the lottery ticket price or how much you're putting into savings: "))
  purchase_rate<-readline("How often are you buying lottery tickets or putting into savings?(daily,weekly,monthly,yearly): ")
  interest_rate<-as.numeric(readline("Enter the annual interest rate of your high-yield savings account(in percentage,e.g.,5 for 5%): ")) / 100
  years<-as.numeric(readline("Enter the number of years to simulate: "))
  
#Loops that define the time intervals for daily, weekly, monthly and yearly. 
  if (purchase_rate=="daily"){
    periods_per_year<-365
  }else if (purchase_rate=="weekly"){
    periods_per_year<-52.1429
  }else if (purchase_rate=="monthly"){
    periods_per_year<-12
  }else if (purchase_rate=="yearly"){
    periods_per_year<-1
  }else{
    stop("Invalid rate of purchase. Choose 'daily', 'weekly', 'monthly', or 'yearly'.")
  }
  
#This line tells us the total period/cycles will be the periods. We also set the initial savings_balance
#at 0. 

  total_periods<-periods_per_year*years
  
  savings_balance<-0
  
  total_deposited<-total_periods*price
  
#The lottery balance will also be set to 0. 
  lottery_balance<-0
  chance<-1/302575350
  
#The following lines of code will be used later for the graphing portion. They track period over time
  time_periods<-1:total_periods
  savings_over_time<-numeric(total_periods)
  lottery_over_time<-numeric(total_periods)
  
#This loop will iterate through whatever the input was. The equation below will output the balance of the
#savings account after the simulation. The 'runif' statement is making the simulation randomly generate 
#a number from 0 to 1(uniform dist). The 'if' statement that precedes it will make it so that if the
#generated number is less than the probability of winning the lottery, 10 million dollars is added to
#the lottery balance.

  for (period in 1:total_periods){
    savings_balance<-savings_balance*(1+interest_rate/periods_per_year)+price
    if (runif(1)<chance){
      lottery_balance<-lottery_balance+10000000 
    }
  
  
#Record balances for plotting
  savings_over_time[period]<-savings_balance
  lottery_over_time[period]<-lottery_balance
  }
  
#Outputs. The dollar amounts are rounded to the nearest 2 decimal places. The \n are just new lines.
  cat("After", years, "years of saving", price, "per", purchase_rate, ":\n")
  cat("Total money deposited into the savings account: $", round(total_deposited, 2), "\n")
  cat("Savings account balance: $", round(savings_balance, 2), "\n")
  cat("Lottery balance: $", round(lottery_balance, 2), "\n")

#a dataframe is created the lottery and saving balances over time as well as the 
df<-data.frame(
  Period = time_periods,
  Savings = savings_over_time,
  Lottery = lottery_over_time
)

#These lines of code plot the dataframe above. Two lines will be created. One to illustrate the lottery
#ticket balance and one for the savings account balance.
ggplot(df,aes(x=Period))+
  geom_line(aes(y=Savings,color="Savings"),linewidth=1)+  
  geom_line(aes(y=Lottery,color="Lottery"),linewidth=1)+  
  labs(title="Savings vs Lottery Balance Over Time",
       x="Cycles",
       y="Balance($)",
       color="Legend") +
  theme_minimal()
}

###Run the Simulation
Simulation()
2


