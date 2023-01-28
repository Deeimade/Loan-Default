#set working directory in deaktop
setwd("C:/Users/Daniel Imade/Documents/R workspace/R Studio")


#R packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("lubridate")
library(lubridate)
library(stargazer)
library(ggplot2)
library(dplyr)


# Read the data set file
df <- read.csv("LoanData.csv", stringsAsFactors=TRUE, na.strings=c("","NA"))


# clean dataset
df = select(df, VerificationType, LoanDate, Age, Amount, Interest, LoanDuration, 
      MonthlyPayment, Education, NrOfDependants, HomeOwnershipType, 
      IncomeTotal, LiabilitiesTotal, ProbabilityOfDefault, 
      AmountOfPreviousLoansBeforeLoan, Status)

# remove ages less than 18, education less than 1, home ownership less than 0
# probability of default of 0 too
df_clean = select(df, VerificationType, Age, Amount, Interest, LoanDuration, 
            MonthlyPayment, Education, HomeOwnershipType, 
            IncomeTotal, LiabilitiesTotal, ProbabilityOfDefault, 
            AmountOfPreviousLoansBeforeLoan, Status)
#you added number of dependants above in case of wahala
df_clean <- subset(df_clean, Age > 17)
df_clean <- subset(df_clean, Education > 0)
df_clean <- subset(df_clean, HomeOwnershipType >= 0)
df_clean <- subset(df_clean, ProbabilityOfDefault > 0)
df_clean <- na.omit(df_clean)
df_clean <- mutate(df_clean, HousingSituation = if_else(HomeOwnershipType == 0, 'Homeless',
                                                if_else(HomeOwnershipType == 1, 'Owner',
                                                if_else(HomeOwnershipType == 2, 'parents',
                                                if_else(HomeOwnershipType == 3, 'Tenant',
                                                if_else(HomeOwnershipType == 4, 'Tenant',
                                                if_else(HomeOwnershipType == 5, 'Tenant',
                                                if_else(HomeOwnershipType == 6, 'Owner',
                                                if_else(HomeOwnershipType == 7, 'Owner',
                                                if_else(HomeOwnershipType == 8, 'Mortgage',
                                                if_else(HomeOwnershipType == 9, 'Owner',
                                                if_else(HomeOwnershipType == 10, 'Other', "NA"))))))))))))

# Actual Data Analytics

# Settings
op <- options(scipen=999)

# Key descriptive statistics
stargazer(df_clean, type='html')

# Visualization of the dataset
# --------- loan by date
loan_by_date <- select(df, Amount, LoanDate)
loan_by_date <- loan_by_date %>% mutate(LoanDate = ymd(LoanDate))
loan_by_date <- mutate(loan_by_date, Month = month(LoanDate,label = TRUE),Year = year(LoanDate))


# Average amount loaned by people each year
yearly_loan <- loan_by_date %>% group_by(Year) %>% summarise(Loaned = mean(Amount))
summarise(yearly_loan)
firstcolors <- c("#40E0D0", "#FFBF00", "#FF7F50", "#DE3163", "#6495ED", '#5D3FD3', '#6082B6', '#6F8FAF', '#A52A2A', '#E97451', '#818589', '#8A9A5B', '#228B22', '#808000', '#40E0D0')
barplot(height = yearly_loan$Loaned, names = yearly_loan$Year, col = firstcolors, main="Average Amount Borrowed Yearly", xlab="Year", ylab="Amount")

# Total amount loaned by people each year
yearly_total_loan <- loan_by_date %>% group_by(Year) %>% summarise(Loaned = sum(Amount))
summarise(yearly_total_loan)
secondcolors <- c('#6082B6', '#6F8FAF', '#A52A2A', '#E97451', '#818589', "#40E0D0", "#FFBF00", "#FF7F50", "#DE3163", "#6495ED", '#5D3FD3', '#8A9A5B', '#228B22', '#808000', '#40E0D0')
barplot(height = yearly_total_loan$Loaned, names = yearly_total_loan$Year, col = secondcolors, main="Total Amount Borrowed Yearly", xlab="Year", ylab="Amount", xpd = FALSE)


# Age description of Borrowers
hist(df_clean$Age, main="Age description of Borrowers", xlab="Age", ylab="Frequency", col="#FF7F50")


# Borrowers housing situation
housing_table <- df_clean %>% group_by(HousingSituation) %>% summarize(Housing = n())%>% mutate(Prop = round(Housing/sum(Housing)*100, digits = 2))
housing_table <- housing_table %>% arrange(desc(Housing)) %>% mutate(lab.ypos = cumsum(Prop) - 0.5*Prop)
thirdcolors <- c("#FFBF00", "#40E0D0", "#FF7F50", "#DE3163", "#6495ED", "#900C3F")
options(repr.plot.width = 20, repr.plot.height = 7)
ggplot(housing_table, aes(x = "", y = Prop, fill = HousingSituation), xlab = 'rating') + geom_bar(width = 1, stat = "identity", color = "white") + coord_polar("y", start = 0)+ geom_text(aes(y = lab.ypos, label = Prop), color = "white", size = 4)+ scale_fill_manual(values = thirdcolors) + theme_void() + labs(title="Pie chart of Housing Situation of Borrowers") + theme(legend.position = "bottom", legend.text=element_text(size=11), plot.title = element_text(size=14))


# Number of Dependants --- (To be continued)
dependant_table <- select(df, NrOfDependants)
dependant_table <- na.omit(dependant_table)

hist(as.numeric(dependant_table$NrOfDependants), main="Number of dependants of Borrowers", xlab="Number of Dependants", ylab="Frequency", col="#FF7F50")


# Regression Analysis
# To run the linear regression model on independent and dependent variables

# Clean data
regression_table = select(df_clean, Age, Amount, Interest, LoanDuration, 
                  Education, IncomeTotal, LiabilitiesTotal, ProbabilityOfDefault, 
                  AmountOfPreviousLoansBeforeLoan, Status)

regression_table <- regression_table[!(regression_table$Status=="Current"),]
regression_table <- na.omit(regression_table)

str(df_clean)

regression_table <- mutate(regression_table, Repaid = if_else(Status == 'Late', 0,
                                              if_else(Status == 'Repaid', 1, 0)))


# Run correlation test
cor(regression_table[c('Age', 'Amount', 'Interest', 'LoanDuration', 
                       'Education', 'IncomeTotal', 'LiabilitiesTotal',
                       'AmountOfPreviousLoansBeforeLoan')])


# independent variable
# ProbabilityOfDefault


# linear regression analysis
linear_model <- lm(Repaid ~ LiabilitiesTotal, data=regression_table)
summary(linear_model)


# multiple regression analysis
multiple_model <- lm(ProbabilityOfDefault ~ AmountOfPreviousLoansBeforeLoan + Education + Amount, data=regression_table)
summary(multiple_model)


















