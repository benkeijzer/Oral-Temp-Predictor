#Loading in the Data / Packages:

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(GGally)
library(corrplot)
library(reshape2)
library(cowplot)
library(randomForest)
library(tibble)
library(gridExtra)
library(grid)
library(naniar)
library(visdat)
library(caret)
library(glmnet)
library(torch)
library(luz)


flir = read_csv("./Data/FLIR_groups1and2.csv", skip = 2)
ici = read_csv("./Data/ICI_groups1and2.csv", skip = 2)

flir = flir[-2]
ici = ici[-2]
    #Removing the second column which contained no information

flir = flir %>% select(-c("...30", "...58", "...86", "...114"))
ici = ici %>% select(-c("...30", "...58", "...86", "...114"))



#Processing the Data 

#Converting to mean infrared data 

a_flir = colnames(flir)[2:28]
    #This is a list of strings 

a_flir = str_sub(a_flir, 1, -2)
    #This is creating a list of the column names without the identifying number (the number of the repeat)

flir_mean_variables = list()
    #To store the variables 
count = 1
for (col in seq(2,28)){
    #This will iterate through every column index I need 

    var_name = paste0(a_flir[count], "_mean")
    var_data = flir[c(col, col + 27, col + 54, col + 81)]
        #This produces a dataframe with just the repeat features 
        #I just need to calculate the colmeans

    flir_mean_variables[[var_name]] = rowMeans(var_data, na.rm = TRUE)
    count = count + 1
}

flir_mean = as.data.frame(flir_mean_variables)

flir_mean$aveOralM = flir[["aveOralM"]]

a_ici = colnames(ici)[2:28]
    #This is a list of strings 

a_ici = str_sub(a_ici, 1, -2)
    #This is creating a list of the column names without the identifying number (the number of the repeat)

ici_mean_variables = list()
    #To store the variables 
count = 1
for (col in seq(2,28)){
    #This will iterate through every column index I need 

    var_name = paste0(a_flir[count], "_mean")
    var_data = flir[c(col, col + 27, col + 54, col + 81)]
        #This produces a dataframe with just the repeat features 
        #I just need to calculate the colmeans

    ici_mean_variables[[var_name]] = rowMeans(var_data, na.rm = TRUE)
    count = count + 1
}

ici_mean = as.data.frame(ici_mean_variables)

ici_mean$aveOralM = flir[["aveOralM"]]



#Baseline Models 
    #TO compare the different datasets 

baseline_model_ici = lm(data = ici_mean, aveOralM ~.)
summary(baseline_model_ici)

baseline_model_flir = lm(data = flir_mean, aveOralM ~ .)
summary(baseline_model_flir)



#Counting NA values 

flir_na_values = list()

for(col in colnames(flir)){
    flir_na_values[[col]] = sum(is.na(flir[col]))
}

flir_na_values = data.frame(t(sapply(flir_na_values,c)))

ici_na_values = list()

for(col in colnames(ici)){
    ici_na_values[[col]] = sum(is.na(ici[col]))
}

ici_na_values = data.frame(t(sapply(ici_na_values,c)))

ici_na_values_long = data.frame(
  Variable = colnames(ici_na_values),
  var_missing = as.numeric(ici_na_values[1, ])
)
ici_na_values_long$data = "ICI"

flir_na_values_long = data.frame(
  Variable = colnames(flir_na_values),
  var_missing = as.numeric(flir_na_values[1, ])
)
flir_na_values_long$data = "FLIR"

na_values_long = rbind(ici_na_values_long, flir_na_values_long)


ggplot(data = na_values_long, aes(x = Variable, y = var_missing, fill = data)) +

geom_bar(stat = "identity", position = "dodge") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
labs(title = "Comparing Missing Values in ICI and FLIR",
    x = "Variable",
    y = "Number of Missing Values") +
theme_bw()

print(paste0("Number of missing values in flir: ", sum(is.na(flir))))
print(paste0("Number of missing values in ici: ", sum(is.na(ici))))


#Plotting the temporal nature of the data 
    #Using the mean values at every time interval
    #You can see how it changes with every value 

flir_infrared = flir %>%
select(-c("SubjectID", "aveOralM","aveOralF", "Gender", "Age", "Ethnicity", "T_atm", "Humidity", "Distance", "Cosmetics", "Time", "Date"))

mean_infrared = data.frame(means = colMeans(flir_infrared, na.rm = TRUE))
mean_infrared = rownames_to_column(mean_infrared)
mean_infrared$time = str_sub(mean_infrared$rowname, -1, -1)
mean_infrared$Group = substr(mean_infrared$rowname, 1, nchar(mean_infrared$rowname)-1)
mean_infrared$time = as.numeric(mean_infrared$time)

mean_infrared = mean_infrared %>% 
filter(Group != "T_offset")
ggplot(data = mean_infrared, aes(x = time, y = means, col = Group)) +
geom_line() +
theme_bw()


#Performing median imputation

flir_clean = flir

for(column in 1:ncol(flir_clean)){
    if (is.numeric(flir_clean[[column]])) {
        flir_clean[[column]][is.na(flir_clean[[column]])] = median(flir_clean[[column]], na.rm = TRUE)
        print(column)
    }
}
sum(is.na(flir_clean))


#Time related analysis 
#Is there a significatn change between each of the variables for each time?
    #using friedman.test (as non parametrc, comparing 3+ groups from matched pairs)

    #If there is it may be indicative of a linear relationship between time and the values of the measurements 

infrared_flir_clean = flir_clean %>%
select(-c("SubjectID", "Gender", "Age", "Ethnicity", "T_atm", "Humidity", "Cosmetics", "Time", "Date", "Distance", "aveOralM", "aveOralF"))

#head(infrared_flir_clean)
count = 1

for(value in 1:27){
    data = infrared_flir_clean[c(count, count + 27, count + 54, count + 81)]
    #data_long = data %>%
    #pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
    test = friedman.test(data.matrix(data))
    print(test)

    count = count + 1
}

#Correlation with time 

infrared_flir_clean = flir_clean %>%
select(-c("SubjectID", "Gender", "Age", "Ethnicity", "T_atm", "Humidity", "Cosmetics", "Time", "Date", "Distance", "aveOralM", "aveOralF"))

#head(infrared_flir_clean)
count = 1

for(value in 1:27){
    data = infrared_flir_clean[c(count, count + 27, count + 54, count + 81)]
    colnames(data) = c(1,2,3,4)
    
    data_long = data %>%
    pivot_longer(cols = everything(), names_to = "Time", values_to = "Value")
        #This converts the data just seperated by time 

    cor = cor.test(as.numeric(data_long$Time), data_long$Value, method = "spearman")
    print(paste0("The correlation score for variable ", colnames(infrared_flir_clean[count]), " is:"))
    print(cor)
    count = count + 1
}


#Missingness Relationship 

gg_miss_var(flir)

#Is missingness related to the target variable 
    #Loop over eevry column 
    #extract temps for groups of missing / not missing 
    #Is there a difference 

for (column in colnames(flir)){
    if(column == "aveORalM"){
        next}

    n_na = sum(is.na(flir[[column]]))
    n_non_na = sum(!is.na(flir[[column]]))
    
    if(n_na > 0 && n_non_na > 0){
        #Only iterates if there are NA values 
        na_flag = is.na(flir[[column]])
    
        # Wilcoxon rank-sum test: compares target between missing vs non-missing
        result = wilcox.test(aveOralM ~ na_flag, data = flir)
        if (result[3] < 0.05){
            print(paste0("Column ", column, " has significant changes in NA and non NA groups (p-value = ", result[3]))
        }
    }
}

#OUtlier Value detection 

for(feature in colnames(select(flir_clean, -c("SubjectID", "Gender", "Age", "Ethnicity", "Cosmetics", "Time", "Date")))){
    summary = fivenum(flir_clean[[feature]])
    
    Q1 = summary[2]
    Q3 = summary[4]
    IQR = Q3 - Q1

    outlier_values = flir_clean[[feature]][flir_clean[[feature]] < (Q1 - (3 * IQR)) | flir_clean[[feature]] > (Q3 + (3 * IQR))]

    print(paste0("The Extreme Values for ", feature, " are: "))
    print(outlier_values)
    print("-------------------------------------------------------------------")
    print("")
}

#EDA

#Plotting infrared variables 

infrared_flir_clean = flir_clean %>%
select(-c("SubjectID", "Gender", "Age", "Ethnicity", "T_atm", "Humidity", "Cosmetics", "Time", "Date", "Distance", "aveOralM", "aveOralF"))

#head(infrared_flir_clean)
count = 1
plot_list = list() 

for(value in 1:27){
    data = infrared_flir_clean[c(count, count + 27, count + 54, count + 81)]
    data_long = data %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

    p = ggplot(data = data_long, aes(x = Variable, y = Value)) + 
    geom_boxplot() +
    ggtitle(paste("Boxplot for Feature", colnames(infrared_flir_clean)[value])) +
    theme_bw()
    assign(paste0("plot_", count), p)
    plot_list[[value]] = p

    count = count + 1
}

options(repr.plot.width = 10, repr.plot.height = 25)

grid.arrange(grobs = plot_list, ncol = 3)

options(repr.plot.width = NULL, repr.plot.height = NULL)


#Repeating for non infrared variables 
par(mfrow = c(1,3))

boxplot(flir_clean["T_atm"])
boxplot(flir_clean["Humidity"])
boxplot(flir_clean["Distance"])


age = ggplot(data = flir_clean, aes(x = Age)) +
geom_bar(stat = "count") +
theme_bw()

eth = ggplot(data = flir_clean, aes(x = Ethnicity)) +
geom_bar(stat = "count") +
theme_bw()

gen = ggplot(data = flir_clean, aes(x = Gender)) +
geom_bar(stat = "count") +
theme_bw()

cos = ggplot(data = flir_clean, aes(x = Cosmetics)) +
geom_bar(stat = "count") +
theme_bw()

hum = ggplot(data = flir_clean, aes(x = Humidity)) +
geom_histogram() +
theme_bw()

atm = ggplot(data = flir_clean, aes(x = T_atm)) +
geom_histogram() +
theme_bw()

plot_grid(age, eth, gen, cos, hum, atm) + theme_bw()


#Label distribution 

oral_hist = ggplot(data = flir_clean, aes(x = aveOralM)) +
geom_histogram() +
theme_bw()

oral_box = ggplot(data = flir_clean, aes(y = aveOralM)) +
geom_boxplot() +
theme_bw()

plot_grid(oral_hist, oral_box)

shapiro.test(as.numeric(flir_clean[["aveOralM"]]))


#Normality of variables 
#head(infrared_flir_clean)

scores = list()

for(column in colnames(infrared_flir_clean)){
    sw = shapiro.test(as.numeric(infrared_flir_clean[[column]]))
    #print(sw)
    p_value = round(sw$p.value, 3) 
    scores[[column]] = p_value
}
scores


#Colinearity 

corrplot(cor(flir_mean, method = "spearman"), diag = FALSE, type = "upper")


cor_data = cor(flir_mean, method = "spearman")
cor_data = melt(cor_data)
cor_data = cor_data %>% filter(Var2 == "aveOralM")

ggplot(data = cor_data, aes(x = Var2, y = factor(Var1, levels = Var1[order(value, decreasing = FALSE)]), fill = value)) +
geom_tile() +
scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1)) +
theme_bw() +
theme(axis.text.x = element_text(angle = 45, hjust = 1),
axis.title.y = element_blank(),
axis.title.x = element_blank()) +  
labs(title = "Correlation of Oral Temperature with Infrared variables", fill = "Correlation") +
geom_text(aes(x = Var2, y = Var1, label = round(value, 2)), color = "black", size = 4, angle = 0) 


#Catehhorical data analysis 
age = ggplot(data = flir_clean, aes(x = Age, y = aveOralM)) +
geom_boxplot() + 
theme_bw()

eth = ggplot(data = flir_clean, aes(x = Ethnicity, y = aveOralM)) +
geom_boxplot() + 
theme_bw()

cos = ggplot(data = flir_clean, aes(x = as.factor(Cosmetics), y = aveOralM)) +
geom_boxplot() + 
theme_bw()

plot_grid(age, eth, cos)

#GEtting KW test results 

kruskal.test(aveOralM ~ Age, data = flir_clean)

kruskal.test(aveOralM ~ Ethnicity, data = flir_clean)

kruskal.test(aveOralM ~ as.factor(Cosmetics), data = flir_clean)


#Feature permutation to determine Importance 

#RF featurepermutation 

a_flir = colnames(flir)[2:28]
    #This is a list of strings 

a_flir = str_sub(a_flir, 1, -2)
    #This is creating a list of the column names without the identifying number (the number of the repeat)

flir_mean_variables = list()
    #To store the variables 
count = 1
for (col in seq(2,28)){
    #This will iterate through every column index I need 

    var_name = paste0(a_flir[count], "_mean")
    var_data = flir[c(col, col + 27, col + 54, col + 81)]
        #This produces a dataframe with just the repeat features 
        #I just need to calculate the colmeans

    flir_mean_variables[[var_name]] = rowMeans(var_data, na.rm = TRUE)
    count = count + 1
}

flir_fs_nn = as.data.frame(flir_mean_variables)

flir_fs_nn$aveOralM = flir[["aveOralM"]]
flir_fs_nn$Gender = flir[["Gender"]]
flir_fs_nn$Ethnicity = flir[["Ethnicity"]]
flir_fs_nn$T_atm = flir[["T_atm"]]
flir_fs_nn$Humidity = flir[["Humidity"]]
flir_fs_nn$Distance = flir[["Distance"]]
flir_fs_nn$Cosmetics = flir[["Cosmetics"]]
flir_fs_nn$Age = flir[["Age"]]

flir_fs_nn = na.omit(flir_fs_nn)

set.seed(42)

rf_feature_imp = randomForest(aveOralM ~ ., data = flir_fs_nn, importance = TRUE)

importance(rf_feature_imp)

fi = importance(rf_feature_imp)

fi = data.frame(fi)

fi = cbind(Variable = rownames(fi), fi)
rownames(fi) = 1:nrow(fi)

ggplot(data = fi, aes(x = Variable, y = IncNodePurity, fill = IncNodePurity)) +
geom_bar(stat = "identity") +
theme_bw() +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
labs(y = "Increase in Node Purity", 
    title = "Increase in Node Purity by Variable")

ggplot(data = fi, aes(x = Variable, y = X.IncMSE, fill = X.IncMSE)) +
geom_bar(stat = "identity") +
theme_bw() +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
labs(y = "Increase in MSE", 
    title = "Increase in MSE by Variable") +
geom_hline(yintercept = mean(fi$X.IncMSE), color = "red", linetype="dashed")


sig_var = fi$Variable[fi$X.IncMSE > mean(fi$X.IncMSE)]
sig_var = setdiff(sig_var, c("Gender", "T_atm"))
    #Removes the non infrared variables (as I wont have to convert this back to 1-4)
    #I have added these back below

all_sig_var = list()

for(var in sig_var){
    var = str_replace(var, "_mean", "")
        #Removing _mean
    for (num in 1:4){
        adj_var = paste0(var, num)
        all_sig_var = append(all_sig_var, adj_var)
            #Appending 1-4 at end of list 
    }
}

all_sig_var = unlist(all_sig_var)
    #Converting from a list 

nn_data = flir_clean %>%
select(all_of(all_sig_var))

nn_data = cbind(nn_data, flir[,c("Gender", "T_atm", "aveOralM")])

head(nn_data)
    #This is the data that I will use fro my NN model 


reg_perm =  cbind(flir_mean, select(flir, c(Gender, Age, Ethnicity, T_atm, Humidity, Distance, Cosmetics)))
    #Now contains all reelevent predictors and the response 

reg_perm = na.omit(reg_perm)


#Function for train test split 
    #To reduce on redundant code 
set.seed(42)

tts = function(data) {
    ind = sample(1 : nrow(data), floor(nrow(data) * 0.20), replace = FALSE)
    #I want the test set to comprise of 20% of the data
    #Generates indexes for the test set

    test = data[ind,]
    train = data[-ind,]
    
    print(paste0("Test Data Dimensions: ", dim(test)))
    print(paste0("Train Data Simensions: ", dim(train)))

    return(list(train = train, test = test))
}


set.seed(42)

result = tts(reg_perm)

# Extract the train and test datasets
train = result$train
test = result$test

#Base MSE 

base_reg = lm(aveOralM ~ ., data = train)
    #This is the baseline model with none of the data shuffled
    #Calculate MSE 

sum = summary(base_reg)

base_pred = data.frame(pred = predict(base_reg, newdata = select(test, - aveOralM)), actual = test$aveOralM)
    #Creates a dataframe of my predicted and actual values 
#head(base_pred)

base_mse = mean((base_pred$actual - base_pred$pred)^2)
base_mse
    #This is the MSE based on all of the data
    #This is what I am going to be comparing the results to to find the increase in MSE 



set.seed(42)

mse_results = list()

for(column in 1:ncol(test)){
        #Iterates over every column  of the dataframe 
    copy = data.frame(test)
        #This makes a copy of the dataframe so that it does not change the original 
    if(colnames(copy)[column] == "aveOralM"){
        next
    }
        #This will automatically skip over the permutation for the label (will obviously cause a big MSE increase 

    copy[, column] =sample(copy[, column])  
        #This shuffles the tyest dataset
        #Shuffles the column the data is already on

    pred_df = data.frame(pred = predict(base_reg, newdata = select(copy, - aveOralM)), actual = test$aveOralM)
        #Generating predicted values 
        #Saving them in a df alongside the actual values 
    
    MSE = mean((pred_df$actual - pred_df$pred)^2)
        #Calculating MSE 

    incr_mse = MSE - base_mse 
        #Sbtracting the base MSE from the permuted models MSA = to how much the MSE increased

    mse_results[[colnames(copy)[column]]] = incr_mse
    
}

mse_results = t(data.frame(mse_results))
mse_results = data.frame(Variable = rownames(mse_results), MSE = mse_results[,1])

mse_results

ggplot(data = mse_results, aes(x = Variable, y = MSE, fill = MSE)) +
geom_bar(stat = "identity") +
theme_bw() +
labs(y = "Increase in MSE", 
     x = "Variable", 
     title = "Permutation Feature Importance") +
theme(legend.position = "none") +
theme(axis.text.x = element_text(size = 10, angle = 90, face = "bold", hjust = 1, vjust = 0.5)) +
geom_hline(yintercept = mean(mse_results$MSE), color = "red", linetype="dashed")



perm_imp_features = mse_results %>%
filter(mse_results$MSE > mean(mse_results$MSE))

reg_perm_cor_data = cor_data %>% 
filter(Var1 %in% c("Max1R13__mean", "T_RC_Max_mean", "T_LC_mean", "T_LC_Max_mean",
                  "canthiMax_mean", "canthi4Max_mean","T_Max_mean"))

non_imp_cor_data = cor_data %>%
filter(!(Var1 %in% rownames(perm_imp_features)))

imp = ggplot(data = reg_perm_cor_data, aes(x = Var2, y = factor(Var1, levels = Var1[order(value, decreasing = FALSE)]), fill = value)) +
geom_tile() +
scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1)) +
theme_bw() +
theme(axis.text.x = element_text(angle = 45, hjust = 1),
axis.title.y = element_blank(),
axis.title.x = element_blank()) +  
labs(title = "Correlation To Oral Temp") +
geom_text(aes(x = Var2, y = Var1, label = round(value, 2)), color = "black", size = 4, angle = 0) +
theme(legend.position = "none")

reg_perm_cor_data_1 = cor_data %>% 
filter(!(Var1 %in% c("T_RC_Dry_mean", "T_FHLC_mean", "T_FH_Max_mean", "T_Max_mean", "T_FHC_Max_mean")))

non_imp = ggplot(data = non_imp_cor_data, aes(x = Var2, y = factor(Var1, levels = Var1[order(value, decreasing = FALSE)]), fill = value)) +
geom_tile() +
scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1)) +
theme_bw() +
theme(axis.text.x = element_text(angle = 45, hjust = 1),
axis.title.y = element_blank(),
axis.title.x = element_blank()) +  
labs(title = "Correlation To Oral Temp") +
geom_text(aes(x = Var2, y = Var1, label = round(value, 2)), color = "black", size = 4, angle = 0) +
#geom_text(aes(x = Var2, y = Var1, label = round(mse_results[Var1, MSE], 4))) +
theme(legend.position = "none")

plot_grid(imp, non_imp)



reg_important_features = c(
    "Max1R13_1", "Max1R13_2", "Max1R13_3", "Max1R13_4",
    "T_RC_Max1", "T_RC_Max2", "T_RC_Max3", "T_RC_Max4",
    "T_LC1", "T_LC2", "T_LC3", "T_LC14",
    "T_LC_Max1", "T_LC_Max2", "T_LC_Max3", "T_LC_Max4",
    "canthiMax1", "canthiMax2", "canthiMax3", "canthiMax4",
    "canthi4Max1", "canthi4Max2", "canthi4Max3", "canthi4Max4",
    "T_Max1", "T_Max2", "T_Max3", "T_Max4",
    "aveOralM")
    #This is the list of all the important features I will be using 

reg_data = flir_clean %>%
select(all_of(reg_important_features))
    #Selecting only the important eatures determined by feature permutation in my lm model 

corrplot(cor(reg_data, method = "spearman"), diag = FALSE, type = "upper")



#PCA for regression

#Function to transform the data
to_long = function(train, col_names) {
  df = data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(df) = col_names 
        #Initialising df to store our data in 
  
  for (row in 1:nrow(train)) {
          #Iterating over every row of the trianing data
    for (time in 1:4) {
            #Iterating over every time (1-4)
      time = matrix(time)  
      selected_columns = grep(paste0(time, "$"), colnames(train), value = TRUE)
            #Only selecting columns in the right time 
            #By finding what they end with (usign regex)
      
      extracted_values = train[row , selected_columns]
            #Subsetting the data by time 
      
      #extracted_values = cbind(extracted_values, train[row, "T_atm"])
        #I do not what to append the T_atm 
        #As this is not temporal data -> should not be inlcuded in the PCA 
        #Because it is not highly correlated with the other predictors
      data = cbind(time, extracted_values)
      colnames(data) = col_names
      
      df = rbind(df, data)
            #Binding the matrix to the global datagrame 
    }
  }
  
  # Return the final transformed data frame
  return(df)
}

#I need to convert it to long format to retain all of the information whilst still reducing the dimensions 


#Function to convert the long PCA data into short 
to_short = function(data, col_name){
    mat = data.frame(matrix(ncol = length(col_names), nrow = 0))
        #Initialising the global df 
    colnames(mat) = col_names
    
    count = 1
    
    for (i in seq(0, nrow(data), 4)){
        #Iterating over every 4th row (as this represents a new individual)
        temp = matrix(count)
            #A new temp matrix every time to store the values 
        for (time in 1:4) {
    
            temp = cbind(temp, select(data[(i + time),], - Time))
            #print(temp)
            #colnames(mat) = col_names
            }
    #temp = select(temp, -1)
        
        mat = rbind(mat, temp)
        count = count + 1
        
    }
    colnames(mat) = col_names
    
    mat = mat %>% filter(row_number() <= n()-1)
        #Removing the last row 
    mat = mat[,-1]
        #Removing the first column
}


col_names = c("Time", "Max1R13", "T_RCMax", "T_LC", "T_LC_Max", "canthiMax", "canthi4Max", "T_Max")

reg_pca = to_long(reg_data, col_names)
    #Converting training data into  a long format so we can apply PCA 

reg_pca_vectors = prcomp(select(reg_pca, -Time), scale = TRUE)

summary(reg_pca_vectors)

par(mfrow = c(1,2))

plot(reg_pca_vectors, col = "steelblue", main = "Variance Explained in PC's")

c_p = summary(reg_pca_vectors)$importance[3, ]

barplot(c_p, col = rgb(70/255, 130/255, 180/255, 0.6), 
        xlab = "Principal Component", ylab = "Cumulative Var Explained",
        main = "Cumulative Proportion of Variance Explained",
        ylim = c(0, 1), names.arg = paste0("PC", seq_along(c_p)), las = 2, cex.names = 0.7)

abline(h = 0.95, col = "red", lty = 2)

sel_reg_pcas = reg_pca_vectors$x[,1:2]
    #Selecting the first 4 principal components to sxplain most of the variance in the data
sel_reg_pcas = as.data.frame(sel_reg_pcas)
sel_reg_pcas = cbind(sel_reg_pcas, reg_pca["Time"])
    #This contains the PC values for each data point 
    #Including a time variable for the placement back into short format 


col_names = c("Number", 
              "PC1_1", "PC2_1",
              "PC1_2", "PC2_2",
             "PC1_3", "PC2_3",
             "PC1_4", "PC2_4")

    #I need to change this depending on how many PC's I am choosing to use 

mat = to_short(sel_reg_pcas, col_names)

final_reg_data = cbind(mat, reg_data[29])

head(final_reg_data)
    #This is the PC data returned back into short data form 
    #It has retained the temporal nature by having 2 PC's for every time point 


#Regression Model:

set.seed(42)

result = tts(final_reg_data)

# Extract the train and test datasets
train = result$train
test = result$test

set.seed(42)

lambdas=10^seq(-3,3,by=0.2)
alphas = seq(0,1, by = 0.05)
    #Assigning different values of alpha and lmabda to test

results = expand.grid(alpha = alphas)
    #This stores a dataframe of all the combinations, 
    #For every result of the two I can stroe it in a new column

ctrl_kfold = trainControl(method = "cv", number = 10)
    #Setting up cross validation

for (parameters in 1: nrow(results)){
    alpha = results[parameters, 1]

    elastic_model = train(aveOralM ~ ., 
                              #The target variabele
                    data = train,
                            #This is my training data 
                    method = "glmnet",
                    metric = "RMSE",
                    tuneGrid = expand.grid(alpha = alpha,
                                     lambda = lambdas),
                    trControl = ctrl_kfold,
                    thresh = 1e-10)
    #print(elastic_model)
    
    elastic_lambda = elastic_model$bestTune$lambda
        #This finds the best value of lambda for the particular alpha 

    #print(elastic_lambda)
    elastic_RMSE = elastic_model$results %>% filter(lambda == elastic_lambda)
    elastic_RMSE = elastic_RMSE$RMSE

    results[parameters, "lambda"] = elastic_lambda

    results[parameters, "RMSE"] = elastic_RMSE
}

print(paste0("The best hyperparameters gave a RMSE of: ", round(min(results$RMSE), 3)))
print(paste0("This occured at index: ", which.min(results$RMSE)))
print(paste0("Alpha Value of: ", results[which.min(results$RMSE), "alpha"]))
print(paste0("Lambda Value of: ", results[which.min(results$RMSE), "lambda"]))

reg_alpha = results[which.min(results$RMSE), "alpha"]
reg_lambda =  results[which.min(results$RMSE), "lambda"]

ggplot(data = results, aes(x = alpha, y = RMSE)) +
geom_line(color = "steelblue", alpha = 0.8) +
theme_bw() +
geom_point(aes(x = alpha, y = RMSE, size = lambda, color = lambda ))


set.seed(42)

x_train = select(train, - aveOralM)
y_train = select(train, aveOralM)
y_train = as.numeric(unlist(y_train))
x_test = select(test, - aveOralM)
y_test = select(test, aveOralM)

print(dim(x_train))
print(length(y_train))
print(dim(x_test))
print(dim(y_test))

elastic_net_model = glmnet(as.matrix(x_train), y_train, alpha = reg_alpha , lambda = reg_lambda)

coef(elastic_net_model)

reg_predictions = predict(elastic_net_model, newx = as.matrix(x_test))

reg_predictions = cbind(reg_predictions, y_test)

head(reg_predictions)


reg_mse = mean((reg_predictions$s0 - reg_predictions$aveOralM)^2, na.rm = TRUE)

print(paste0("The MSE for the elastic net regression model is: ", reg_mse))


#PCA for NN 

#Creating dummy variable for gender 

du_nn = dummyVars(~ Gender, data = nn_data)

du_nn_data = predict(du_nn, newdata = nn_data)

du_nn_data = as.data.frame(du_nn_data)

nn_data = cbind(nn_data, du_nn_data) 

nn_data = select(nn_data, - Gender)

set.seed(42)

result = tts(nn_data)
    #Splitting the data into train and test 
    #Prior to PCA 
        #SO that the test PCA does not influence the transformation

# Extract the train and test datasets
nn_train = result$train
nn_test = result$tes

head(nn_train)

#Converting into long format 
a = colnames(nn_train[,-c(61:64)])
    #Removing the non-infrared data from the dimensionality reduction 
        #Atmospheric temp, Gender, and the oral temp (label)

b = a[seq(1, length(a), 4)]
#b = b[-13]
col_names = c("Time")

for (i in b){
    i = str_sub(i, end = -2)
    col_names = append(col_names, i)
}

print(col_names)
    #Extracting all of the infrared colnames 

nn_train_pca = to_long(nn_train, col_names)
    #Using my to_long function

#nn_train_pca = nn_train_pca[,-16]


#Applyoing PCA 
nn_pca = prcomp(select(nn_train_pca, - Time), scale = TRUE)
summary(nn_pca)

par(mfrow = c(1,2))

plot(nn_pca, col = "steelblue", main = "Variance Explained in PC's")

c_p = summary(nn_pca)$importance[3, ]

barplot(c_p, col = rgb(70/255, 130/255, 180/255, 0.6), 
        xlab = "Principal Component", ylab = "Cumulative Var Explained",
        main = "Cumulative Proportion of Variance Explained",
        ylim = c(0, 1), names.arg = paste0("PC", seq_along(c_p)), las = 2, cex.names = 0.7)

abline(h = 0.95, col = "red", lty = 2)

sel_nn_pcas = nn_pca$x[,1:4]
    #Selecting the first 4 principal components to sxplain most of the variance in the data
sel_nn_pcas = as.data.frame(sel_nn_pcas)
sel_nn_pcas = cbind(sel_nn_pcas, nn_train_pca["Time"])
head(sel_nn_pcas)
dim(sel_nn_pcas)

col_names = c("Number", 
              "PC1_1", "PC2_1", "PC3_1", "PC4_1",
              "PC1_2", "PC2_2", "PC3_2", "PC4_2",
              "PC1_3", "PC2_3", "PC3_3", "PC4_3",
              "PC1_4", "PC2_4", "PC3_4", "PC4_4")

nn_pca_train = to_short(sel_nn_pcas, col_names)
head(nn_pca_train)

nn_training_data = cbind(nn_pca_train, nn_train[,61:64])
    #Binds atm_temp, oral_temp, and gender 

head(nn_training_data)


#PCA on NN test data 

a = colnames(nn_test[,-c(61:64)])

b = a[seq(1, length(a), 4)]
col_names = c("Time")

for (i in b){
    i = str_sub(i, end = -2)
    col_names = append(col_names, i)
}
#col_names = append(col_names, "T_atm")
print(col_names)

nn_test_pca_data = to_long(nn_test, col_names)

nn_test_pcas = predict(nn_pca, newdata = select(nn_test_pca_data, - Time))

test_nn_pcas = nn_test_pcas[, 1:4]
    #Selecting the first 6 principal components to sxplain most of the variance in the data
test_nn_pcas = as.data.frame(test_nn_pcas)
test_nn_pcas = cbind(test_nn_pcas, nn_test_pca_data["Time"])

col_names = c("Number", 
              "PC1_1", "PC2_1", "PC3_1", "PC4_1", 
              "PC1_2", "PC2_2", "PC3_2", "PC4_2", 
              "PC1_3", "PC2_3", "PC3_3", "PC4_3",
              "PC1_4", "PC2_4", "PC3_4", "PC4_4")

nn_pca_test = to_short(test_nn_pcas, col_names)

nn_test_data = cbind(nn_pca_test, nn_test[,61:64])

head(nn_test_data)


#MLP Construction

set.seed(42)

mlp_train_test = tts(nn_training_data)
    #Creating test and training data 
    #THe other data will be used as my validation data 

dim(mlp_train_test$train)
dim(mlp_train_test$test)

mlp_train = mlp_train_test$train
mlp_test = mlp_train_test$test
mlp_validation = nn_test_data

mlp_train = na.omit(mlp_train)

torch_x = torch_tensor(as.matrix(mlp_train[,-18]),
                      dtype = torch_float())
    #ONly converting the training data into a torch 

torch_y = torch_tensor(as.numeric(mlp_train[,18]),
                        dtype = torch_float())

torch_y = torch_y$view(c(653, 1))
    #Converting the format of the label torch that matches the y_pred 


torch_test_x = torch_tensor(as.matrix(mlp_test[,-18]),
                            dtype = torch_float())

torch_test_y = torch_tensor(as.numeric(mlp_test[,18]), 
                            dtype = torch_float())
torch_test_y = torch_test_y$view(c(163,1))

print(dim(torch_x))
print(dim(torch_y))


print(dim(torch_test_x))
print(dim(torch_test_y))


batch_train = function(model, epochs, optimiser, criterion, x_data, y_data, batch_size){

    torch_manual_seed(42)

    num_batches = ceiling(nrow(x_data) / batch_size)
        #Finding the number of batches from the batch size 
        #Round up so you dont have a really small batch at the end
    
    epoch_results = data.frame(
    epoch = integer(),
    train_mse = numeric(),
    test_mse = numeric())
    
    for (i in 1:epochs) {

        for (batch in seq_len(num_batches)){
            #Iterates over every batch
            start = (batch - 1) * batch_size + 1
                #b-1 -> starts at 0
                #bs + 1 -> doesnt overlap 
            end   = min(batch * batch_size, nrow(torch_x))
                #Either ends at the end of the batch or the end of the df 

            batch_x = torch_x[start:end,]
            batch_y = torch_y[start:end,]
                #Exteacting the batches for label and feature data 
            
            optimiser$zero_grad()  
                #Resets the gradients 
            y_pred = model(batch_x) 
                #predicting the values for the batch features 
            loss = criterion(y_pred, batch_y) 
                #Calculates loss based on the given crietrion
                #Compared to labels from same batch
            loss$backward() 
                #Backward propogation
            optimiser$step()
                #Next step 
        }
          
        #Tracking model performance 
        MSE = torch_mean((y_pred - batch_y)**2)
            #This returns a torch tensor 
        MSE_value = MSE$item()
            #You have to extract the value in order to paste it 
        RMSE_value = sqrt(MSE_value)

        print(paste0("Epoch: ", i))
        print(paste0("Training MSE: ", MSE_value))
        print(paste0("Training RMSE: ", RMSE_value))

        #Finding test metrixs:
        test_pred = model(torch_test_x)
        test_MSE = torch_mean((test_pred - torch_test_y)**2)
        test_MSE_value = test_MSE$item()
        
        print(paste0("Test MSE: ", test_MSE_value))
        print("======================================")

        epoch_results = rbind(epoch_results, data.frame(epoch = i, train_mse = MSE_value, test_mse = test_MSE_value))

    }
    return(epoch_results)
}

#This is a function for batch training 

#Testing Learnging Rate 
torch_manual_seed(42)

criterion = nn_mse_loss()


mlp_results = data.frame(lr = numeric(), 
                     epoch = integer(), 
                     train_mse = numeric(), 
                     test_mse = numeric())

for(learn in seq(0.001, 0.1, length.out = 10)){
    #Iterates over the LR's I want to test 
    mlp_model = nn_sequential(
        nn_linear(19, 15),
        nn_relu(),
            #The activation function is only applied to the hidden layers 
            #Allows you to capture non linear dependencies 
        nn_dropout(0.3),
            #Dropout function that prevents overfitting -> increased regularisation (default = 0.3)
        nn_linear(15, 1)
            #No activation function on the output 
        )
        #Resets the mlp model every iteration (resets the parameters)
    
    optimiser = optim_adam(mlp_model$parameters, lr = learn)
    epoch_results = batch_train(mlp_model, 200, optimiser, criterion, torch_x, torch_y, 256)
    epoch_results$lr = learn

    mlp_results = rbind(mlp_results, epoch_results)
    
}

start = ggplot(data = filter(mlp_results, lr != 0.001), aes(x = epoch, y = test_mse, color = factor(lr))) +
geom_smooth(se = FALSE, span = 0.4) +theme_bw() +
xlim(0, 100)

end = ggplot(data = filter(mlp_results, lr != 0.001), aes(x = epoch, y = test_mse, color = factor(lr))) +
geom_smooth(se = FALSE, span = 0.4) +
theme_bw() + 
xlim(100,200)

grid.arrange(start, end)

ggplot(data = filter(mlp_results, lr == "0.067")) +
geom_smooth(aes(x = epoch, y = train_mse, color = "Train"), se = FALSE, span = 0.3) +
geom_smooth(aes(x = epoch, y = test_mse, color = "Test"), se = FALSE, span = 0.3) +
theme_bw() 


torch_manual_seed(42)

criterion = nn_mse_loss()

sizes = c(16, 32, 64, 128, 256)

#TEtsing batch sizes 
batch_mlp_results = data.frame(batch = numeric(), 
                     epoch = integer(), 
                     train_mse = numeric(), 
                     test_mse = numeric())

for(batch_size in sizes){
    mlp_model = nn_sequential(
        nn_linear(19, 15),
        nn_relu(),
        nn_dropout(0.3),
        nn_linear(15, 1)
        )

    optimiser = optim_adam(mlp_model$parameters, lr = 0.067)
        #using the optimal LR 
    epoch_results = batch_train(mlp_model, 200, optimiser, criterion, torch_x, torch_y, batch_size)
    epoch_results$batch = batch_size 

    batch_mlp_results = rbind(batch_mlp_results, epoch_results)
    
}

#PLotting MLP Batch Results 

start = ggplot(data = batch_mlp_results, aes(x = epoch, y = test_mse, col = factor(batch))) +
geom_smooth(se = FALSE, span = 0.3) + 
theme_bw() +
xlim(0,100)

end = ggplot(data = batch_mlp_results, aes(x = epoch, y = test_mse, col = factor(batch))) +
geom_smooth(se = FALSE, span = 0.3) + 
theme_bw() + 
xlim(100, 200)


grid.arrange(start, end)


#Testing Drop Rates 

torch_manual_seed(42)

criterion = nn_mse_loss()

sizes = c(0.1, 0.2, 0.3, 0.4, 0.5)


drop_mlp_results = data.frame(batch = numeric(), 
                     epoch = integer(), 
                     train_mse = numeric(), 
                     test_mse = numeric())

for(drop_rate in sizes){
    mlp_model = nn_sequential(
        nn_linear(19, 15),
        nn_relu(),
        nn_dropout(drop_rate),
        nn_linear(15, 1)
        )

    optimiser = optim_adam(mlp_model$parameters, lr = 0.067)
        #using the optimal LR 
    epoch_results = batch_train(mlp_model, 500, optimiser, criterion, torch_x, torch_y, 32)
    epoch_results$loss = drop_rate 

    drop_mlp_results = rbind(drop_mlp_results, epoch_results)
    
}

start = ggplot(data = drop_mlp_results, aes(x = epoch, y = test_mse, color = factor(loss))) +
geom_smooth(se = FALSE, span = 0.3) +
theme_bw() +
xlim(0, 100)

end = ggplot(data = drop_mlp_results, aes(x = epoch, y = test_mse, color = factor(loss))) +
geom_smooth(se = FALSE, span = 0.3) +
theme_bw() +
xlim(100, 500)

grid.arrange(start, end)


#Final MLP Model
train_mlp_model = function(model, epochs, optimiser, criterion, x_data, y_data, batch_size){

    torch_manual_seed(42)

    num_batches = ceiling(nrow(x_data) / batch_size)
        #Finding the number of batches from the batch size 
        #Round up so you dont have a really small batch at the end
    
    epoch_results = data.frame(
    epoch = integer(),
    train_mse = numeric(),
    test_mse = numeric())
    
    for (i in 1:epochs) {

        for (batch in seq_len(num_batches)){
            #Iterates over every batch
            start = (batch - 1) * batch_size + 1
                #b-1 -> starts at 0
                #bs + 1 -> doesnt overlap 
            end   = min(batch * batch_size, nrow(torch_x))
                #Either ends at the end of the batch or the end of the df 

            batch_x = torch_x[start:end,]
            batch_y = torch_y[start:end,]
                #Exteacting the batches for label and feature data 
            
            optimiser$zero_grad()  
                #Resets the gradients 
            y_pred = model(batch_x) 
                #predicting the values for the batch features 
            loss = criterion(y_pred, batch_y) 
                #Calculates loss based on the given crietrion
                #Compared to labels from same batch
            loss$backward() 
                #Backward propogation
            optimiser$step()
                #Next step 
        }
          
        #Tracking model performance 
        MSE = torch_mean((y_pred - batch_y)**2)
            #This returns a torch tensor 
        MSE_value = MSE$item()
            #You have to extract the value in order to paste it 
        RMSE_value = sqrt(MSE_value)

        print(paste0("Epoch: ", i))
        print(paste0("Training MSE: ", MSE_value))
        print(paste0("Training RMSE: ", RMSE_value))

        #Finding test metrixs:
        test_pred = model(torch_test_x)
        test_MSE = torch_mean((test_pred - torch_test_y)**2)
        test_MSE_value = test_MSE$item()
        
        print(paste0("Test MSE: ", test_MSE_value))
        print("======================================")

        epoch_results = rbind(epoch_results, data.frame(epoch = i, train_mse = MSE_value, test_mse = test_MSE_value))

    }
    list(epoch_results = epoch_results, model = model)
}

torch_manual_seed(42)

criterion = nn_mse_loss()

mlp_model = nn_sequential(
    nn_linear(19, 15),
    nn_relu(),
    nn_dropout(0.1),
    nn_linear(15, 1))

optimiser = optim_adam(mlp_model$parameters, lr = 0.067)


result = train_mlp_model(mlp_model, 300, optimiser, criterion, torch_x, torch_y, 32)

best_mlp_model = result$model 
best_mlp_metrics = result$epoch_results

start = ggplot(data = best_mlp_metrics) +
geom_smooth(aes(x = epoch, y = test_mse, color = "Test"), se = FALSE, span = 0.3) + 
geom_smooth(aes(x = epoch, y = train_mse, color = "Train"), se = FALSE, span = 0.3) +theme_bw() +
labs(x = "Epoch", y = "MSE") + xlim(0,150)

end = ggplot(data = best_mlp_metrics) +
geom_smooth(aes(x = epoch, y = test_mse, color = "Test"), se = FALSE, span = 0.3) + 
geom_smooth(aes(x = epoch, y = train_mse, color = "Train"), se = FALSE, span = 0.3) +theme_bw() +
labs(x = "Epoch", y = "MSE") + xlim(150,300)

grid.arrange(start, end)


#MLP Results 

torch_val_x = torch_tensor(as.matrix(select(mlp_validation, - aveOralM)),
                      dtype = torch_float())
torch_val_y = torch_tensor(as.matrix(select(mlp_validation, aveOralM)),
                      dtype = torch_float())

best_mlp_model$eval()

best_mlp_pred = best_mlp_model(torch_val_x)

dim(best_mlp_pred)

best_mlp_df = data.frame(Prediction = as_array(best_mlp_pred), Actual = select(mlp_validation, aveOralM))

best_mlp_df = na.omit(best_mlp_df)

head(best_mlp_df)


#Linear MLP (without a non-linear function)

#Linear RNN 

torch_manual_seed(42)

criterion = nn_mse_loss()

mlp_model = nn_sequential(
    nn_linear(19, 15),
    nn_dropout(0.1),
    nn_linear(15, 1))

optimiser = optim_adam(mlp_model$parameters, lr = 0.067)


result = train_mlp_model(mlp_model, 300, optimiser, criterion, torch_x, torch_y, 32)

linear_mlp_model = result$model 
linear_mlp_metrics = result$epoch_results

ggplot(data = linear_mlp_metrics) +
geom_smooth(aes(x = epoch, y = test_mse, color = "Test"), se = FALSE, span = 0.3) + 
geom_smooth(aes(x = epoch, y = train_mse, color = "Train"), se = FALSE, span = 0.3) +theme_bw() +
xlim(0, 300) 
labs(x = "Epoch", y = "MSE")

#Linear MLP Results

linear_mlp_model$eval()
    #Evaluation mode

torch_val_x = torch_tensor(as.matrix(select(mlp_validation, - aveOralM)),
                      dtype = torch_float())
torch_val_y = torch_tensor(as.matrix(select(mlp_validation, aveOralM)),
                      dtype = torch_float())

dim(torch_val_x)

linear_mlp_pred = linear_mlp_model(torch_val_x)

linear_model_df = data.frame(Prediction = as_array(linear_mlp_pred), Actual = select(mlp_validation, aveOralM))

linear_model_df = na.omit(linear_model_df)
head(linear_model_df)



#RNN Construction

rnn_train = nn_training_data[,-c(20,19,17)]
rnn_test = nn_test_data[,-c(20,19,17)]
    #This is removing the non temporal data from the RNN data

rnn_train = na.omit(rnn_train)
rnn_test = na.omit(rnn_test)
head(rnn_train)
    #These now contain both the features and the labels 
    #The featurees will be specified in the code below

set.seed(42)

rnn_train_test = tts(rnn_train)

rnn_validation = rnn_test
    #The test set is the validation set
rnn_test = rnn_train_test$test
rnn_train = rnn_train_test$train

print(dim(rnn_train))
print(dim(rnn_test))


#Code Adapted from: https://skeydan.github.io/Deep-Learning-and-Scientific-Computing-with-R-torch/time_series.html#data-inspection

#Converting the data into a dataset
    #A format that torch can handle 

demand_dataset = dataset(
  name = "demand_dataset",
  
  initialize = function(data, n_timesteps, sample_frac = 1) {
    self$n_timesteps = n_timesteps
    
    # Separate features and labels
    features = data[, 1:16]   # 24 features
    labels = data[, 17]     # 1 label column
    
    # Convert to torch tensors
    self$features = torch_tensor(as.matrix(features))
    self$labels   = torch_tensor(as.matrix(labels))
          #Extractint the features and labels, converting to a torch tensor 
    
    # Define valid starting points for time windows
    n = nrow(self$features) - self$n_timesteps
    self$starts = sort(sample.int(n = n, size = n * sample_frac))
  },
  
  .getitem = function(i) {
    start = self$starts[i]
    end  = start + self$n_timesteps - 1
    
    list(
      # shape: [n_timesteps, 24]
      x = self$features[start:end, ],
      
      # shape: [1] (the "next" label after the window)
      y = self$labels[end + 1]
    )
  },
  
  .length = function() {
    length(self$starts)
  }
)

n_timesteps = 4

a = as.matrix(rnn_train)
b = as.matrix(rnn_test)
train_mean = mean(a)
train_sd = sd(a)

train_ds = demand_dataset(a, n_timesteps)
test_ds = demand_dataset(b, n_timesteps)

dim(train_ds[1]$x)
    #4 timesteps 
    #24 features 
dim(train_ds[1]$y)
    #This is just the label 

#I need to add a third dimension of batch_size

#Creating a function for the RNN training for easier hyperparameter calculation

rnn_training = function(train, test, epochs = 1000, batch_size = 256, learning_rate = 0.001,
                        input_size = 24, dropout = 0.2, rec_dropout = 0.1,
                        criterion = nn_mse_loss()){
        #Uses the datasets for the input data
        #Setting default parameters 

    #Converting the test and train data
    train_dl = train %>%
        #Using the dataset defined above 
    dataloader(batch_size = batch_size, shuffle = TRUE)
        #Convetring it into 3D
    
    test_dl = test %>%
    dataloader(batch_size = batch_size, shuffle = TRUE)
    
    #Creating the model
    model = nn_module(
        initialize = function(input_size = 16,
                            hidden_size = 15,
                            dropout = 0.2,
                            num_layers = 2,
                            rec_dropout = 0.1) {
        self$num_layers <- num_layers
        
        self$rnn <- nn_gru(
          input_size = input_size,
          hidden_size = hidden_size,
          num_layers = num_layers,
          dropout = rec_dropout,
          batch_first = TRUE
        )
        
        self$dropout <- nn_dropout(dropout)
        self$output <- nn_linear(hidden_size, 1)
        },
        forward = function(x) {
            rnn_out <- self$rnn(x)
            
            # rnn_out[[1]] is the full sequence of outputs
            out <- rnn_out[[1]]
            
            # If we only want the last time step, select it from seq_len dimension:
            # shape becomes [batch_size, hidden_size]
            out <- out[, dim(out)[2], ]
            
            # Apply dropout
            out <- self$dropout(out)
            
            # Pass through the final linear layer
            out <- self$output(out)

            out
        }
        )

        #Initialising model with specified hyperparameters 
    net = model(
      input_size  = 16,
            #this is the amount of features
      hidden_size = 15,
            #The amount of hidden neurons
      num_layers  = 2,
            #Amount of GRU layers I want 
      rec_dropout = rec_dropout
            #Dropout for regularisation
    )
    
    optimiser = optim_adam(net$parameters, lr = learning_rate)

    print("Training has begun.")
    
    epoch_results = data.frame(
    epoch = integer(),
    train_mse = numeric(),
    test_mse = numeric())
        #To store the metrics 
    
    for (i in 1:epochs) {
        train_pred = c()
        train_lab = c()
            #This is to store the values of the prediciton and label 
            #As I am using batches 
    
        test_pred = c()
        test_lab = c()

        #Updating the parameters 
        coro::loop(for (batch in train_dl) {
            optimiser$zero_grad()
            y_pred = net(batch$x)
            loss = criterion(y_pred, batch$y)
            loss$backward()
            optimiser$step()
            train_pred = c(train_pred, as.numeric(y_pred))
            train_lab = c(train_lab, as.numeric(batch$y))
                #This appends the predictions and target
                #So that I can generate MSE results on the whole dataset (not just a single bactCH)
        })

        #Generating test values 
        net$eval()
            #This turns off any parameter tuning during generation of test data
        coro::loop(for (batch in test_dl) {
            y_pred = net(batch$x)
            test_pred = c(test_pred, as.numeric(y_pred))
                #Saving the prediction on the y data
            test_lab = c(test_lab, as.numeric(batch$y))
        })
        net$train()

       #Updating the user
        if ((i %% 1 == 0) | (i == 1)){
            MSE = mean((train_pred - train_lab)**2)
            RMSE_value = sqrt(MSE)
            test_MSE = mean((test_pred - test_lab) **2)
    
            print(paste0("Epoch: ", i))
            print(paste0("Training MSE: ", MSE))
            print(paste0("Training RMSE: ", RMSE_value))
            print(paste0("Test MSE: ", test_MSE))
            
            epoch_results = rbind(epoch_results, data.frame(epoch = i, train_mse = MSE, test_mse = test_MSE))

        }
    }
    return(epoch_results)
}


#TEsting LEarnign Rate 

breaks = 10

rnn_results = data.frame(lr = numeric(), 
                     epoch = integer(), 
                     train_mse = numeric(), 
                     test_mse = numeric())

torch_manual_seed(42)
    #Setting the seed so that the initial weights stay constant

for(learn in seq(0.001, 0.1, length.out = breaks)){
    epoch_results = rnn_training(train_ds, test_ds, epochs = 201, batch_size = 256, learning_rate = learn,
                        input_size = 16, dropout = 0.3, rec_dropout = 0.1,
                        criterion = nn_mse_loss())

    epoch_results$lr = learn

    rnn_results = rbind(rnn_results, epoch_results)
}

start = ggplot(data = filter(rnn_results, lr != 0.001), aes(x = epoch, y = test_mse, color = factor(lr))) +
geom_smooth(se = FALSE, span = 0.3) +theme_bw() +
xlim(0,100)

end =ggplot(data = filter(rnn_results, lr != 0.001), aes(x = epoch, y = test_mse, color = factor(lr))) +
geom_smooth(se = FALSE, span = 0.3) +theme_bw() +
xlim(100, 200)

grid.arrange(start, end)

#Testing Batch Size 

sizes = c(16, 32, 64, 128, 256)

batch_rnn_results = data.frame(batch = numeric(), 
                     epoch = integer(), 
                     train_mse = numeric(), 
                     test_mse = numeric())

torch_manual_seed(42)
    #Setting the seed so that the initial weights stay constant

for(batch_siz in sizes){
    epoch_results = rnn_training(train_ds, test_ds, epochs = 201, batch_size = batch_siz, learning_rate = 0.045,
                        input_size = 16, dropout = 0.3, rec_dropout = 0.1,
                        criterion = nn_mse_loss())

    epoch_results$batch = batch_siz

    batch_rnn_results = rbind(batch_rnn_results, epoch_results)
}

start = ggplot(data = batch_rnn_results, aes(x = epoch, y = test_mse, color = factor(batch))) + 
geom_smooth(se = FALSE, span = 0.3) +
theme_bw() + xlim(0,100)

end = ggplot(data = batch_rnn_results, aes(x = epoch, y = test_mse, color = factor(batch))) + 
geom_smooth(se = FALSE, span = 0.3) +
theme_bw() + xlim(100,200)

grid.arrange(start, end)

#Testing Drop Rate

torch_manual_seed(42)

criterion = nn_mse_loss()

sizes = c(0.1, 0.2, 0.3, 0.4, 0.5)


drop_mlp_results = data.frame(batch = numeric(), 
                     epoch = integer(), 
                     train_mse = numeric(), 
                     test_mse = numeric())

for(drop_rate in sizes){
    mlp_model = nn_sequential(
        nn_linear(19, 15),
        nn_relu(),
        nn_dropout(drop_rate),
        nn_linear(15, 1)
        )

    optimiser = optim_adam(mlp_model$parameters, lr = 0.067)
        #using the optimal LR 
    epoch_results = batch_train(mlp_model, 500, optimiser, criterion, torch_x, torch_y, 32)
    epoch_results$loss = drop_rate 

    drop_mlp_results = rbind(drop_mlp_results, epoch_results)
    
}

start = ggplot(data = drop_mlp_results, aes(x = epoch, y = test_mse, color = factor(loss))) +
geom_smooth(se = FALSE, span = 0.3) +
theme_bw() +
xlim(0, 100)

end = ggplot(data = drop_mlp_results, aes(x = epoch, y = test_mse, color = factor(loss))) +
geom_smooth(se = FALSE, span = 0.3) +
theme_bw() +
xlim(100, 500)

grid.arrange(start, end)

#Testing Loss 

rnn_loss_results = data.frame(loss = numeric(), 
                     epoch = integer(), 
                     train_mse = numeric(), 
                     test_mse = numeric())

torch_manual_seed(42)
    #Setting the seed so that the initial weights stay constant

sizes = c(0.1, 0.2, 0.3, 0.4, 0.5)


for(loss_rate in sizes){
    epoch_results = rnn_training(train_ds, test_ds, epochs = 201, batch_size = 256, learning_rate = 0.067,
                        input_size = 16, dropout = loss_rate, rec_dropout = 0.1,
                        criterion = nn_mse_loss())

    epoch_results$loss = loss_rate

    rnn_loss_results = rbind(rnn_loss_results, epoch_results)
}

start = ggplot(data = rnn_loss_results, aes(x = epoch, y = test_mse, color = factor(loss))) + 
geom_smooth(se = FALSE, span = 0.3) +
theme_bw() + xlim(0,100)

end = ggplot(data = rnn_loss_results, aes(x = epoch, y = test_mse, color = factor(loss))) + 
geom_smooth(se = FALSE, span = 0.3) +
theme_bw() + xlim(100, 200)

grid.arrange(start, end)


#Final RNN

#Creating a function for the RNN training for easier hyperparameter calculation

best_rnn_training = function(train, test, epochs = 1000, batch_size = 256, learning_rate = 0.001,
                        input_size = 16, dropout = 0.2, rec_dropout = 0.1,
                        criterion = nn_mse_loss()){
        #Uses the datasets for the input data
        #Setting default parameters 

    #Converting the test and train data
    train_dl = train %>%
        #Using the dataset defined above 
    dataloader(batch_size = batch_size, shuffle = TRUE)
        #Convetring it into 3D
    
    test_dl = test %>%
    dataloader(batch_size = batch_size, shuffle = TRUE)
    
    #Creating the model
    model = nn_module(
        initialize = function(input_size = 16,
                            hidden_size = 15,
                            dropout = 0.2,
                            num_layers = 2,
                            rec_dropout = 0.1) {
        self$num_layers <- num_layers
        
        self$rnn <- nn_gru(
          input_size = input_size,
          hidden_size = hidden_size,
          num_layers = num_layers,
          dropout = rec_dropout,
          batch_first = TRUE
        )
        
        self$dropout <- nn_dropout(dropout)
        self$output <- nn_linear(hidden_size, 1)
        },
        forward = function(x) {
            rnn_out <- self$rnn(x)
            
            # rnn_out[[1]] is the full sequence of outputs
            out <- rnn_out[[1]]
            
            # If we only want the last time step, select it from seq_len dimension:
            # shape becomes [batch_size, hidden_size]
            out <- out[, dim(out)[2], ]
            
            # Apply dropout
            out <- self$dropout(out)
            
            # Pass through the final linear layer
            out <- self$output(out)

            out
        }
        )

        #Initialising model with specified hyperparameters 
    net = model(
      input_size  = 16,
            #this is the amount of features
      hidden_size = 15,
            #The amount of hidden neurons
      num_layers  = 2,
            #Amount of GRU layers I want 
      rec_dropout = rec_dropout
            #Dropout for regularisation
    )
    
    optimiser = optim_adam(net$parameters, lr = learning_rate)

    print("Training has begun.")
    
    epoch_results = data.frame(
    epoch = integer(),
    train_mse = numeric(),
    test_mse = numeric())
        #To store the metrics 
    
    for (i in 1:epochs) {
        train_pred = c()
        train_lab = c()
            #This is to store the values of the prediciton and label 
            #As I am using batches 
    
        test_pred = c()
        test_lab = c()

        #Updating the parameters 
        coro::loop(for (batch in train_dl) {
            optimiser$zero_grad()
            y_pred = net(batch$x)
            loss = criterion(y_pred, batch$y)
            loss$backward()
            optimiser$step()
            train_pred = c(train_pred, as.numeric(y_pred))
            train_lab = c(train_lab, as.numeric(batch$y))
                #This appends the predictions and target
                #So that I can generate MSE results on the whole dataset (not just a single bactCH)
        })

        #Generating test values 
        net$eval()
            #This turns off any parameter tuning during generation of test data
        coro::loop(for (batch in test_dl) {
            y_pred = net(batch$x)
            test_pred = c(test_pred, as.numeric(y_pred))
                #Saving the prediction on the y data
            test_lab = c(test_lab, as.numeric(batch$y))
        })
        net$train()
            #Tuns back on parameter tuning 

       #Updating the user
        if ((i %% 1 == 0) | (i == 1)){
            MSE = mean((train_pred - train_lab)**2)
            RMSE_value = sqrt(MSE)
            test_MSE = mean((test_pred - test_lab) **2)
    
            print(paste0("Epoch: ", i))
            print(paste0("Training MSE: ", MSE))
            print(paste0("Training RMSE: ", RMSE_value))
            print(paste0("Test MSE: ", test_MSE))
            
            epoch_results = rbind(epoch_results, data.frame(epoch = i, train_mse = MSE, test_mse = test_MSE))

        }
    }
    list(epoch_results = epoch_results, model = net)
        #Returning the model with the learned parameters 
}

torch_manual_seed(42)


result = best_rnn_training(train_ds, test_ds, epochs = 2000, batch_size = 32, learning_rate = 0.045,
                        input_size = 16, dropout = 0.1, rec_dropout = 0.1,
                        criterion = nn_mse_loss())

best_rnn_model = result$model
best_rnn_results = result$epoch_results

start = ggplot(data = best_rnn_results) + 
geom_line(aes(y = test_mse, x = epoch, color = "Test")) + 
geom_line(aes(y = train_mse, x = epoch, color = "Train")) + theme_bw() +
xlim(0,100)

end = ggplot(data = best_rnn_results) + 
geom_line(aes(y = test_mse, x = epoch, color = "Test")) + 
geom_line(aes(y = train_mse, x = epoch, color = "Train")) + theme_bw() +
xlim(200, 2000) +ylim(0,1)

grid.arrange(start, end)

#RNN Results 

#RNN Results

best_rnn_model$eval()

c = as.matrix(rnn_validation)

validation_ds = demand_dataset(c, n_timesteps)

#validation_ds[1]

validation_dl = validation_ds %>%
dataloader(batch_size = 32, shuffle = TRUE)

validation_preds =c()
validation_labels =c()

coro::loop(for (batch in validation_dl) {
  y_pred <- best_rnn_model(batch$x)
  validation_preds = c(validation_preds, as.numeric(y_pred))
  validation_labels = c(validation_labels, as.numeric(batch$y))
})

validation_pred = data.frame(Prediction = validation_preds, Actual = validation_labels)

head(validation_pred)


#Comparing Performances

#Calculatign metrics 

final_results = data.frame(Model = character(), 
                           MSE = numeric(),
                           RMSE = numeric(),
                           MAE = numeric())

final_results = bind_rows(final_results, data.frame(Model = "MLP", 
                      MSE = mean((best_mlp_df[["Prediction"]] - best_mlp_df[["aveOralM"]]) **2 ),
                      RMSE = sqrt(mean((best_mlp_df[["Prediction"]] - best_mlp_df[["aveOralM"]]) **2 )),
                      MAE = mean(abs(best_mlp_df[["Prediction"]] - best_mlp_df[["aveOralM"]]))))

final_results = bind_rows(final_results, data.frame(Model = "RNN", 
                      MSE = mean((validation_pred[["Prediction"]] - validation_pred[["Actual"]]) **2 ),
                      RMSE = sqrt(mean((validation_pred[["Prediction"]] - validation_pred[["Actual"]]) **2 )),
                      MAE = mean(abs(validation_pred[["Prediction"]] - validation_pred[["Actual"]]))))

final_results = bind_rows(final_results, data.frame(Model = "Linear MLP", 
                      MSE = mean((linear_model_df[["Prediction"]] - linear_model_df[["aveOralM"]]) **2 ),
                      RMSE = sqrt(mean((linear_model_df[["Prediction"]] - linear_model_df[["aveOralM"]]) **2 )),
                      MAE = mean(abs(linear_model_df[["Prediction"]] - linear_model_df[["aveOralM"]]))))

final_results = bind_rows(final_results, data.frame(Model = "Elastic Net", 
                      MSE = mean((reg_predictions[["s0"]] - reg_predictions[["aveOralM"]]) **2 ),
                      RMSE = sqrt(mean((reg_predictions[["s0"]] - reg_predictions[["aveOralM"]]) **2 )),
                      MAE = mean(abs(reg_predictions[["s0"]] - reg_predictions[["aveOralM"]]))))

final_results = final_results %>%
  pivot_longer(cols = c("MSE", "RMSE", "MAE"), 
               names_to = "Metric", 
               values_to = "Value")

final_results

ggplot(final_results, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performance Comparison",
       y = "Error Metric Value") +
  theme_bw()


