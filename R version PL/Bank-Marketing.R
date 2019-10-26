######################################## Bank Marketing ################################################

######################## Intall package ########################
install.packages('tidyverse')
install.packages("descr")
install.packages("tables")
install.packages("egg")
# Cross Tables
install.packages("gmodels")
# Additional packages for GGplot
install.packages("ggplot2")
install.packages("ggmosaic")
install.packages('corrplot')
install.packages('ggpubr')
install.packages('cowplot')
install.packages("ggimage")
install.packages("BiocManager")
# Machine Learning
install.packages('caret')
install.packages("ROCR")
install.packages("plotROC") #patch for ggplot
install.packages("pROC")
install.packages("PRROC")
install.packages("xgboost")
install.packages("mlr")
install.packages("randomForest")
install.packages("kernlab")
install.packages("MLmetrics")

######################## Library ########################
library(dplyr)
library(descr, quietly = T)
library(tables, quietly = T)
library(knitr)
#
library(ggplot2)
library(egg)
library(gmodels)
library(ggimage)
library(ggmosaic) 
library(corrplot) 
library(ggpubr) 
library(cowplot)
theme_set(theme_bw())
#
library(caret) 
library(ROCR) 
library(plotROC) 
library(pROC)
library(PRROC) 
library(mlr)
library(randomForest)
library(kernlab)
library(xgboost)
library(tidyverse)


######################## Importing data ########################
setwd('C:/Users/Maciek^M/OneDrive/Desktop/Projekty/Bank-Marketing/R version PL/')
df_bank <- read.csv(file = "Date/Bank_Marketing_data.csv", 
                    sep = ";",
                    stringsAsFactors = F)


#####################################################################################################
##################################### 1. Information about data #####################################
#####################################################################################################
dim <- dim(df_bank)
dim[1] # number of row
dim[2] # number of column
names(df_bank) #names of columns
str(df_bank)
summary(df_bank)
crosstable_y <- CrossTable(df_bank$y) # Percent   (knitr)
crosstable_y


############ Have we Nan or Na?? ############
any(is.na(df_bank)) #False
### 'Na' in this data is 'unknows' ###
number_of_unknown <- sum(df_bank == 'unknown')
table_unknown <- df_bank %>%
    summarise_all(list(~sum(. =='unknown'))) %>%
    gather(key = 'Feature', value = 'Number_of_unknown')  %>%
    arrange(desc(Number_of_unknown))
table_unknown
df_bank1 <- df_bank %>%
    mutate_if(is.character, list(~na_if(., "unknown")))
number_of_empty_row <- dim(df_bank)[1] - dim(df_bank1[complete.cases(df_bank1), ])[1]
per_unknown <- round(number_of_empty_row/dim(df_bank)[1]*100, 2)

#--------------------------- Opis -----------------------------@

# A? 6 zmiennych posiada nieznane warto?ci. Co gorsza, rz?du w kt?rych znajduj? si? brakuj?ce informacje stanowi? prawie
# 25% ca?ego zbioru, wi?c zanim postanowimy co zrobi? z tymi danymi, przestudiujmy je.

#------------------------ END ---------------------------------@



###################################################################################################
##################################### 2. Exploraroty Analysis #####################################
###################################################################################################

###### Function 
fun_barplot_categorical <- function(x_column, xlabel, fill_column='y'){
    # x_column: (str) name of column with dataframe
    # fill_column: (str) name of column with dataframe which we fill
    # xlabel: (str) name of x label
    gg1 <- ggplot(data = df_bank, aes_string(x = x_column , fill = fill_column)) +
        geom_bar(position = "dodge2") +
        theme(axis.text.x = element_text(angle = -35,  hjust = 0)) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        labs(x = xlabel, y = "Count")
    
    gg2 <- ggplot(data = df_bank, aes_string(x = x_column, fill = fill_column)) +
        geom_bar(position = "fill") +
        theme(axis.text.x = element_text(angle = -35,  hjust = 0)) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        labs(x = xlabel, y="Percent of population")
    
    
    plot_grid(gg1, gg2,
              nrow =2,
              labels = c("a)", "b)"))
}


############@
fun_histogram <- function(main_column, xlabel, xmin=0, xmax=20, bins=50){
    # main_column: (str) name of column with dataframe
    # xlabel: (str) name of x label
    # limit_breaks: (int) x limit
    gg <- ggplot(df_bank, aes_string(x=main_column, color="y", fill="y")) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 70)+
        geom_density(alpha=0.2)+
        facet_grid(y ~ ., scales = 'free_y') +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        scale_x_continuous(breaks = seq(xmin, xmax, bins)) +
        labs(x = xlabel, 
             y = "Density")+
        theme_classic()
    return(gg)
}


############@
fun_boxplot <- function(column, ylabel){
    # column: (str) name of column with dataframe
    # ylabel: (str) name of y label
    ggplot(data = df_bank, aes_string(x = "y", y = column)) +
        stat_boxplot(geom = "errorbar", width = 0.6) +
        geom_boxplot(aes(fill = y)) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        labs(x = "Long-term deposit taken",
             y = ylabel)
}


############@
fun_crosstable <- function(column1, column2="y"){
    # column1: (str) name of first column with dataframe to CrossTable
    # column2: (str) name of second column with dataframe to CrossTable
    return(CrossTable(df_bank[, column1], df_bank[, column2], 
                      prop.r = T, 
                      prop.c = F, 
                      prop.t = F, 
                      prop.chisq = F,
                      dnn = c(column1, column2)))
}

#############################################################
fun_important_feature_ggplot = function(model){
    # Function do present most importent variable in us model
    # model: model used to plot variable importances
    if (class(model)[1] == "ranger"){
        imp_df = model$variable.importance %>% 
            data.frame("Overall" = .) %>% 
            rownames_to_column() %>% 
            rename(variable = rowname) %>% 
            arrange(-Overall)
    } else {
        imp_df = varImp(model) %>%
            rownames_to_column() %>% 
            rename(variable = rowname) %>% 
            arrange(-Overall)
    }
    
    # first panel (half most important variables)
    gg1 = ggplot(data = imp_df, aes(x = reorder(variable, Overall), weight = Overall, fill = -Overall)) +
        geom_bar() +
        coord_flip() +
        labs(x = "Variables",
             y = "Importance") +
        theme(legend.position = "none")
    
    imp_range = ggplot_build(gg1)[["layout"]][["panel_params"]][[1]][["x.range"]]
    imp_gradient = scale_fill_gradient(limits = c(-imp_range[2], -imp_range[1]),
                                       low = "#132B43", 
                                       high = "#56B1F7")
    
    
    return(gg1 + imp_gradient)
}

#################
fun_ROC_PR_curve_ggplot = function(df) {
    # df: dataset generate by generateThreshVsPerfData
    # info in data: 
    #learner - type of model
    #ppv - precision
    #tpr - recall
    #tnr - specificity
    #threshold 
    
    data <- df$data
    
    roc_curve_test <- data %>%
        mutate(tnr = 1-tnr) %>%
        ggplot()+
        aes(x = tnr, y = tpr, colour = learner)+
        geom_line(size=1) +
        xlab("Spenificity") +
        ylab("Sensitivity")+
        ggtitle("ROC Curve", subtitle = "Validation dataset")
    
    pr_curve_test <- ggplot(data, aes(x = tpr, y = ppv, colour = learner))+
        geom_line(size=1) +
        xlab("Recall") +
        ylab("Precision")+
        ggtitle("PR Curve", subtitle = "Validation dataset")
    
    curves_test = ggarrange(roc_curve_test, pr_curve_test, 
                            common.legend = T,
                            legend = "bottom")
    
    return(curves_test)
}


###############################################################################################
####################################### Bank client data:
### 1. Age (numeric)
name_of_column <- 'age'
name_of_label <- 'Age'
fun_histogram(name_of_column, name_of_label, 15, 88, 5)
fun_boxplot(name_of_column, name_of_label)
summary(df_bank$age)  #table

#------------- Opis Age --------------------#
# Przedzia? wiekowy os?b bior?cych kredyt szacuje si? miedzy 18 rokiem ?ycia, a 88 rokiem ?ycia. Jednak?e mo?na zauwa?y?,
# ?e osoby kt?re uko?czy?y 60 rok ?ycia z wi?ksz? ch?ci? bra?y lokaty, ni? tego nie robi?y. ?redni wiek utrzymuje si? na poziomie 40 lat.
# Wiedz?c, ?e osoby odk?adaj? na lokaty fundusze wtedy, kiedy dobrze zaczynaj? zarabnia? to podzieli?bym ludzi ze wzgl?du na wiek.
#  Mi?dzy wiekiem [MIN, 30] <- young, [30,65] <- worker, [65, MAX] <- pensioner. Taki podzia? powinien u?atwi? analiz? przysz?ych algorytm?w.
# 
#--------------- END -----------------------#
df_bank <- df_bank %>%
    mutate(age_categorical = if_else(age < 65 , if_else(age >= 30, 'worker', 'young'), "pensioner")) %>%
    select(-age)



### 2. Jobs (categorical)
name_of_column <- 'job'
name_of_label <- 'Job'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable(name_of_column)
#-----------------------Opis Job ----------------#
'# W tej kolumnie mamy 39 warto?ci nieznanych, co stanowi ledwo 1% ca?ego zbioru, wi?c pozbywamy si? wierszy, kt?re zawieraj? t? informacj?.
'dod#-------------------- END -------------------------@
df_bank <- df_bank %>%
    filter(job != "unknown")


### 3. Marital status (categorical)
name_of_column <- 'marital'
name_of_label <- 'Marital status'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable(name_of_column)
#-----------------------Opis --------------------#
# Sytuacja taka sama jak przy kolumnie 'job'. Mamy tutaj nieznane warto?ci, ale stanowi? one tylko 0.3% wszystkich danych, wi?c r?wnie? usuwamy te wiersze.

#-------------------- END -------------------------@
df_bank <- df_bank %>%
    filter(marital != "unknown")




### 4. Education (categorical)
name_of_column <- 'education'
name_of_label <- 'Education level'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable(name_of_column)
#-----------------------Opis --------------------#
# Pula klient?w b?d?cymi 'illiterate' zawieta tylko jedn? osob?, wi?c statystycznie taka obserwacja nic nam nie daje.
# Natomiast w tym przypadku mamy problem z nieznanymi warto?ciami. Po pierwsze stanowi? one 4.1% wszystkich badanych.
# Najbardziej podobne proporcje danych mi?dzy 'yes' i 'no' ma categoria klient?w, kt?rzu uko?czyli uniwersytet, wi?c wszystkich klient?w 'unknown'
# dodam do tej puli klient?w.
#-------------------- END -------------------------@
df_bank <- df_bank %>% 
    filter(education != "illiterate") %>%
    mutate(education = recode(education, "unKnown" = "university.degree"))



### 5. Has credit in default? (categorical)
name_of_column <- 'default'
name_of_label <- 'Has credit in default?'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable(name_of_column)
#-----------------------Opis --------------------#
# Ta kolumna nie przedstawia wystarczaj?cej ilo?ci danych o os?bach, kt?re domy?lnie wzie?y ten kredyt. 
# Z tego powodu ta kolumna nie b?dzie mia?a ?adnego wi?kszego wp?ywu na nasze modele, dlatego j? usuwamy.
#-------------------- END -------------------------@
df_bank <- df_bank %>% 
    select(-default)



### 6. Has housing loan? (categorical)
name_of_column <- 'housing'
name_of_label <- 'Has housing loan?'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable(name_of_column)

#-----------------------Opis --------------------#
# W tej kolumnie znajduj? si? informacje na temat posiadania kredytu hipotecznego (kredytu na dom). Ilo?? danych nieznanych odpowiada, 2,5% wszystkich obserwacji,
# Nie mo?emy pozwoli? sobie na usuni?cie tak du?ej liczny wierszy, a pod?aczenie do jakie? innej opcji nie wchodzi w gr?. 
# Przeprowad?my testy na niezale?no?? zmiennych kategorycznych. Wykonamy test chisq w zelu zbadania niezale?no?ci mi?dzy 2 zmiennymi.
#-------------------- END -------------------------@

chisq.test(df_bank$housing, df_bank$y)

#-----------------------Opis --------------------#
# Niestety poziom istotno?ci(p-value) na poziomie 73% ?wiadczy o du?ej zale?no?ci mi?dzy danymi, wi?c tej zmiennej r??nie? nie b?d? bra? pod uwag?.
#-------------------- END -------------------------@
df_bank <- df_bank %>% 
    select(-housing)


### 7. Has personal loan? (categorical)
name_of_column <- 'loan'
name_of_label <- 'Education level'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable(name_of_column)
chisq.test(df_bank$loan, df_bank$y)
#-----------------------Opis --------------------#
# W tej kolumnie znajduj? si? informacje na temat posiadania kredytu. Sytuacja jest ta sama co w przypadku kredytu hipotecznego.
# Nie mo?emy pozwoli? sobie na usuni?cie tak du?ej liczny wierszy (2.5%), a pod?aczenie do jakie? innej opcji nie wchodzi w gr?. 
# Wykonamy test chisq w zelu zbadania niezale?no?ci mi?dzy 2 zmiennymi.
# Poziom istotno?ci(p-value) na poziomie 56.8% ?wiadczy o du?ej zale?no?ci mi?dzy danymi, wi?c tej zmiennej r??nie? nie b?d? bra? pod uwag?.
#-------------------- END -------------------------@
df_bank <- df_bank %>% 
    select(-loan)





################## Related with the last contact of the current campaign: ##################

### 8. Contact communication type (categorical)
name_of_column <- 'contact'
name_of_label <- 'Contact'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable(name_of_column)
#-----------------------Opis --------------------#
# Osoby, z kt?rymi pr?bowano si? skontaktowa? na telefon kom?rkowy stanowi? 64.4% ca?ej badanej spo?eczno?ci i co 6 osoba z nich wzie?a lokat? d?ugoterminow?.
#-------------------- END -------------------------@




### 9. Last contact month of year (categorical)
name_of_column <- 'month'
name_of_label <- 'Month'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable(name_of_column)
#-----------------------Opis --------------------#
# Ciekaw? sutuacj? jest fact, ?e w zestawieniu w og?le nie mamy tranzakcji ze stycznia oraz z lutego. 
#-------------------- END -------------------------@




### 10. Last contact day of the week (categorical)
name_of_column <- 'day_of_week'
name_of_label <- 'Day of week'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable(name_of_column)
#-----------------------Opis --------------------#
# W ka?dym roboczym dniu tygodnia jest wykonywane mniej wi?cej tyle samo po??cze? z klientami, 
# wi?c nie jeste?my w stanie wyci?gn?? ?adnych wi?kszych wniosk?w, z obserwacji samych wykres?w.

#-------------------- END -------------------------@





### 11. Last contact duration, in seconds (numeric) - 
#-----------------------Opis --------------------#
# Czas trwania ostatniego kontaktu jest atrybutem, kt?ry ma du?y wp?yw na cel wyj?ciowy (y)
# Wa?na uwaga: ten atrybut ma du?y wp?yw na cel wyj?ciowy. Jednak czas trwania nie jest znany przed wykonaniem po??czenia. 
# Ponadto po zako?czeniu po??czenia "y" jest oczywi?cie znane. W zwi?zku z tym  nale?y j? odrzuci?, je?li chcemy stworzenie realistycznego modelu predykcyjnego.
#-------------------- END -------------------------@
df_bank = df_bank %>% 
    select(-duration)


#################### Other parameter: #################################

### 12. Number of contacts performed during this campaign and for this client (numeric)
name_of_column <- 'campaign'
name_of_label <- 'Campaign'
fun_histogram(name_of_column, name_of_label,0, 20, 1)
fun_boxplot(name_of_column, name_of_label)
summary(df_bank$campaign)  #table
count(df_bank %>%
      select(campaign) %>%
      filter(campaign>=7))
#-----------------------Opis --------------------#
# W tej kolumnie zawieraj? si? informacje dotycz?ce liczby po??cze? do danego klienta w ramach kampani. 
# Przygl?daj?c si? boxplotowi mo?emy zauwa?y?, ?e obserwacje od 7 s? ju? uwa?ane jako warto?ci odstaj?ce. 
# W zwi?zku z czym wszystkie watro?ci, kt?re s? wi?ksze ni? 7, ale mniejsze ni? 12 zamieni? na 7+, a nast?pnie zamienie na zmienn? kategoryczn?.
# Zmienne powyzej 12 odrzucam.
#-------------------- END -------------------------@
df_bank <- df_bank %>%
    filter(campaign <= 12) %>%
            mutate(campaign = if_else(campaign == 1, "1",
                                      if_else(campaign == 2, "2",
                                              if_else(campaign == 3, "3",
                                                      if_else(campaign == 4, "4",
                                                              if_else(campaign == 5, "5",
                                                                      if_else(campaign == 6, "6", "7+")))))))
name_of_column <- 'campaign'
name_of_label <- 'Campaign'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable('campaign')



### 13. Pdays (numeric)
name_of_column <- 'pdays'
name_of_label <- 'Pdays'
# fun_histogram(name_of_column, name_of_label,0, 10) #nothing
# fun_boxplot(name_of_column, name_of_label) # nothing
summary(df_bank$pdays)  #table
table(df_bank$pdays)
#-----------------------Opis --------------------#
# Kolumna 'pdays' znajduj? si? informacje o ilo?ci dni od ostatniego kontaktu z klientem podczas poprzedniej kampanii. Je?li warto?c wynosi "999" to oznacza, ze nigdy si? z dan? osob? nie skontaktowane.
# Informacja zawarta w tej kolumnie jest zbyt rozproszona i niejednoznaczna. W zwi?zku z tym t? informacj? zamienimy w taki spos?b, zeby odpowiada?a na pytanie: czy kontaktowano si? z dan? osob? w poprzedniej kampani?
#-------------------- END -------------------------@
df_bank <- df_bank %>%
    mutate(pdays_call = if_else(pdays == 999,"no","yes")) %>%
    select(-pdays)
fun_crosstable('pdays_call')


### 14. Previous (numeric)
table(df_bank$previous)
df_bank <-  df_bank %>% 
    mutate(previous = if_else(previous >=  2, "2+", if_else(previous == 1, "1", "0")))
#-----------------------Opis --------------------#
#Zmienna 'previous' zawiera informacje dotycz?ce liczby kontakt?w wykonanych przed t? kampani? do tego klienta. W obecnym stanie, ta zmienna zwraca bardzo podobne wyniki co zmienna "pdays_binary". 
#Poniewa? liczba klient?w, ? kt?rymi kontaktowano si? 2 i wi?cej razy jest wyj?tkowa ma?a w stosunku do os?b, z kt?rymi si? nie kontaktowano to propnuj? po??czy? te zmienne w wi?ksz? ca?o??.
# Tak przygotowana zmienna powie nam, czy n?kanie os?b poprzez cz?strze kontakty telefoniczne daje wymierne skutki.
#-------------------- END -------------------------@

### 15. Poutcome (categorical)
name_of_column <- 'poutcome'
name_of_label <- 'Poutcome'
fun_barplot_categorical(name_of_column, name_of_label)
fun_crosstable(name_of_column)
#-----------------------Opis --------------------#
# Czy sukces lub pora?ka podczas poprzedniej kampani da?y si? odczy? w obecnej kampani??
# Jak najbardziej. A? 64.8% os?b chcia?bo przed?u?y? swoj? lokat?. 14.8% os?b , mimo ?e ostatnio nie wzieli lokaty to czas to czas mi?dzy kampaniami da? im sporo do my?lenia i spowodowa?,
# ?e osoby, z kt?rymi si? kontaktowano w poprzedniej kampani ch?tniej bra?y lokat? d?ugoterminow? ni? osoby, z k?trymi kontaktowano si? pierwszy raz.
#-------------------- END -------------------------@


#-------------------------------------------------------------------------------------------------------------@
#                                      Social and economic context attributes
#-------------------------------------------------------------------------------------------------------------@
#----------------- Opis ----------------- @
# Ostatnie 5 wsp??czynnik?w to wska?niki spo?eczne oraz ekonomiczne. Analiza ka?dej z osobna jest nie pokaz?a ?adnej charakterystycznej cech. 
# Jednak?e te zmienne powinny by? jako? skorelowane ze sob?, wi?c obliczmy macierz korelacji.

############################################# Corrplot #######################################################

#-------------------------------------------
numeric_column <- names(which(sapply(df_bank, is.numeric)))
df_bank %>%
    select(numeric_column) %>%
    cor() %>%
    corrplot(method = "number",
             type = "upper",
             tl.cex = 0.6,
             tl.offset = 0.3,
             tl.srt = 45)


#------------------- Opis ---------------------------#
# Zgodnie z oczekiwaniami, zmienne spo?eczne i ekonomiczne s? silnie ze sob? skorelowane. Szczeg?lnym przypadkiem silnego skorelowania charakteryzuje si? zmienna 
# eps.price.rate, kt?ra jest wska?nikiem zmienno?ci zatrudnienia. W zwi?zku z czym ta zmienna zostaje usuni?ta.
#-----------------------------------------------------#
df_bank <- df_bank %>%
            select(-emp.var.rate)



### 21. y - czy klient subskrybowa? lokat?? (dw?jkowy: ?tak?, ?nie?)
#----- Opis-------#
# POniewa? nie mozeby zostawi? tej zmiennej w postaci zmiennej characterystycznej, musimy zamieni? go na zmienn? liczbow?.
#-----------------
df_bank <- df_bank %>%
    mutate(y = as.factor(if_else(y == "yes", 1,0)))



######################################### Normalization #######################################################

norm_df_bank <- normalizeFeatures(df_bank, target = 'y')


############################################ Convert Categorical -> Dummy variable ######################################
#-------------------------- Opis ---------------------------#
# ?eby model regresji logistyczne oraz XGBoost mog?y prawid?owo dzia?a? nale?y zmieni? zmienne kategoryczne na zmiennej fikcyjne (dummy variable).
#-----------------------------------------------------------#
norm_df_bank <- mutate_if(norm_df_bank, is.character, as.factor)
factor_column <- names(which(sapply(norm_df_bank, is.factor)))
factor_column <- factor_column[-(length(factor_column)-2)]

dummy_norm_df_bank <- createDummyFeatures(norm_df_bank, 
                                    target = "y",
                                    cols = c(factor_column))
                                        
                        


head(dummy_norm_df_bank)
summary(dummy_norm_df_bank)



#-----------------------------------------------------------------------------------------------------------@
#                                              Predictive models                                            #
#-----------------------------------------------------------------------------------------------------------@


#----------------------- 1. Split and Create Datasets -------------------------2

#----- Opis ------- @
# Nasze dane s? ju? prawie gotowe do dzielenia na zbi?r treningowy oraz zbi?r walidacyjny. W pierwszej kolejno?ci chcia?bym znormalizowa? nasze dane.
# Tak zabieg mo?e poprawi? jako? niekt?rych modeli np. logistic regresion. Nast?pnie dane zostan? podzielone w stosunku 80/20.
# Dane treningowe pos?u?? do wytrenowania naszego modelu, natomiast dane testowe zostan? wykorzystane podczas walidacji modelu.
#------------------ @


glimpse(dummy_norm_df_bank)

set.seed(2019)
index_train_list = createDataPartition(dummy_norm_df_bank$y,
                          times = 1,
                          p = 0.8,
                          list = F)

train_bank = dummy_norm_df_bank[index_train_list, ]
test_bank = dummy_norm_df_bank[-index_train_list, ]

CrossTable(train_bank$y)
CrossTable(test_bank$y)


#-------------------------- 2. Measure selection -------------------------#

# Na tym etapie przyst?pujemy do generalnego opisu danych i co hcemy wyci?gn?? z danych:
# \begin{enumerate}
# \item Nasze dane s? mocno niezr?wnowa?one (stosunek 89/11). To powoduje, ze ?atwo b?dzie nam osi?gn?? wysobie accuracy, kt?re w rzeczywisto?ci mo?e nie oddawa? dok?adnie tego co chcemy;
# \item Nasz model ma przewidywa?, kt?ry klient rzeczywi?cie za?o?y lokat? d?ugoterminow?. Patrz?c na "Confusion matrix", ta miara jest opisana przez True Positive (TP).
# \item Pod miar? "False Positive" (FP) b?d? kryli si? klienci, kt?rzy jeszcze nie b?d? gotowi na za?o?enie lokaty d?ugoterminowej
# \item Pod miar? "False Negative" (FN) b?d? kryli si? klienci, kt?rzy s? ch?tni za?ozy? lokat?, jednak?e ban si? z nimi nie skontaktowa?. Ta miara w naszym przypadku b?dzie przynosi? najwi?ksze straty dla banku.
# \item A pod ostatni? miar? "True negative" (TN) b?d? wszyscy klienci banku, kt?rzy rzeczywi?cie nie chc? za?o?y? lokaty.
# \item Czyli, ?eby maksymalizowa? zyski musimy skupi? si? na maksymalizacji miar "Recall", "Precision" oraz F_1 score z g??wnym skupieniem si? na f_1 score, poniewa? jest to ?rednia harmoniczna z "Recall" oraz "Precision".
# \item W dalszym etapie por?wnywania modeli skupimy si? na por?wnaniach  krzywych ROC i krzywych PR.
# \end{enumerate}



##########################################################################################################
###################################  MODELS  #############################################################
##################################################################################

# ---------------------------- Opis ----------------------------- @
# W ramach tej cz?sci b?d? wykorzystywa? bibliotek? \verb+mlr+ do tworzenia modeli:
# \begin{itemize}
# \item Logistic regression
# \item Random forest
# \item SVM
# \item XGBoost
# \end{itemize}
#------------------------------------------------------------------#

###################  Przygotowanie danych dla pakietu "mrl"

trainTask <- makeClassifTask(data = train_bank, target = "y")
testTask <- makeClassifTask(data = test_bank, target = "y")


#-------------------------------------------------------------------------- @
#----------------------- 2.1 Logistic Regresion --------------------------- @
#-------------------------------------------------------------------------- @

logistic_learner <- makeLearner("classif.logreg",
                                predict.type = "prob")
getParamSet("classif.logreg")



cv.logistic <- crossval(learner = logistic_learner,
                          task = trainTask,
                          iters = 5,
                          stratify = TRUE,
                          measures = f1,
                          show.info = F)

logistic_model <- train(logistic_learner, trainTask)
summary(getLearnerModel(logistic_model))
log_pred <- predict(logistic_model, testTask)
calculateROCMeasures(log_pred)

log_pred

fun_important_feature_ggplot(getLearnerModel(logistic_model))

#-------------------------------------------------------------------------- @
#---------------------------- 2.2 Random Forest --------------------------- @
#-------------------------------------------------------------------------- @

rf_learner <- makeLearner("classif.randomForest", 
                          predict.type = "prob", 
                          par.vals = list(ntree = 500, mtry = 3))

getParamSet("classif.randomForest")

rf_params <- makeParamSet(
    makeIntegerParam("ntree", lower = 50, upper = 500),
    makeIntegerParam("mtry", lower = 3, upper = 20),
    makeIntegerParam("nodesize", lower = 10, upper = 50)
)

rf_control <- makeTuneControlRandom(maxit = 20)

resample_desc <- makeResampleDesc("CV", iters = 5)

rf_tune_params <- tuneParams(learner = rf_learner, 
                             resampling = resample_desc, 
                             task = trainTask, 
                             par.set = rf_params, 
                             control = rf_control,
                             measures = f1)

rf_tuned_learner <- setHyperPars(learner = rf_learner, 
                                 par.vals = rf_tune_params$x)
#########
rf_model <- train(rf_tuned_learner, trainTask)
# summary(getLearnerModel(rf_model))
fun_important_feature_ggplot(getLearnerModel(rf_model))
rf_pred <- predict(rf_model, testTask)
calculateROCMeasures(rf_pred)


#-------------------------------------------------------------------------- @
#------------------------------ 2.3 SVM ----------------------------------- @
#-------------------------------------------------------------------------- @

ksvm_learner <- makeLearner("classif.ksvm", 
                    predict.type = "prob")

getParamSet("classif.ksvm")

ksvm_params <- makeParamSet(
    makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), 
    makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) 
)
ctrl <- makeTuneControlGrid()

resample_desc <- makeResampleDesc("CV", iters = 5)

ksvm_tune_params <- tuneParams(learner = ksvm_learner, 
                  task = trainTask, 
                  resampling = resample_desc, 
                  par.set = ksvm_params, 
                  control = ctrl,
                  measures = f1)

ksvm_tuner_learner <- setHyperPars(learner = ksvm_learner, 
                                   par.vals = ksvm_tune_params$x)

#######
ksvm_model <- train(ksvm_tuner_learner, trainTask)
# summary(getLearnerModel(ksvm_model))
fun_important_feature_ggplot(getLearnerModel(ksvm_model))
ksvm_pred <- predict(ksvm_model, testTask)
calculateROCMeasures(ksvm_pred)

ksvm_model

#-------------------------------------------------------------------------- @
#------------------------------ 2.4 XGBoost ------------------------------- @
#-------------------------------------------------------------------------- @

xgb_learner <- makeLearner(
    "classif.xgboost",
    predict.type = "prob",
    par.vals = list(
        objective = "binary:logistic",
        eval_metric = "error",
        nrounds = 200
    )
)

getParamSet("classif.xgboost")

xgb_params <- makeParamSet(
    makeIntegerParam("nrounds", lower = 100, upper = 1000),
    makeIntegerParam("max_depth", lower = 1, upper = 10),
    makeNumericParam("eta", lower = 0.025, upper = 0.5),
    makeNumericParam("gamma", lower = 0, upper = 1),
    makeNumericParam("min_child_weight", lower = 1, upper = 3),
    makeNumericParam("colsample_bytree", lower = 0.2, upper = 1), 
    makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
)

xgb_control <- makeTuneControlRandom(maxit = 10)

resample_desc <- makeResampleDesc("CV", iters = 5)

xgb_tuned_params <- tuneParams(
    learner = xgb_learner,
    task = trainTask,
    resampling = resample_desc,
    par.set = xgb_params,
    control = xgb_control,
    measures = f1)

xgb_tuned_learner <- setHyperPars(
    learner = xgb_learner,
    par.vals = xgb_tuned_params$x)


#######
xgb_model <- train(xgb_tuned_learner, trainTask)
summary(getLearnerModel(xgb_model))

xgb_pred <- predict(xgb_model, testTask)
calculateROCMeasures(xgb_pred)

####################################################### Benchmark experiment #######################################

list_of_models <- list(logistic_learner, 
                     rf_tuned_learner,
                     ksvm_tuner_learner, 
                     xgb_tuned_learner)
rdesc = makeResampleDesc("CV", iters = 10)
list_of_measures = list(acc, ppv, tpr, tnr, auc, f1)

set.seed(2019)
table_of_measure = benchmark(list_of_models, testTask, rdesc, measures = list_of_measures)
table_of_measure = data.frame(table_of_measure)
table_of_measure  <- table_of_measure %>%
    group_by(learner.id) %>%
    summarise(mean(acc), mean(ppv), mean(tpr), mean(tnr), mean(auc), mean(f1))

AUCPR = c( pr.curve(testTask$env$data$y, log_pred$data$response)[[2]],
          pr.curve(testTask$env$data$y, rf_pred$data$response)[[2]],
          pr.curve(testTask$env$data$y, ksvm_pred$data$response)[[2]],
          pr.curve(testTask$env$data$y, xgb_pred$data$response)[[2]])

table_of_measure <- table_of_measure %>%
    mutate(aucpr = AUCPR)
kable(t(table_of_measure), 'latex')
####################################################### ROC Curve & PR Curve ########################################
list_of_pred <- list(logistic = log_pred, 
                     random_forest = rf_pred,
                     SVM = ksvm_pred, 
                     XGBoost = xgb_pred)
list_of_measures = list(acc, ppv, tpr, tnr)
set.seed(2019)
score_test = generateThreshVsPerfData(obj = list_of_pred, 
                                      measures = list_of_measures)
score_test$data
fun_ROC_PR_curve_ggplot(score_test)







