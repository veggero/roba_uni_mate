# Imposto la working directory, definisco i nomi dei campi e leggo i dati e
# li attaccho.

setwd('/home/niccolove/Documents/Scuola/SD/')

campi = c('age', 'workclass', 'fnlwgt', 'education', 'education-num', 
          'marital-status', 'occupation', 'relationship', 'race', 'sex', 
          'capital-gain', 'capital-loss', 'hours-per-week', 'native-country',
          'income')

data = read.table('adult.txt', sep=',', na.strings='?', skip=1, col.names=campi)
attach(data)

# Dei dati visualizzo dimensione, struttura e i primi dieci

dim(data); str(data)
first-ten = data[1:10,]
View(first-ten)

# Creo un nuovo campo ordinato per l'educazione in base ai campi dati

levels(education)

educazioni = c('Preschool', '1st-4th', '5th-6th', '7th-8th', '9th', '10th', 
               '11th', '12th', 'HS-grad', 'Prof-school', 'Assoc-acdm', 
               'Assoc-voc', 'Some-college', 'Bachelors', 'Masters', 'Doctorate')

education_rec = ordered(education, levels=0:15, labels=educazioni)

# Prendiamo le prime dieci righe delle colonne numeriche

numeric_only = data[,c(1, 3, 5, 11, 12, 13)]
numeric_only[1:10,]

# Disegnamo i grafici per tutte le variabili qualitative

levels(workclass)
# Iteriamo su tutte le colonne e accettiamo solo quelle non numeriche
for (i in 1:ncol(data)){
    if(!is.numeric(data[1,i])) {
        
        # Prendiamo la table e calcoliamo freq, aggiungiamo i valori relativi e
        # assoluti, e disegnamo il grafico
        my_table = table(data[,i])
        str(my_table)
        freq = prop.table(my_table)
        binded = cbind(my_table, round(freq, 4), round(freq*100, 2))
        barplot(freq, cex.names=0.5, cex.axis=1.3, legend.text=colnames(data[i]))
        abline(h=0)
    }
}

# Prendiamo i valori assoluti e relativi della frequenza di ogni combinazione di
# workclass ed education

absolute_table = round(prop.table(table(workclass, education)), 4)
relative_table = absolute_table*100

# Ordiniamo i dati per workclass e education discendente e visualizziamo le
# prime e le ultime 5 righe

data_per_workclass = data[with(data, order(workclass, -education)),]
head(data_per_workclass, 5)
tail(data_per_workclass, 5)

# Creiamo i data frame di maschietti e femminucce, ne mostriamo la dimensione e
# li riuniamo in un singolo dataframe

maschietti = subset(data_per_workclass, sex=="Male")
dim(maschietti)
femminucce = subset(data_per_workclass, sex=="Female")
dim(femminucce)
entrambi = rbind(maschietti, femminucce)
