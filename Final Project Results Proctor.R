
Final Project Results Section
Evie Proctor


library(tidyverse)

drug.data <- read.csv("Drug Treatment Matrix Dataset.csv", row.names = "Treatment")


drug_data_clean <- drug.data %>%
  drop_na() #not great because this drops both of the entire control rows

library(dplyr)

adavosertib_select <- drug.data %>%
  select(Adavosertib.0, Adavosertib.0.1, Adavosertib.0.5, Adavosertib.1.5, Adavosertib.4.6, 
           Adavosertib.13.7, Adavosertib.41.2, Adavosertib.123.5, Adavosertib.370.4, 
           Adavosertib.1111.1, Adavosertib.3333.3, Adavosertib.10000)

prexasertib_select <- drug.data %>%
  select(Prexasertib.0, Prexasertib.0.1, Prexasertib.0.5, Prexasertib.1.5, Prexasertib.4.6,
         Prexasertib.13.7, Prexasertib.41.2, Prexasertib..123.5, Prexasertib..370.4,
         Prexasertib.1111.1, Prexasertib..3333.3, Prexasertib..10000)

gemcitabine_select <- drug.data %>%
  select(Gemicitabine.0, Gemicitabine.0.1, Gemicitabine.0.5, Gemicitabine.1.5, Gemicitabine.4.6, 
         Gemicitabine.13.7, Gemicitabine.41.2, Gemicitabine.123.5, Gemicitabine.370.4, 
         Gemicitabine1111.1, Gemicitabine..3333.3, Gemicitabine..10000)

#now I am going to make seperate plots for each figure

#Starting with Adavosertib (WEE1 inhibitor)

adavosertib_select <- as.matrix(adavosertib_select)

heatmap(adavosertib_select) #need to figure out how to remake the order of this so that it goes chronologically
#Realized at this point that is it actually waaaayyyy too complicated to work with this set in a matrix format so I have restructured it into a datatable!








#THESE ARE THE ACTUAL FIGURES

drug_table <- read.csv("Drug Treatment Table Dataset.csv")
# X is % but that didn't translate well from the excel file

Figure1

ggplot(data = drug_table, mapping = aes(x = ConcA, y = ConcB)) +
  geom_point(aes(color = DrugB)) 
#here is a ~very~ preliminary plot showing overall difference in concentrations
#since all the drugs follow the same pattern of treatment they are all the same 
#(was basically testing to see if this table setup would work better than the matrix which it does)
#don't use this one it kinda sucks and the next one shows better stuff


Figure2

ggplot(data = drug_table, mapping = aes(x = ConcA, y = X.Viability, color = DrugB)) +
  geom_point() + 
  scale_x_log10() +
  xlab('Ceralasertib Concentration (nM)') +
  ylab('Percent Viability') +
  ggtitle('Percent Viability Dependant on Ceralasertib Concentration')
#This is showing the percent viability strictly dependant on Ceralasertib concentration
#Here we can see that as ceralasertib concentration increases viability decreases 


Figure3

ggplot(drug_table, aes(x = interaction(DrugA, DrugB), y = X.Viability, fill = DrugB)) +
  geom_violin() +
  geom_point() +
  ggtitle('Percent Viability Distribution by Drug Combination') +
  xlab('Drug Combination') +
  ylab('Percent Viability') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(drug_table, aes(x = interaction(DrugA, DrugB), y = X.Viability, fill = DrugB)) +
  geom_boxplot() +
  ggtitle('Percent Viability Distribution by Drug Combination') +
  xlab('Drug Combination') +
  ylab('Percent Viability') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#not sure if I like the violin plot or the boxplot better
#lets use the boxplot here


Figure4#multiple part figure but also this should probably be figure 1...

adavosertib_table <- drug_table %>%
  filter(DrugB == 'Adavosertib')

ggplot(data = adavosertib_table, mapping = aes(x = ConcB, y = X.Viability)) +
  geom_smooth(color = 'purple') +
  scale_x_log10() +
  xlab('Adavosertib (nM)') +
  ylab('Percent Viability') +
  ggtitle('Dose Response Curve Adavosertib')


prexasertib_table <- drug_table %>%
  filter(DrugB == 'Prexasertib')

ggplot(data = prexasertib_table, mapping = aes(x = ConcB, y = X.Viability)) +
  geom_smooth(color = 'purple') +
  scale_x_log10() +
  xlab('Prexasertib (nM)') +
  ylab('Percent Viability') +
  ggtitle('Dose Response Curve Prexasertib')


gemcitabine_table <- drug_table %>%
  filter(DrugB == 'Gemcitabine')

ggplot(data = gemcitabine_table, mapping = aes(x = ConcB, y = X.Viability)) +
  geom_smooth(color = 'purple') +
  scale_x_log10() +
  xlab('Gemcitabine (nM)') +
  ylab('Percent Viability') +
  ggtitle('Dose Response Curve Gemcitabine')


ceralasertib_table <- drug_table %>%
  filter(DrugA == 'Ceralasertib')

ggplot(data = ceralasertib_table, mapping = aes(x = ConcB, y = X.Viability)) +
  geom_smooth(color = 'purple') +
  scale_x_log10() +
  xlab('Ceralasertib (nM)') +
  ylab('Percent Viability') +
  ggtitle('Dose Response Curve Ceralasertib')

#use all of these, do like A,B,C,D figures


Figure5

adavosertib_table$X.Viability <- as.numeric(adavosertib_table$X.Viability)
adavosertib_table$ConcA <- as.numeric(as.character(adavosertib_table$ConcA))
adavosertib_table$ConcB <- as.numeric(as.character(adavosertib_table$ConcB))
range(adavosertib_table$X.Viability, na.rm = TRUE)

ggplot(adavosertib_table, aes(x = ConcA, y = ConcB, fill = X.Viability)) +
  geom_tile() +
  scale_x_log10() +
  scale_y_log10() +
  scale_fill_gradient(low = "blue", high = "red", limits = c(5.57, 130.68)) +
  xlab('Adavosertib (nM)') +
  ylab('Ceralasertib (nM)') +
  ggtitle('Heatmap of Adavosertib with Ceralasertib')



prexasertib_table$X.Viability <- as.numeric(prexasertib_table$X.Viability)
prexasertib_table$ConcA <- as.numeric(as.character(prexasertib_table$ConcA))
prexasertib_table$ConcB <- as.numeric(as.character(prexasertib_table$ConcB))
range(prexasertib_table$X.Viability, na.rm = TRUE)

ggplot(prexasertib_table, aes(x = ConcA, y = ConcB, fill = X.Viability)) +
  geom_tile() +
  scale_x_log10() +
  scale_y_log10() +
  scale_fill_gradient(low = "blue", high = "red", limits = c(3.28, 220.79)) +
  xlab('Prexasertib (nM)') +
  ylab('Ceralasertib (nM)') +
  ggtitle('Heatmap of Prexasertib with Ceralasertib')



gemcitabine_table$X.Viability <- as.numeric(gemcitabine_table$X.Viability)
gemcitabine_table$ConcA <- as.numeric(as.character(gemcitabine_table$ConcA))
gemcitabine_table$ConcB <- as.numeric(as.character(gemcitabine_table$ConcB))
range(gemcitabine_table$X.Viability, na.rm = TRUE)

ggplot(gemcitabine_table, aes(x = ConcA, y = ConcB, fill = X.Viability)) +
  geom_tile() +
  scale_x_log10() +
  scale_y_log10() +
  scale_fill_gradient(low = "blue", high = "red", limits = c(1.32, 100)) +
  xlab('Gemcitabine (nM)') +
  ylab('Ceralasertib (nM)') +
  ggtitle('Heatmap of Gemcitabine with Ceralasertib')

#same with these!


#maybe make a 3D synthetic lethality plot

Figure 6 (but really figure 5)

library(rgl)

adavosertib_matrix <- xtabs(X.Viability ~ ConcA + ConcB, data = adavosertib_table)
x <- sort(unique(adavosertib_table$ConcA))
y <- sort(unique(adavosertib_table$ConcB))

persp(
  x = x,
  y = y,
  z = adavosertib_matrix,
  col = "lightpink",
  theta = 120,
  phi = 20,
  expand = 0.2,
  ticktype = 'detailed',
  xlab = "Ceralasertib (nM)",
  ylab = "Adavosertib (nM)",
  zlab = "Percent Viability",
  main = "3D Plot of Viability of Ceralasertib with Adavosertib"
)

#this is the same plot but in 3D!
persp3d(
  x = x,
  y = y,
  z = adavosertib_matrix,
  col = "lightpink",
  ticktype = 'detailed',
  xlab = "Ceralasertib (nM)",
  ylab = "Adavosertib (nM)",
  zlab = "Percent Viability",
  main = "3D Plot of Viability of Ceralasertib with Adavosertib"
)



prexasertib_matrix <- xtabs(X.Viability ~ ConcA + ConcB, data = prexasertib_table)
x <- sort(unique(prexasertib_table$ConcA))
y <- sort(unique(prexasertib_table$ConcB))

persp(
  x = x,
  y = y,
  z = prexasertib_matrix,
  col = "lightgreen",
  theta = 120,
  phi = 20,
  expand = 0.2,
  ticktype = 'detailed',
  xlab = "Ceralasertib (nM)",
  ylab = "Prexasertib (nM)",
  zlab = "Percent Viability",
  main = "3D Plot of Viability of Ceralasertib with Prexasertib"
)

persp3d(
  x = x,
  y = y,
  z = prexasertib_matrix,
  col = "lightgreen",
  ticktype = 'detailed',
  xlab = "Ceralasertib (nM)",
  ylab = "Prexasertib (nM)",
  zlab = "Percent Viability",
  main = "3D Plot of Viability of Ceralasertib with Prexasertib"
)


gemcitabine_matrix <- xtabs(X.Viability ~ ConcA + ConcB, data = gemcitabine_table)
x <- sort(unique(gemcitabine_table$ConcA))
y <- sort(unique(gemcitabine_table$ConcB))

persp(
  x = x,
  y = y,
  z = gemcitabine_matrix,
  col = "lightblue",
  theta = 120,
  phi = 20,
  expand = 0.2,
  ticktype = 'detailed',
  xlab = "Ceralasertib (nM)",
  ylab = "Gemcitabine (nM)",
  zlab = "Percent Viability",
  main = "3D Plot of Viability of Ceralasertib with Gemcitabine"
)

persp3d(
  x = x,
  y = y,
  z = gemcitabine_matrix,
  col = "lightblue",
  ticktype = 'detailed',
  xlab = "Ceralasertib (nM)",
  ylab = "Gemcitabine (nM)",
  zlab = "Percent Viability",
  main = "3D Plot of Viability of Ceralasertib with Gemcitabine"
)















