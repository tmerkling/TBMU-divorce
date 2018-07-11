########################################
### Analyses causes of divorce TBMU ###
#######################################

setwd("D:/Thomas/Google Drive/postdoc/FQRNT murre project/murre data")

murre_RS = read.table("RS murre data total.csv", sep = ",", header = TRUE)

murre_RS$Plot = substr(murre_RS$PlotSite,1,1)

Q_plot_RS = subset(murre_RS, Plot == "Q") # the best studied plot

# overall divorce rates depending on year

Year = with(murre_RS, c(YearSeason_1,YearSeason_2,YearSeason_3,YearSeason_4,YearSeason_5,YearSeason_6,YearSeason_7,YearSeason_8,YearSeason_9,YearSeason_10,YearSeason_11,YearSeason_12,YearSeason_13,YearSeason_14,YearSeason_15,YearSeason_16,YearSeason_17,YearSeason_18,YearSeason_19,YearSeason_20,YearSeason_21,YearSeason_22,YearSeason_23))

Outcome = with(murre_RS, c(as.character(PartnerOutcome_1),as.character(PartnerOutcome_2),as.character(PartnerOutcome_3),as.character(PartnerOutcome_4),as.character(PartnerOutcome_5),as.character(PartnerOutcome_6),as.character(PartnerOutcome_7),as.character(PartnerOutcome_8),as.character(PartnerOutcome_9),as.character(PartnerOutcome_10),as.character(PartnerOutcome_11),as.character(PartnerOutcome_12),as.character(PartnerOutcome_13),as.character(PartnerOutcome_14),as.character(PartnerOutcome_15),as.character(PartnerOutcome_16),as.character(PartnerOutcome_17),as.character(PartnerOutcome_18),as.character(PartnerOutcome_19),as.character(PartnerOutcome_20),as.character(PartnerOutcome_21),as.character(PartnerOutcome_22),as.character(PartnerOutcome_23)))

Year_Outcome = data.frame(cbind(Year, Outcome))
Year_Outcome = Year_Outcome[Year_Outcome$Year != 0, ]
Year_Outcome$Year = factor(Year_Outcome$Year)
Year_Outcome$Outcome = factor(Year_Outcome$Outcome)

table_DivRate = with(Year_Outcome, table(Year,Outcome))
Pair_Outcome = data.frame(Year = row.names(table_DivRate), DEAD = table_DivRate[,1], DIV = table_DivRate[,2],NO = table_DivRate[,3], SAME = table_DivRate[,4])
Pair_Outcome$PropDiv = with(Pair_Outcome, DIV/(DIV+DEAD+SAME))
Pair_Outcome$PropDead = with(Pair_Outcome, DEAD/(DIV+DEAD+SAME))
Pair_Outcome$PropNoInfo = with(Pair_Outcome, NO/(NO+DIV+DEAD+SAME))

# focus on Q plot only

Year_Qplot = with(Q_plot_RS, c(YearSeason_1,YearSeason_2,YearSeason_3,YearSeason_4,YearSeason_5,YearSeason_6,YearSeason_7,YearSeason_8,YearSeason_9,YearSeason_10,YearSeason_11,YearSeason_12,YearSeason_13,YearSeason_14,YearSeason_15,YearSeason_16,YearSeason_17,YearSeason_18,YearSeason_19,YearSeason_20,YearSeason_21,YearSeason_22,YearSeason_23))

Outcome_Qplot = with(Q_plot_RS, c(as.character(PartnerOutcome_1),as.character(PartnerOutcome_2),as.character(PartnerOutcome_3),as.character(PartnerOutcome_4),as.character(PartnerOutcome_5),as.character(PartnerOutcome_6),as.character(PartnerOutcome_7),as.character(PartnerOutcome_8),as.character(PartnerOutcome_9),as.character(PartnerOutcome_10),as.character(PartnerOutcome_11),as.character(PartnerOutcome_12),as.character(PartnerOutcome_13),as.character(PartnerOutcome_14),as.character(PartnerOutcome_15),as.character(PartnerOutcome_16),as.character(PartnerOutcome_17),as.character(PartnerOutcome_18),as.character(PartnerOutcome_19),as.character(PartnerOutcome_20),as.character(PartnerOutcome_21),as.character(PartnerOutcome_22),as.character(PartnerOutcome_23)))

Year_Outcome_Qplot = data.frame(cbind(Year_Qplot, Outcome_Qplot))
Year_Outcome_Qplot = Year_Outcome_Qplot[Year_Outcome_Qplot$Year != 0, ]
Year_Outcome_Qplot$Year = factor(Year_Outcome_Qplot$Year)
Year_Outcome_Qplot$Outcome = factor(Year_Outcome_Qplot$Outcome)

table_DivRate_Qplot = with(Year_Outcome_Qplot, table(Year,Outcome))
Pair_Outcome_Qplot = data.frame(Year = row.names(table_DivRate_Qplot), DEAD = table_DivRate_Qplot[,1], DIV = table_DivRate_Qplot[,2],NO = table_DivRate_Qplot[,3], SAME = table_DivRate_Qplot[,4])
Pair_Outcome_Qplot$PropDiv = with(Pair_Outcome_Qplot, DIV/(DIV+DEAD+SAME))
Pair_Outcome_Qplot$PropDead = with(Pair_Outcome_Qplot, DEAD/(DIV+DEAD+SAME))
Pair_Outcome_Qplot$PropNoInfo = with(Pair_Outcome_Qplot, NO/(NO+DIV+DEAD+SAME))
