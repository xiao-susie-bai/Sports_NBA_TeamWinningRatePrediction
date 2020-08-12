setwd("~/Desktop")

# import library 
library(data.table)
library(plyr)
library(plm)
library(pglm)
library(panelAR)

# import data 
dfeast = fread('dfeast.csv', header = TRUE)
df_east = dfeast[,-c('league rank','Player')]

dfwest = fread('dfwest.csv', header = TRUE)
df_west = dfwest[,-c('league rank','Player')]

#### east conference

# linear regression 
lm_east<-lm(df_east$`winning PCT`~.,data=df_east)
summary(lm_east)

# stepwise 
tstep<-step(lm_east)
summary(tstep)

# set up the panel data 
pgr <-pdata.frame(df_east, index = c("Tm", "Year"))

# mixed effect model
gr_pool <- plm(pgr$`winning PCT` ~ Age + G + MP + PER + 
                 TS. + X3PAr + FTr + ORB. + DRB. + TRB. + AST. + STL. + BLK. + 
                 TOV. + USG. + OWS + DWS + WS.48 + OBPM + DBPM + VORP + FG. + 
                 X3P + X2P. + FT + FT. + ORB + DRB + AST + STL + BLK + TOV + 
                 PF + Pos_PG, data = pgr,
               model = "pooling")
summary(gr_pool)

# fixed effect model
gr_fe <- plm(pgr$`winning PCT` ~ Age + G + MP + PER + 
               TS. + X3PAr + FTr + ORB. + DRB. + TRB. + AST. + STL. + BLK. + 
               TOV. + USG. + OWS + DWS + WS.48 + OBPM + DBPM + VORP + FG. + 
               X3P + X2P. + FT + FT. + ORB + DRB + AST + STL + BLK + TOV + 
               PF + Pos_PG
             , data = pgr,
             model = "within")
summary(gr_fe)

# use F-test to test if there are significant difference
pFtest(gr_fe, gr_pool)


#### west conference

# linear regression 
lm_west<-lm(df_west$`winning PCT`~.,data=df_west)
summary(lm_west)

# stepwise 
step<-step(lm_west)
summary(step)

# set up the panel data 
pgr <-pdata.frame(df_east, index = c("Tm", "Year"))

# mixed effect model
gr_pool <- plm(pgr$`winning PCT` ~ Age + G + MP + PER + 
                 X3PAr + FTr + ORB. + DRB. + TRB. + STL. + BLK. + USG. + OWS + 
                 DWS + WS.48 + OBPM + DBPM + VORP + FG. + X3P + X2P. + FT + 
                 FTA + FT. + DRB + STL + BLK + TOV + PF + Pos_C + Pos_PF + 
                 Pos_PG, data = pgr,
               model = "pooling")
summary(gr_pool)

# fixed effect model
gr_fe <- plm(pgr$`winning PCT` ~ Age + G + MP + PER + 
               X3PAr + FTr + ORB. + DRB. + TRB. + STL. + BLK. + USG. + OWS + 
               DWS + WS.48 + OBPM + DBPM + VORP + FG. + X3P + X2P. + FT + 
               FTA + FT. + DRB + STL + BLK + TOV + PF + Pos_C + Pos_PF + 
               Pos_PG
             , data = pgr,
             model = "within")
summary(gr_fe)

# use F-test to test if there are significant difference
pFtest(gr_fe, gr_pool)



