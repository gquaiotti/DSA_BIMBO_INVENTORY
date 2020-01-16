# DATA SCIENCE ACADEMY 
# Big Data Analytics com R e Microsoft Azure Machine Learning
#
# Model to accurately predict inventory demand based on data sales histories
#
# Gabriel Quaiotti
# Jan 2020
#
# In this competition, you will forecast the demand of a product for a given week, at a 
# particular store. 
# The dataset you are given consists of 9 weeks of sales transactions in Mexico. Every week, 
# there are delivery 
# trucks that deliver products to the vendors. Each transaction consists of sales and returns. 
# Returns are the products that are unsold and expired. The demand for a product in a certain 
# week is defined as the sales this week subtracted by the return next week.
# 
# 
# The test and test dataset are split based on time, as well as the public and private 
# leaderboard dataset split.
# 
# 
# Things to note:
#   
# There may be products in the test set that don't exist in the test set. This is the expected 
# behavior of inventory data, 
# since there are new products being sold all the time. Your model should be able to accommodate 
# this.
#
# The adjusted demand (Demanda_uni_equil) is always >= 0 since demand should be either 0 or a 
# positive value. The reason that Venta_uni_hoy - Dev_uni_proxima 
# sometimes has negative values is that the returns records sometimes carry over a few weeks.

# File descriptions
# test.csv — the testing set
# test.csv — the test set
# sample_submission.csv — a sample submission file in the correct format
# cliente_tabla.csv — client names (can be joined with test/test on Cliente_ID)
# producto_tabla.csv — product names (can be joined with test/test on Producto_ID)
# town_state.csv — town and state (can be joined with test/test on Agencia_ID)

# Data fields
# Semana — Week number (From Thursday to Wednesday)
# Agencia_ID — Sales Depot ID
# Canal_ID — Sales Channel ID
# Ruta_SAK — Route ID (Several routes = Sales Depot)
# Cliente_ID — Client ID
# NombreCliente — Client name
# Producto_ID — Product ID
# NombreProducto — Product Name
# Venta_uni_hoy — Sales unit this week (integer)
# Venta_hoy — Sales this week (unit: pesos)
# Dev_uni_proxima — Returns unit next week (integer)
# Dev_proxima — Returns next week (unit: pesos)
# Demanda_uni_equil — Adjusted Demand (integer) (This is the target you will predict)

setwd('D:/Github/DSA_BIMBO_INVENTORY')

library(data.table)
library(lares)
library(Metrics)

v_c_file_test <- "dataset/test_split.csv"

###################################
# test DATASET
###################################
test <- data.table::fread(file = v_c_file_test)

test_sample <- test[sample(nrow(test), 100000)]

glm_model <- readRDS(file = 'model/glm_model.rds')

test_sample$glm_predict <- round(predict(object = glm_model, newdata = test_sample), 0)

test_sample[glm_predict < 0, glm_predict := 0]

#RMSLE
rmsle(test_sample[, Demanda_uni_equil], test_sample[, glm_predict])

lares::mplot_full(tag = test_sample[, Demanda_uni_equil], 
                  score = test_sample[, glm_predict],
                  splits = 10,
                  subtitle = "Balance Regression Model",
                  model_name = "GLM Model",
                  save = T)

remove(glm_model)
gc()

lqs_model <- readRDS(file = 'model/lqs_model.rds')

test_sample$mass_lqs_predict <- ceiling(predict(object = lqs_model, newdata = test_sample))

test_sample[mass_lqs_predict < 0, mass_lqs_predict := 0]

#RMSLE
rmsle(test_sample[, Demanda_uni_equil], test_sample[, mass_lqs_predict])

lares::mplot_full(tag = test_sample[, Demanda_uni_equil],
                  score = test_sample[, mass_lqs_predict],
                  splits = 10,
                  subtitle = "Balance Regression Model",
                  model_name = "MASS LQS Model",
                  save = T)

remove(lqs_model)
gc()

rlm_model <- readRDS(file = 'model/rlm_model.rds')

test_sample$mass_rlm_predict <- ceiling(predict(object = rlm_model, newdata = test_sample))

test_sample[mass_rlm_predict < 0, mass_rlm_predict := 0]

#RMSLE
rmsle(test_sample[, Demanda_uni_equil], test_sample[, mass_rlm_predict])

lares::mplot_full(tag = test_sample[, Demanda_uni_equil],
                  score = test_sample[, mass_rlm_predict],
                  splits = 10,
                  subtitle = "Balance Regression Model",
                  model_name = "MASS RLM Model",
                  save = T)

remove(rlm_model)
gc()

caret_model <- readRDS(file = 'model/caret_model.rds')

test_sample$caret_predict <- ceiling(predict(object = caret_model, newdata = test_sample))

test_sample[caret_predict < 0, caret_predict := 0]

#RMSLE
rmsle(test_sample[, Demanda_uni_equil], test_sample[, caret_predict])

lares::mplot_full(tag = test_sample[, Demanda_uni_equil],
                  score = test_sample[, caret_predict],
                  splits = 10,
                  subtitle = "Balance Regression Model",
                  model_name = "Caret Model - lm",
                  save = T)

remove(caret_model)
gc()

rm(list=ls())
gc()
