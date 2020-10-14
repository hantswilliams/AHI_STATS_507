#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 14 11:16:56 2020

@author: hantswilliams

Week 8 STATS - ANOVA - 2way 


"""

# IF USING JUPYTER NOTEBOOK / COLAB -> uncomment below lines and run at top of notebook: 

#!sudo apt-get install python3-dev default-libmysqlclient-dev
# !pip install pymysql

#Packages for SQL CONNECTION 
from sqlalchemy import create_engine
import sqlalchemy 

#Packages for DATAFRAME 
import pandas as pd

dataframe_diabetes = pd.read_csv("https://raw.githubusercontent.com/hantswilliams/AHI_STATS_507/main/Week_8_ANOVAS/DataSet/diabetic_data.csv")

MYSQL_HOSTNAME = '3.84.158.190' # you probably don't need to change this
MYSQL_USER = 'dba'
MYSQL_PASSWORD = 'ahi2020'
MYSQL_DATABASE = 'examples'

connection_string = f'mysql+pymysql://{MYSQL_USER}:{MYSQL_PASSWORD}@{MYSQL_HOSTNAME}/{MYSQL_DATABASE}'
engine = create_engine(connection_string)

#print (engine.table_names())

dataframe_diabetes.to_sql('diabetes_example', con=engine, if_exists='append')
