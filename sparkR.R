#Sys.setenv(SPARK_HOME='/Users/aimeebarciauskas/Documents/spark-1.6.0-bin-hadoop2.6')
Sys.getenv('SPARK_HOME')

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

sc <- sparkR.init(master = "local[2]", 
                  sparkEnvir = list(spark.driver.memory="500m"))

# you can load additional spark packages together with creating context
# e.g. for loading csv directly
#sc <- sparkR.init(sparkPackages="com.databricks:spark-csv_2.11:1.0.3")

# check the object
sc
class(sc)
