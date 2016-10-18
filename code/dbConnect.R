library(RMySQL)
drv = dbDriver("MySQL")

# Data connection
user = "chilean_movement"
host = "127.0.0.1"
password = "4p6vX22XL8YncnmN"
dbname = "chilean_movement"
port = 3306

# SSL Tunnel
# ssh -N -L 3306:localhost:3306 administrator@dgomezara.cl

# Connection
mydb = dbConnect(drv, user = user, password = password, dbname = dbname, port = port, host = host, client.flag = CLIENT_MULTI_STATEMENTS)
