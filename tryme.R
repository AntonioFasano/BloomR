
## A simple test script
con=br.open()
data=br.bulk.csv(con, "mybloomr/tickers.csv") 
br.close(con)

print(data)



