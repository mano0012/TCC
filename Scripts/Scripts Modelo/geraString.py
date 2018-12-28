arq = open("attr.txt", "r") 
names = arq.readlines()
arq.close()
file = open("convertCat.txt","w")

for i in names:
	file.write('dados$' + i.rstrip() + ' <- revalue(dados$' + i.rstrip() + ', c("Unknown" = "N/A", "Refused" = "N/A"))\n')
	
file.close()