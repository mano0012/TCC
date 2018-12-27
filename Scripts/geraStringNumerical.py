arq = open("attr.txt", "r") 
names = arq.readlines()
arq.close()
file = open("convert1Numerica.txt","w")

for i in names:
	file.write('dados$' + i.rstrip() + ' <- revalue(dados$' + i.rstrip() + ', c("No" = 0, "Yes" = 1, "N/A" = -1, "Unknown" = -2, "Refused" = -3))\n')
	
file.close()