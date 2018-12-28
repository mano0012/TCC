arq = open("attr.txt", "r") 
names = arq.readlines()
arq.close()

arqName = "stringRemapeamento.txt"

file = open(arqName,"w")

for i in names:
	file.write('dados$' + i.rstrip().lower() + ' <- revalue(dados$' + i.rstrip().lower() + ', c("N/A" = -99))\n')
	
file.close()