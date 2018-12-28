arq = open("cod.txt", "r") 
names = arq.readlines()
arq.close()

arqName = "stringsCod.txt"

file = open(arqName,"w")

file.write("c(")

k = 1

for i in names:
	file.write('"' + i.rstrip() + '" = ' + str(k) + ',')
	k+=1
	
file.write(")")
file.close()