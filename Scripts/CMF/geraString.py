arq = open("attr.txt", "r") 
names = arq.readlines()
arq.close()

arqName = "strings.txt"

file = open(arqName,"w")

file.write("select(base")

for i in names:
	file.write(', contains("' + (i.rstrip()).lower() + '")')
	
file.write(")")
file.close()