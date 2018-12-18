arq = open("attrNumericos.txt", "r") 
names = arq.readlines()
arq.close()
file = open("stringNumerica.txt","w")

file.write('select(attr')


for i in names:
	file.write(', ' + i.rstrip())
	
file.write(")")

file.close()