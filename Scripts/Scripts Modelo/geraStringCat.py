arq = open("attrCat.txt", "r") 
names = arq.readlines()
arq.close()
file = open("stringCat.txt","w")

file.write('select(attr')


for i in names:
	file.write(', ' + i.rstrip())
	
file.write(")")

file.close()