arq = open("attr.txt", "r") 
names = arq.readlines()
arq.close()
file = open("string.txt","w")

file.write('select(columns')


for i in names:
	file.write(', contains("' + (i.rstrip()).lower() + '")')
	
file.write(")")

file.close()