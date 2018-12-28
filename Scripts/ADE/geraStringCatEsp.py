arq = open("levels.txt", "r") 
names = arq.readlines()
arq.close()
file = open("stringleveldeppast.txt","w")

k = 0

file.write('dados$deppast <- revalue(dados$deppast, c(')

for i in names:
	file.write(i.rstrip() + " = " + str(k) + ", ")
	k += 1
	
file.write("))")

file.close()