f = open("3/input.txt")
lines = [line.strip("\n") for line in f.readlines()]
f.close()

sums = [sum([int(line[i]) for line in lines]) for i in range(12)]

gamma = int(''.join([str(int(x >= 500)) for x in sums]), base = 2)
epsilon = int(''.join([str(int(x <= 500)) for x in sums]), base = 2)
gamma * epsilon

