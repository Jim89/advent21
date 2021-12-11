f = open("2/input.txt")
lines = [(line.split()[0], int(line.split()[1])) for line in f.readlines()]
f.close()
lines[0]

def calc(lines, dir):
    return sum([l[1] for l in lines if l[0] == dir])

calc(lines, "forward") * (calc(lines, "down") - calc(lines, "up"))


angle = 0
hpos = 0
vpos = 0
for line in lines:
    if line[0] == "forward":
        hpos += line[1]
        vpos += (line[1] * angle)
    else:
        if line[0] == "down":
            angle += line[1]
        else:
            angle -= line[1]
vpos * hpos