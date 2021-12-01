# Make the input
f = open("1/input.txt")
numbers = [int(n) for n in f]
f.close()

# Answer 1
sum([number > numbers[index-1] for index, number in enumerate(numbers) if index > 0])

# Answer 2
groups = [sum(numbers[index:index+3]) for index in range(len(numbers))]
sum([number > groups[index-1] for index, number in enumerate(groups) if index > 0])
