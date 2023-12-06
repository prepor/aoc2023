import sympy as sp
import operator
from functools import reduce

input_data = """
Time:      7  15   30
Distance:  9  40  200
"""

input_data = open('../files/input6.txt').read()

lines = input_data.strip().split('\n')
times = list(map(int, lines[0].split()[1:]))
distances = list(map(int, lines[1].split()[1:]))

pairs = list(zip(times, distances))

def solve(t, d):
  c = sp.symbols('c')
  eq = sp.Eq(c**2 - t * c + d, 0)

  start, end = sp.solve(eq, c)
  start = start + 1 if start.is_integer else sp.ceiling(start)
  end = end - 1 if end.is_integer else sp.floor(end)
  return end - start + 1
    

ways = []
for t, d in pairs:
  ways.append(solve(t, d))

print("Part 1: {}".format(reduce(operator.mul, ways)))

time = int(''.join(map(str, times)))
distance = int(''.join(map(str, distances)))

print("Part 2: {}".format(solve(time, distance)))
