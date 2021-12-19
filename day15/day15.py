from dataclasses import dataclass, field
from heapq import heappush, heappop

def search(grid: list[list[int]]) -> int:

    dr = len(grid) - 1
    dc = len(grid[dr]) - 1

    @dataclass(order=True)
    class Node:
        f: int = field(init=False)
        pos: tuple[int, int]
        g: int
        def __post_init__(self) -> None:
            self.f = self.g + (dr - self.pos[0]) + (dc - self.pos[1])

    fringe = [Node((0, 0), 0)]
    visited: set[tuple[int, int]] = set()
    while True:
        cur = heappop(fringe)
        if cur.pos == (dr, dc):
            return cur.g
        if cur.pos in visited:
            continue
        visited.add(cur.pos)
        cr, cc = cur.pos
        for r, c in [(cr + 1, cc), (cr, cc + 1), (cr - 1, cc), (cr, cc - 1)]:
            if 0 <= r <= dr and 0 <= c <= dc:
                heappush(fringe, Node((r, c), cur.g + grid[r][c]))

def search_large(init_grid: list[list[int]]) -> int:
    return search([[(x + i + j - 1) % 9 + 1 for j in range(5) for x in r]
                   for i in range(5) for r in init_grid])

with open('input.txt') as f:
    grid = [list(map(int, r)) for r in f.read().splitlines()]
print(search(grid))
print(search_large(grid))
