from collections.abc import Iterable
from itertools import repeat
from typing import Callable, TypeVar

T = TypeVar('T')

def count_horizontal_vertical_overlaps(
lines: list[tuple[tuple[int, int], tuple[int, int]]]) -> int:
    vents: dict[tuple[int, int], bool] = {}
    overlaps = 0
    def add(start: int, end: int, mk_coord: Callable[[int], tuple[int, int]]
    ) -> None:
        nonlocal overlaps
        for i in range(min(start, end), max(start, end) + 1):
            c = mk_coord(i)
            if c in vents:
                if not vents[c]:
                    overlaps += 1
                    vents[c] = True
            else:
                vents[c] = False
    for (x1, y1), (x2, y2) in lines:
        if x1 == x2:
            add(y1, y2, lambda y: (x1, y))
        elif y1 == y2:
            add(x1, x2, lambda x: (x, y1))
    return overlaps

def mk_range(start: int, end: int) -> Iterable[int]:
    if start < end:
        return range(start, end + 1)
    if start > end:
        return range(start, end - 1, -1)
    return repeat(start)

def count_all_overlaps(
lines: list[tuple[tuple[int, int], tuple[int, int]]]) -> int:
    vents: dict[tuple[int, int], bool] = {}
    overlaps = 0
    for (x1, y1), (x2, y2) in lines:
        for c in zip(mk_range(x1, x2), mk_range(y1, y2)):
            if c in vents:
                if not vents[c]:
                    overlaps += 1
                    vents[c] = True
            else:
                vents[c] = False
    return overlaps

def pair(it: Iterable[T]) -> tuple[T, T]:
    a, b = it
    return a, b

with open('input.txt') as f:
    lines = [pair(pair(map(int, c.split(',')))
                  for c in l.rstrip().split(' -> ')) for l in f]
print(count_horizontal_vertical_overlaps(lines))
print(count_all_overlaps(lines))
