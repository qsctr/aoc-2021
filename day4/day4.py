from typing import Optional

def first_win(draws: list[int], boards: list[list[list[int]]]) -> int:
    bs: list[list[list[Optional[int]]]] = [
        [[x for x in r] for r in b] for b in boards]
    for n in draws:
        for b in bs:
            for r in b:
                for i, x in enumerate(r):
                    if x == n:
                        r[i] = None
                        if (all(x1 is None for x1 in r)
                                or all(r1[i] is None for r1 in b)):
                            return sum(x1 or 0 for r1 in b for x1 in r1) * n
    return 0

def last_win(draws: list[int], boards: list[list[list[int]]]) -> int:
    bs: list[list[list[Optional[int]]]] = [
        [[x for x in r] for r in b] for b in boards]
    for n in draws:
        bs1 = []
        for b in bs:
            for r in b:
                for j, x in enumerate(r):
                    if x == n:
                        r[j] = None
                        if (all(x1 is None for x1 in r)
                                or all(r1[j] is None for r1 in b)):
                            if len(bs) == 1:
                                return sum(
                                    x1 or 0 for r1 in b for x1 in r1) * n
                            break
                else:
                    continue
                break
            else:
                bs1.append(b)
        bs = bs1
    return 0

with open('input.txt') as f:
    draw_str, *board_strs = f.read().split('\n\n')
draws = list(map(int, draw_str.split(',')))
boards = [
    [[int(s) for s in l.split()] for l in b.splitlines()] for b in board_strs]
print(first_win(draws, boards))
print(last_win(draws, boards))
