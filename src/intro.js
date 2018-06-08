xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
squares = [ for (x of xs) x * x ],
evens = [ for (x of xs) if (x % 2 === 0) x ],
many_a = [ for (x of xs) for (y of xs) 'a' ],
hundred = [ for (i of [0])
            for (x of Array(5))
            for (y of Array(20)) i = i + 1 ],
[xs, squares, evens, many_a, hundred]
