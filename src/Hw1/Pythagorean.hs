-- A triple (x,y,z) is Pythagorean when x^2 + y^2 ==z^2, write a
-- function that computes a list of all Pythagorean triples whole
-- elements are at most a given n

pythagorean n = [
    (x,y,z) | x <-map (^2) [1..n],
              y <-map (^2) [1..n],
              z <-map (^2) [1..n],
              x + y == z,
              x <= n, y <= n, z <= n ]