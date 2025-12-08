# Advent of Code 2025 - Day 08

import os
import strformat
import strutils
import sequtils
import sugar
import math
import algorithm
import strscans
import re
import tables
import deques

type Junction = tuple
  x: int
  y: int
  z: int

proc euclidianDistance(a: Junction, b: Junction): float =
  result = sqrt(float((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2 + (a.z - b.z) ^ 2))

# Union find implementation, usually we have a rank to show the height
# of a tree in the forst, but for our specific aoc problem, the sizes
# of the forst is strictly better !
type UnionFind = object
  parent: seq[int]
  sizes: seq[int] # size of the sets
  count: int      # number of set

proc initUnionFind(size: int): UnionFind =
  result.count = size
  result.parent = newSeq[int](size)
  result.sizes = newSeq[int](size)
  for i in 0 ..< size:
    result.parent[i] = i
    result.sizes[i] = 1

# find the parent of x
proc find(uf: var UnionFind, x: int): int =
  if uf.parent[x] != x:
    uf.parent[x] = uf.find(uf.parent[x])
  result = uf.parent[x]

# union of the two sets of x and y
proc union(uf: var UnionFind, x, y: int) =
  let parentX = uf.find(x)
  let parentY = uf.find(y)
  if parentX != parentY:
    if uf.sizes[parentX] < uf.sizes[parentY]:
      uf.parent[parentX] = parentY
      uf.sizes[parentY] += uf.sizes[parentX]
    else:
      uf.parent[parentY] = parentX
      uf.sizes[parentX] += uf.sizes[parentY]
    uf.count -= 1

# find the top n biggest set in the structure
proc max(uf: var UnionFind, n: int): seq[int] =
  var sizes: seq[int] = @[]
  for i in 0 ..< uf.parent.len:
    if uf.parent[i] == i:
      sizes.add(uf.sizes[i])
  sizes.sort(SortOrder.Descending)
  result = sizes[0 ..< min(n, sizes.len)]

proc parseLine(line: string): Junction =
  let parts = line.split(',')
  result = (x: parseInt(parts[0]), y: parseInt(parts[1]), z: parseInt(parts[2]))

proc readFile[A](transformer: (string) -> A, filepath: string): seq[A] =
  var results: seq[A] = @[]
  for line in lines(filepath):
    results.add(transformer(line))
  result = results

proc solve(junctions: seq[Junction], cables: int): (int, int) =
  type TEdge = (float, int, int, (Junction, Junction))
  let size = junctions.len
  var edges: seq[TEdge] = @[]
  var uf = initUnionFind(junctions.len)
  for i in 0..<(size-1):
    for j in (i+1) ..< size:
      let dist = euclidianDistance(junctions[i], junctions[j])
      edges.add((dist, i, j, (junctions[i], junctions[j])))
  edges.sort()

  var q = edges.toDeque
  var last: TEdge

  var remainingCables = cables
  while remainingCables > 0:
    last = q.popFirst()
    let (_, x, y, _) = last
    uf.union(x, y)
    remainingCables -= 1


  let score1 = uf.max(3).foldl(a * b)

  while uf.count > 1:
    last = q.popFirst()
    let (_, x, y, _) = last
    uf.union(x, y)

  let (_, _, _, (j1, j2)) = last
  let score2 = j1.x * j2.x
  result = (score1, score2)

when isMainModule:
  let filepath = paramStr(1)
  let cables = if "day08-example" in filepath: 10 else: 1000
  let junctions = readFile(parseLine, filepath)
  let (score1, score2) = solve(junctions, cables)
  echo fmt"Part 1: {score1}"
  echo fmt"Part 2: {score2}"
