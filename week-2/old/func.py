#!/usr/bin/env python3.4

def id(n):         return n
def cube(n):      return n*n*n
def square(n):    return n*n
def fact(n): return  1 if n <= 1  else n*fact(n-1)

def sum(f): return lambda a,b:  0 if a > b else f(a) + sum(f)(a+1,b)

def sumInts():  return sum(lambda x: x)
def sumCubes(): return sum(lambda x: x*x*x)
def sumFact():  return sum(fact)




if __name__ == "__main__":
    print("done")
