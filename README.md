# FILTER

Experimental implementation of
[FIR](https://en.wikipedia.org/wiki/Finite_impulse_response) and
[IIR](https://en.wikipedia.org/wiki/Infinite_impulse_response) digital filters

## Usage

```erlang
1> application:start(sasl).
2> application:start(gproc).
3> application:start(filter).
4> FirParams = [1, 1.25].
5> IirParams = [0.8].
6> Destination = self().
7> Filter = filter:new(FirParams, IirParams, Destination).
8> filter:in(Filter, 1).
ok
9> filter:out(Filter).   
1.0
10> filter:in(Filter, 0).
ok
11> filter:out(Filter).  
0.44999999999999996
12> filter:in(Filter, 0).
ok
13> filter:out(Filter).  
-0.36
14> filter:in(Filter, 0).
ok
15> filter:out(Filter).  
0.288
```



