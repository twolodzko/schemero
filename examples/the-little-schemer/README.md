
The code example come from *The Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme:

```shell
$ hyperfine -m 50 --warmup 1 'scheme --quiet < examples/the-little-schemer/run-all.scm' './schemero examples/the-little-schemer/run-all.scm' 
Benchmark 1: scheme --quiet < examples/the-little-schemer/run-all.scm
  Time (mean ± σ):     214.7 ms ±   4.0 ms    [User: 166.3 ms, System: 48.4 ms]
  Range (min … max):   208.7 ms … 228.7 ms    50 runs
 
Benchmark 2: ./schemero examples/the-little-schemer/run-all.scm
  Time (mean ± σ):      1.083 s ±  0.023 s    [User: 1.664 s, System: 0.164 s]
  Range (min … max):    1.045 s …  1.153 s    50 runs
 
Summary
  'scheme --quiet < examples/the-little-schemer/run-all.scm' ran
    5.04 ± 0.14 times faster than './schemero examples/the-little-schemer/run-all.scm'
```
