# Benchmarking the cost of (some of) `transformers`

These benchmarks can easily be ran with [Nix](http://nixos.org/nix). Assuming you have Nix installed, you can build the benchmarks executable with:

```
nix-build
```

Benchmarks can than be ran with

```
./result -o report.html
```

If you don't have Nix, you'll have to manually install `transformer-ocharles` with Cabal. You may get different results as you'll be in a slightly different environment.
