# BiCGMethodForCRS

CRS形式で行列を保存し、Bi-CG法によって連立一次方程式を解く。

## Usage

```
$ gfortran crs_bicg.f90 -o crs_bicg
$ ./ex.sh
$ gnuplot plot.g
```

## Output
- ./data/N.dat
    - gammaをNにした場合の計算結果。反復回数と各反復での相対残差が含まれる。
- ./bicg.eps
    - ./data/N.datをプロットした画像。
