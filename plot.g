# plot.g -- gnuplot コマンド・ファイル
set term postscript eps color enhanced
set output "./bicg.eps"
set xrange[0:100]
set xlabel "Number of Iterations"
set logscale y
set format y "10^{%L}"
set ylabel "Residual Error"
plot "./data/0.9375.dat" title "{/Symbol g} = 1 - (1/2)^4" w l,\
    "./data/0.875.dat" title "{/Symbol g} = 1 - (1/2)^3" w l,\
    "./data/0.75.dat" title "{/Symbol g} = 1 - (1/2)^2" w l,\
    "./data/0.5.dat" title "{/Symbol g} = 1/2" w l,\
    "./data/0.25.dat" title "{/Symbol g} = (1/2)^2" w l,\
    "./data/0.125.dat" title "{/Symbol g} = (1/2)^3" w l,\
    "./data/0.0625.dat" title "{/Symbol g} = (1/2)^4" w l
