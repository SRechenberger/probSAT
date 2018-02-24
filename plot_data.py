#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plot
import sys
import math

fname = sys.argv[1]

data = np.genfromtxt(fname, delimiter=',', names=True)

entropy = []
flips   = []
cb      = []

for (e,f,c) in data:
    entropy.append(e)
    flips.append(f)
    cb.append(c)

avg = {}
for (e,f,c) in data:
    if e in avg:
        avg[e].append(f)
    else:
        avg[e] = [f]

entropy_flipAverage = []
flipAverage = []
flipMax = []
flipMin = []
for e, fs in sorted(avg.items()):
    entropy_flipAverage.append(e)
    flipAverage.append(np.median(fs))
    flipMax.append(max(fs))
    flipMin.append(min(fs))



entropy_flip_fig, entropy_flip_ax = plot.subplots()

entropy_flip_ax.grid(linestyle='--')
entropy_flip_ax.scatter(entropy, flips, s=0.1,marker='v')
entropy_flip_ax.plot(entropy_flipAverage, flipAverage, linewidth=0.5,color='red')
entropy_flip_ax.plot(entropy_flipAverage, flipMin, linewidth=0.5,color='green')
# entropy_flip_ax.plot(entropy_flipAverage, flipMax, linewidth=0.5,color='yellow')
plot.savefig(fname+'.entropy_flip.pdf')

cb_entropy_fig, cb_entropy_ax = plot.subplots()

cb_entropy_ax.scatter(cb, entropy, s=1)
plot.savefig(fname+'.cb_entropy.pdf')
