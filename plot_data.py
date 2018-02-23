#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plot
import sys

data = np.genfromtxt(sys.argv[1], delimiter=',', names=True)

entropy = []
flips   = []
cb      = []

for (e,f,c) in data:
    entropy.append(e)
    flips.append(f)
    cb.append(c)

entropy_flip_fig, entropy_flip_ax = plot.subplots()

entropy_flip_ax.scatter(entropy, flips, s=1)
plot.savefig('entropy_flip.pdf')

cb_entropy_fig, cb_entropy_ax = plot.subplots()

cb_entropy_ax.scatter(cb, entropy, s=1)
plot.savefig('cb_entropy.pdf')
