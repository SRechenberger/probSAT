CC=gcc
CFLAGS= -Wall -Wextra -static -O3 -funroll-loops -fexpensive-optimizations 
#CFLAGS=  -pg -ggdb -Wall -lm   -Wno-missing-braces -static 

all: probSAT probSAT_H probSAT_CH

probSAT:	probSAT.c
			$(CC) $(CFLAGS)  probSAT.c -lm -o probSAT
probSAT_H:	probSAT_H.c
			$(CC) $(CFLAGS)  probSAT_H.c -lm -o probSAT_H
probSAT_CH:	probSAT_CH.c
			$(CC) $(CFLAGS)  probSAT_CH.c -lm -o probSAT_CH
clean:	
		rm -f probSAT

