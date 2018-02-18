CC=gcc
CFLAGS= -Wall -Wextra -static -O3 -funroll-loops -fexpensive-optimizations 
#CFLAGS=  -pg -ggdb -Wall -lm   -Wno-missing-braces -static 

all: probSAT probSAT_H probSAT_PH probSAT_L

probSAT:	probSAT.c
			$(CC) $(CFLAGS)  probSAT.c -lm -o probSAT
probSAT_H:	probSAT_H.c
			$(CC) $(CFLAGS)  probSAT_H.c -lm -o probSAT_H
probSAT_L:	probSAT_L.c
			$(CC) $(CFLAGS)  probSAT_L.c -lm -o probSAT_L
probSAT_PH:	probSAT_PH.c
			$(CC) $(CFLAGS)  probSAT_PH.c -lm -o probSAT_PH
clean:	
		rm -f probSAT

