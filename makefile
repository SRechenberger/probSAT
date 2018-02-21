CC=gcc
CFLAGS= -Wall -Wextra -static -O3 -funroll-loops -fexpensive-optimizations 
#CFLAGS=  -pg -ggdb -Wall -lm   -Wno-missing-braces -static 

all: probSAT probSAT_S

probSAT:	probSAT.c
			$(CC) $(CFLAGS)  probSAT.c -lm -o probSAT
probSAT_S:	probSAT_S.c
			$(CC) $(CFLAGS)  probSAT_S.c -lm -o probSAT_S
clean:	
		rm -f probSAT

