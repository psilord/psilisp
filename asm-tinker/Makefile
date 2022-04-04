CFLAGS = -Wall

all: de

de: funinst.o main.o
	gcc $(CFLAGS) -o de funinst.o main.o

funinst.o: funinst.S
	gcc $(CFLAGS) -c funinst.S -o funinst.o

main.o: main.c
	gcc $(CFLAGS) -c main.c -o main.o

clean:
	rm -rf de *.o
