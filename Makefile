CFLAGS = -Wall -fomit-frame-pointer

all: de

de: explore.o scheme_entry.o main.o
	gcc $(CFLAGS) -o de explore.o scheme_entry.o main.o

explore.o: explore.S
	gcc $(CFLAGS) -c explore.S -o explore.o

scheme_entry.o: scheme_entry.S
	gcc $(CFLAGS) -c scheme_entry.S -o scheme_entry.o

main.o: main.c
	gcc $(CFLAGS) -c main.c -o main.o

clean:
	rm -rf de *.o
