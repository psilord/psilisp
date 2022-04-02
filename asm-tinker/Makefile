CFLAGS = -Wall

all: de

de: test_fun_inst_0.o main.o
	gcc $(CFLAGS) -o de test_fun_inst_0.o main.o

test_fun_inst_0.o: test_fun_inst_0.S
	gcc $(CFLAGS) -c test_fun_inst_0.S -o test_fun_inst_0.o

main.o: main.c
	gcc $(CFLAGS) -c main.c -o main.o

clean:
	rm -rf de *.o
