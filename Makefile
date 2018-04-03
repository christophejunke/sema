c.out: sem
	./sem > $@

test: sem
	./sem

sem: sem.c
	gcc -pthread $< -o $@

