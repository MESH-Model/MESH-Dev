CC = gfortran
FILE = compare

all: $(FILE) 
	call compare.bat

$(FILE): $(FILE).o
	$(CC) -o $(FILE) $(FILE).o

$(FILE).o: $(FILE).f90
	$(CC) -c $(FILE).f90 

clean:
	rm compare compare.o