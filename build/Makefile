CC = g++
CXXFLAGS = -O3

VM_CXX = $(wildcard ../vm/*.cpp )
VM_OBJS = $(subst .cpp,.o,$(subst ../vm/,./,$(VM_CXX)))


all: ../comp ../virtual_machine
	@echo "Finished build"

compiler: ../comp
	$(CC) ../compiler/compiler.cpp $(CXXFLAGS) -o ../comp
	@echo "Finished compiled build"

virtual_machine: ../virtual_machine
	$(CC)  $(VM_OBJS) $(CXXFLAGS) -o ../virtual_machine
	@echo "Finished virtual_machine build"

../virtual_machine: $(VM_OBJS)

../comp: ../compiler/compiler.cpp
	$(CC) ../compiler/compiler.cpp $(CXXFLAGS) -o ../comp

$(VM_OBJS): $(VM_CXX)
	$(CC) $(VM_CXX) -c $(CXXFLAGS) -I../vm/ $(-o $(VM_OBJS))