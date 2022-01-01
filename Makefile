.PHONY: all clean

PROGRAM = build\lib\libfutils.a
OBJS = \
	build/vector.o \
	build/str.o

TEST_OBJS = \
	

MODS = \
	m_vector.mod \
	m_str.mod

LIBRARY_TO_BUILD = build/lib
INCLUDE = build/include

FC = ftn  # ifort
FLAGS = -module $(INCLUDE) -I$(INCLUDE) 

# FC = gfortran
# FLAGS = -J$(INCLUDE) -I$(INCLUDE) 

AR = ar rc

RM = rm -f
MKDIR = mkdir -p

ifeq ($(OS),Windows_NT)
	RM = powershell del
	MKDIR = mkdir
endif

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(AR) $(PROGRAM) $^

build/vector.o: src\vector.F90 
	$(FC) $(FLAGS) -c $< -o build/vector.o

build/str.o: src\str.F90 
	$(FC) $(FLAGS) -c $< -o build/str.o

test:  build/test

builddir:
	-$(MKDIR) build
	-$(MKDIR) build\test
	-$(MKDIR) build\test\lib
	-$(MKDIR) $(LIBRARY_TO_BUILD)
	-$(MKDIR) $(INCLUDE)

clean:
	-$(RM) $(PROGRAM)
	-$(RM) $(INCLUDE)/*.mod
	-$(RM) $(LIBRARY)/*.a
	-$(RM) build/*.o
	-$(RM) build/test/*.exe
	-$(RM) build/test/*.o
	-$(RM) build/test/lib/*.a
