#########################
# Compiler and standard #
#########################

CMP = ifort
STD = -std08

#########
# Flags #
#########

CMPFLAGS = -fopenmp -diag-disable=7025,6477
OPTFLAGS = -O2 -xAVX
#OPTFLAGS = -O2 -xCORE-AVX2
#OPTFLAGS = -O2 -xCORE-AVX512 -qopt-zmm-usage=high
REPFLAGS = -g -qopt-report=5 -qopt-report-filter="gsimulation.f90,313-417"

FLAGS = $(STD) $(CMPFLAGS) $(OPTFLAGS) $(REPFLAGS)

#########
# Files #
#########

SRC = mt19937_64.f90 cputime.f90 types.f90 particle.f90 gsimulation.f90 main.f90
OBJ = $(SRC:.f90=.o)

EXE = ./nbody.x
ADV = ./adv-v7-openmp

#################
# GENERAL RULES #
#################

all: clean $(EXE)

run: $(EXE)
	$(info )
	$(info Running the CPU executable: )
	$(EXE)

$(EXE): $(OBJ)
	$(info )
	$(info Linking the CPU executable:)
	$(CMP) $(FLAGS) -o $(EXE) $(OBJ)

%.o: %.f90
	$(info )
	$(info Compiling $< for CPU: )
	$(CMP) $(FLAGS) -o $@ -c $<

asm: gsimulation.f90
	$(info )
	$(info Generating assembly code: )
	$(CMP) $(FLAGS) -S $^

clean:
	$(info )
	$(info Removing temporary files: )
	rm -f $(EXE) $(OBJ) *.mod *.s *.optrpt

#################
# ADVISOR RULES #
#################

survey:
	advixe-cl -collect survey -project-dir $(ADV) -- $(EXE)

roofline:
	advixe-cl -collect survey -project-dir $(ADV) -- $(EXE)
	advixe-cl -collect tripcounts -flop -project-dir $(ADV) -- $(EXE)

map:
	advixe-cl -collect map -mark-up-list=1 -project-dir $(ADV) -- $(EXE)

dependencies:
	advixe-cl -collect dependencies -mark-up-list=1 -project-dir $(ADV) -- $(EXE) 1000 1

open-gui:
	advixe-gui $(ADV)/$(ADV).advixeproj >/dev/null 2>&1 &

clean-results:
	rm -rf $(ADV)
