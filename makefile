ifndef VERBOSE
.SILENT:
endif
INCLUDE = archmage.ml
TARGET  = archmage

.PHONY: clean
all: compile run clean
build: compile clean

compile: $(INCLUDE)
	ocamlfind ocamlopt -o $(TARGET) $(INCLUDE)

run: compile
	./$(TARGET)

clean:
	rm -f *.cmi *.cmo *.cmx *.o

