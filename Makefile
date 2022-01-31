LIB_SRCS := ast.ml optmanip.ml env.ml astmanip.ml addrs.ml contextual.ml util.ml vtable.ml compil.ml
BUILD_DIR := _build

LIB_CMXS := $(LIB_SRCS:.ml=.cmx)
LIB_OBJS := $(LIB_SRCS:.ml=.o)
LIB_FILES := $(LIB_SRCS:%=lib/%)

all: $(BUILD_DIR)/compilc.exe

$(BUILD_DIR)/compilc.exe: $(BUILD_DIR)/parser.mli $(BUILD_DIR)/parserMessages.ml $(BUILD_DIR)/lexer.ml bin/main.ml $(LIB_FILES) 
	cp $(LIB_FILES) $(BUILD_DIR)
	cp bin/main.ml $(BUILD_DIR)
	cd $(BUILD_DIR) && ocamlfind ocamlopt -package menhirLib -c ast.ml parser.mli
	cd $(BUILD_DIR) && ocamlfind ocamlopt -package menhirLib -for-pack Libcompil -c $(LIB_SRCS) parser.ml parserMessages.ml lexer.ml
	cd $(BUILD_DIR) && ocamlfind ocamlopt -pack -o libcompil.cmx $(LIB_CMXS) parser.cmx parserMessages.cmx lexer.cmx
	cd $(BUILD_DIR) && ocamlfind ocamlopt -package menhirLib -a -o libcompil.cmxa libcompil.cmx
	cd $(BUILD_DIR) && ocamlfind ocamlopt -package menhirLib -c main.ml
	cd $(BUILD_DIR) && ocamlfind ocamlopt -package menhirLib -o compilc.exe menhirLib.cmxa libcompil.cmxa main.cmx

$(BUILD_DIR)/lexer.ml: lib/lexer.mll $(BUILD_DIR)/parser.mli lib/ast.ml
	ocamllex lib/lexer.mll -o $(BUILD_DIR)/lexer.ml

$(BUILD_DIR)/parser.mli: lib/parser.mly lib/ast.ml
	cd $(BUILD_DIR) && menhir --dump --explain --table ../lib/parser.mly -b parser

$(BUILD_DIR)/parserMessages.ml: lib/parser.messages lib/parser.mly
	menhir lib/parser.mly --compile-errors lib/parser.messages > $(BUILD_DIR)/parserMessages.ml
	
clean:
	rm -rf $(BUILD_DIR)
	mkdir $(BUILD_DIR)

# -------------------------------
# Docker part

ARG := $(word 2, $(MAKECMDGOALS))

docker_setup:
	docker volume create katana
	docker-compose build katana --parallel

docker_up: 
	docker-compose up -d
	@echo 'Running docker Image, Port 3000 Exposed'

docker_down: 
	docker-compose down

docker_logs: 
	docker-compose logs -f $(ARG)

docker_run:
	docker run --rm -it turbo_katana_katana:latest
