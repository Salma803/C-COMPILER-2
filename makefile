# Compilateur OCaml
OCAMLC = ocamlc
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex

# Dossiers
IN_TESTS = ./in_tests
OUT_TESTS = ./out_tests

# Cible principale
all: cminus

# Génération du lexer et parser
lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(OCAMLYACC) parser.mly

# Compilation
%.cmi: %.mli
	$(OCAMLC) -c $

%.cmo: %.ml
	$(OCAMLC) -c $

# Dépendances
ast.cmo: ast.ml
	$(OCAMLC) -c ast.ml

semantic.cmo: semantic.ml ast.cmo
	$(OCAMLC) -c semantic.ml

parser.cmi: parser.mli ast.cmo
	$(OCAMLC) -c parser.mli

parser.cmo: parser.ml ast.cmo parser.cmi
	$(OCAMLC) -c parser.ml

lexer.cmo: lexer.ml parser.cmi
	$(OCAMLC) -c lexer.ml

main.cmo: main.ml ast.cmo semantic.cmo parser.cmi lexer.cmo
	$(OCAMLC) -c main.ml

# Lien final
cminus: ast.cmo semantic.cmo parser.cmo lexer.cmo main.cmo
	$(OCAMLC) -o cminus ast.cmo semantic.cmo parser.cmo lexer.cmo main.cmo

# Nettoyage
clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli cminus parser.output
	rm -rf $(OUT_TESTS)

# ============================
#   Tests automatiques
# ============================

TESTS_OK := $(wildcard $(IN_TESTS)/OK/*.c)
TESTS_KO := $(wildcard $(IN_TESTS)/KO/*.c)

.PHONY: test test-ok test-ko all clean

test: test-ok test-ko
	@echo ""
	@echo "✔️  Tous les tests sont terminés."
	@echo "📁 Les résultats sont dans $(OUT_TESTS)/"
	@echo ""

test-ok: cminus
	@echo ""
	@echo "========== 🟢 TESTS OK =========="
	@echo ""
	@mkdir -p $(OUT_TESTS)/OK
	@for file in $(TESTS_OK); do \
		basename=$$(basename $$file .c); \
		echo "➡️  Test: $$file"; \
		./cminus $$file > $(OUT_TESTS)/OK/$$basename.txt 2>&1; \
		status=$$?; \
		if [ $$status -eq 0 ]; then \
			echo "✔️ Accepté (correct)"; \
		else \
			echo "❌ REJET INATTENDU — voir $(OUT_TESTS)/OK/$$basename.txt"; \
		fi; \
		echo ""; \
	done

test-ko: cminus
	@echo ""
	@echo "========== 🔴 TESTS KO =========="
	@echo ""
	@mkdir -p $(OUT_TESTS)/KO
	@for file in $(TESTS_KO); do \
		basename=$$(basename $$file .c); \
		echo "➡️  Test: $$file"; \
		./cminus $$file > $(OUT_TESTS)/KO/$$basename.txt 2>&1; \
		status=$$?; \
		if [ $$status -ne 0 ]; then \
			echo "✔️ Rejeté (correct)"; \
			echo "   --- Erreur détectée ---"; \
			cat $(OUT_TESTS)/KO/$$basename.txt; \
			echo "   --- Fin de l'erreur ---"; \
		else \
			echo "❌ ACCEPTATION INATTENDUE — voir $(OUT_TESTS)/KO/$$basename.txt"; \
		fi; \
		echo ""; \
	done