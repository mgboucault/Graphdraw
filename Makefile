EXEC = graphdraw
COMPILO = ocamlfind ocamlc
LIBS = -package graphicspdf,graphics -linkpkg


$(EXEC) : graphdrawpdf.cmo graphlayout.cmo graphdraw.cmo main.cmo
	$(COMPILO) $(LIBS) -o graphdraw graphdrawpdf.cmo graphlayout.cmo graphdraw.cmo main.cmo

graphdrawpdf.cmo : graphdrawpdf.mli graphdrawpdf.ml
	$(COMPILO) $(LIBS) -c graphdrawpdf.mli
	$(COMPILO) $(LIBS) -c graphdrawpdf.ml
	
graphlayout.cmo : graphlayout.mli graphlayout.ml
	$(COMPILO) $(LIBS) -c graphlayout.mli
	$(COMPILO) $(LIBS) -c graphlayout.ml

graphdraw.cmo : graphdraw.mli graphdraw.ml
	$(COMPILO) $(LIBS) -c graphdraw.mli
	$(COMPILO) $(LIBS) -c graphdraw.ml

main.cmo : main.ml
	$(COMPILO) $(LIBS) -c main.ml
	
clean :
	rm -rf $(EXEC) *.cmi *.cmo
