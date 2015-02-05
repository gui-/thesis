report.pdf: reference.bib report.tex sections/*.tex 
	pdflatex -shell-escape report
	bibtex report
	pdflatex -shell-escape report
	pdflatex -shell-escape report
	clear
	ls

partial:
	bibtex report
	pdflatex -shell-escape report

clean:
	rm -f *.lot *.lof *.lol *.toc *.log *.out *.aux *.blg *.bbl report.pdf sections/*.aux 

rebuild: clean report.pdf
