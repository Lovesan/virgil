all: manual

manual: virgil-manual.texinfo style.css
	sh gendocs.sh -o manual --html "--css-include=style.css" virgil-manual "Virgil User Manual"

clean:
	find . \( -name "*.temp" -o -name "*.info" -o -name "*.aux" -o -name "*.cp" -o -name "*.fn" -o -name "*.fns" -o -name "*.ky" -o -name "*.log" -o -name "*.pg" -o -name "*.toc" -o -name "*.tp" -o -name "*.vr" -o -name "*.dvi" -o -name "*.cps" -o -name "*.vrs" \) -exec rm {} \;
	rm -rf manual
