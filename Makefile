all:
	dune build ./src/main.exe @install

run-client:
	rm -rf web/static/js
	mkdir -p web/static/js
	cp _build/default/web/client/main.js web/static/js/main.js