all:
	mkdir -p ./target
	elm-make src/Main.elm --output=target/index.html
	cp -r src/static target/
	cd target && python3 -m http.server
