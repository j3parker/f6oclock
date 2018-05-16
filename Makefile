all: src/*.elm
	# TODO build to a better spot
	elm make src/Main.elm --output src/main.js
	# npm install -g uglify-js
	uglifyjs -c -m -- src/main.js > src/main.min.js
