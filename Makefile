all: src/*.elm
	# TODO build to a better spot
	elm make src/Main.elm --output src/main.js
