test: dist/build/copilot-test/copilot-test
	./dist/build/copilot-test/copilot-test

dist/build/copilot-test/copilot-test: Main.hs Language.hs API.hs
	cabal build > /dev/null

clean:
	@cabal clean
