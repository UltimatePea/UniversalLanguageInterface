.PHONY: test
test:
	docker build -t testuliimage -f ./test/test_environment/Dockerfile .
	docker run testuliimage

script_test:
	cd test/integrationTestULI; stack install
	integrationTestULI-exe
	@echo Test Successful

travis_test:
	export PATH="/root/.local/bin:${PATH}"
	curl -sSL https://get.haskellstack.org/ | sh
	stack setup
	cabal update
	make script_test
