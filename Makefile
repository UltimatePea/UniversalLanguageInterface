.PHONY: test
test:
	docker build -t testuliimage -f ./test/test_environment/Dockerfile .
	docker run testuliimage

script_test:
	cd test/integrationTestULI; stack install
	integrationTestULI-exe
	@echo Test Successful


