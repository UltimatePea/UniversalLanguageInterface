.PHONY: test
test:
	docker build -t testuliimage -f ./test/test_environment/Dockerfile .
	docker run testuliimage
ptest:
	python3 python3/test_python.py
	@echo Test Successful


