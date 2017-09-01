.PHONY: test
test:
	docker build -t testuliimage ./test/test_environment/
	docker run testuliimage
ptest:
	python3 python3/test_python.py
	@echo Test Successful
	
