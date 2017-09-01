.PHONY: test
test:
	docker build -t testuliimage -f ./test/test_environment/Dockerfile .
	docker run testuliimage

build_image:
	docker build -t testuliimage -f ./test/test_environment/Dockerfile .

verify_image:
	docker run testuliimage

script_test:
	python3 python3/test_python.py
	@echo Test Successful


