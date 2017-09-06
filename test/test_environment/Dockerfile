FROM ubuntu


RUN apt-get update
RUN apt-get install -y \
	build-essential \
	python3 \
	python3-pip \
	haskell-platform \
	curl \
	nodejs
RUN curl -sSL https://get.haskellstack.org/ | sh
ENV PATH="/root/.local/bin:${PATH}"
RUN stack setup
RUN cabal update

WORKDIR /testenv
COPY . /testenv

CMD ["make", "script_test"]
