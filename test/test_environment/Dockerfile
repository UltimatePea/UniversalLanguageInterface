FROM ubuntu

WORKDIR /testenv

COPY . /testenv

RUN apt-get update
RUN apt-get install -y build-essential
RUN apt-get install -y python3
RUN apt-get install -y python3-pip
RUN apt-get install -y haskell-platform
RUN curl -sSL https://get.haskellstack.org/ | sh

CMD ["ls"]

CMD ["make", "script_test"]
