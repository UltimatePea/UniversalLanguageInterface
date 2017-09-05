FROM ubuntu

WORKDIR /testenv

COPY . /testenv

RUN apt-get update
RUN apt-get install -y build-essential
RUN apt-get install -y python3
RUN apt-get install -y python3-pip
RUN apt-get install -y haskell-platform
RUN apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh
ENV PATH="/root/.local/bin:${PATH}"
RUN stack setup

CMD ["ls"]

CMD ["make", "script_test"]
