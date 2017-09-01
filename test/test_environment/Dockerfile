FROM ubuntu

WORKDIR /testenv

COPY . /testenv

RUN apt-get update
RUN apt-get install -y build-essential
RUN apt-get install -y python3
RUN apt-get install -y python3-pip

CMD ["ls"]

CMD ["make", "ptest"]
