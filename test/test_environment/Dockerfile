# Use an official Python runtime as a parent image
FROM ubuntu

# Set the working directory to /app
WORKDIR /testenv

# Copy the current directory contents into the container at /app
COPY . /testenv

# Install any needed packages specified in requirements.txt
RUN apt-get install make

# Run app.py when the container launches
CMD ["make", "ptest"]
