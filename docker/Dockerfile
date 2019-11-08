# Use atlassian default image as a start
FROM atlassian/default-image:2
#FROM node

# Set the working directory to /app
#WORKDIR /app

# Copy the current directory contents into the container at /app
#ADD . /app

# Install any needed packages specified in requirements.txt
#RUN pip install --trusted-host pypi.python.org -r requirements.txt

RUN npm install -g elm --unsafe-perm=true 
RUN npm install -g elm-test --unsafe-perm=true
# Make port 80 available to the world outside this container
#EXPOSE 80

# Define environment variable
#ENV NAME World

# Run app.py when the container launches
#CMD ["python", "app.py"]