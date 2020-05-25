FROM node

COPY . .

RUN npm install -g elm --unsafe-perm=true 
RUN npm install -g elm-test --unsafe-perm=true

RUN elm make ./src/Main.elm --output=index.js
RUN elm-test --fuzz 40
