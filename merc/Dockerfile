FROM node:7.4

RUN npm install http-server -g

WORKDIR /project
COPY package.json /project
RUN npm install

ARG API_URL
COPY . /project
RUN npm run build

EXPOSE 8081
CMD http-server -p 8081 dist
