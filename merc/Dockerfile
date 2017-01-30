FROM node:7.4

RUN npm install http-server -g

WORKDIR /project
COPY . /project
RUN npm install
RUN npm run build

EXPOSE 8081
CMD http-server -p 8081 dist
