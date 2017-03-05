FROM node:7.7-alpine
ARG API_URL

WORKDIR /project
COPY package.json /project
RUN npm install

COPY . /project
RUN npm run build

EXPOSE 8081
CMD node .
