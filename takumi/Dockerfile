FROM node:8.1.2-alpine

ENV NODE_ENV production

WORKDIR /project
COPY package.json /project
RUN npm install

# Make sure .dockerignore includes the files you want.
COPY . /project
EXPOSE 8080
CMD npm start
