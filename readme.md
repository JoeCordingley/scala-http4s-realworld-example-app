# ![RealWorld Example App](logo.png)

[![Build Status](https://travis-ci.org/alex-k1/scala-http4s-realworld-example-app.svg?branch=master)](https://travis-ci.org/alex-k1/scala-http4s-realworld-example-app)

> ### Scala + http4s codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.


This codebase was created to demonstrate a fully fledged fullstack application built with **Scala + http4s** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **Scala + http4s** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.


# How it works

## The application stack

- http4s
- doobie
- cats

# Requirements

- jdk 11
- sbt
- docker-compose

# Getting started

## Run a local development 

### Start a local database

```
docker-compose -f docker-compose-dev.yml up -d
```
    
### Start the application server

```
sbt run
```

The server will start on `localhost:8080`
    
## Run with Docker

### Build an image

```
sbt docker:publishLocal 
```
    
### Start containers

```
docker-compose up -d
```

## Run tests

```
sbt test
```
