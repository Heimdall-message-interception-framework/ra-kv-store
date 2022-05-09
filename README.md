# Raft-based Key/Value Store

This is a modified version of [ra-kv-store](https://github.com/rabbitmq/ra-kv-store) by RabbitMQ.
It is built on top of a [modified Raft implementation](https://gitlab.mpi-sws.org/rep-sys-group/ra) which uses our [message interception layer](https://gitlab.mpi-sws.org/fstutz/sched_msg_interception_erlang).

## Getting Started 

This project makes use of git submodules to handle the git repositories of the various projects.
In order to clone together with all of its dependencies, do the following:

```
$ git clone --recurse-submodules git-rts@gitlab.mpi-sws.org:rep-sys-group/ra-kv-store.git
```

**For some reason, you have to once build the project with make before any of the rebar3 commands below work:**

```
$ make run
```

## Building

We use [rebar3](https://rebar3.readme.io/) to run tests and build the project.

```
$ rebar3 compile
```

## Typechecking

```
$ rebar3 dialyzer
```

## Testcases

```
$ rebar3 dialyzer
```

---

**⚠ WARNING ⚠: The following parts of this readme come from the original [ra-kv-store repository](https://github.com/rabbitmq/ra-kv-store) and are not guaranteed to work with this codebase.**
# Usage

Start the HTTP API:

```
$ make run
```

Use [HTTPie](https://httpie.org/) to play with the KV store. Retrieving a value:
```
$ http GET http://localhost:8080/1
HTTP/1.1 404 Not Found
content-length: 9
content-type: text/plain
date: Wed, 13 Jun 2018 07:12:15 GMT
server: Cowboy

undefined
```

Setting a value:
```
$ http -f PUT http://localhost:8080/1 value=1
HTTP/1.1 204 No Content
date: Wed, 13 Jun 2018 07:12:28 GMT
server: Cowboy

```

Getting the value back:
```
$ http GET http://localhost:8080/1
HTTP/1.1 200 OK
content-length: 1
content-type: text/plain
date: Wed, 13 Jun 2018 07:12:34 GMT
server: Cowboy

1

```

[Comparing-and-swapping](https://en.wikipedia.org/wiki/Compare-and-swap) a value, success case:
```
$ http -f PUT http://localhost:8080/1 value=2 expected=1
HTTP/1.1 204 No Content
date: Wed, 13 Jun 2018 07:13:02 GMT
server: Cowboy

```

[Comparing-and-swapping](https://en.wikipedia.org/wiki/Compare-and-swap) a value, failure case:
```
$ http -f PUT http://localhost:8080/1 value=2 expected=1
HTTP/1.1 409 Conflict
content-length: 1
date: Wed, 13 Jun 2018 07:13:08 GMT
server: Cowboy

2
```

# Jepsen test

See the [readme](jepsen/jepsen.rakvstore/README.md).

# License

RA KV Store is [Apache 2.0 licensed](https://www.apache.org/licenses/LICENSE-2.0.html).

_Sponsored by [Pivotal](http://pivotal.io)_
