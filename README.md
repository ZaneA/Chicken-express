# Chicken-express

A FastCGI web framework in Chicken Scheme, based on the [API of Express](http://expressjs.com/api.html) for [Node.js](http://nodejs.org).

Plenty is missing right now and some parts will likely not make it in at all, but I hope to end up with a solid framework for building future Chicken web apps.

## Requirements

A few modules are required which can be installed with `chicken-install`, as well as [Chicken itself](http://www.call-cc.org). Notable modules are:

- protobj
- uri-common
- matchable
- fastcgi
- base64 (for `middleware-favicon`)
- colorize (for the example)

## Usage

A small example is included as `example.scm`. You can run this with:

    CHICKEN_ENV=development ./example.scm

This will start a simple FastCGI server at port 3000 by default. The port can be changed by doing:

    CHICKEN_ENV=development ./example.scm --port <port>

Alternatively the framework is designed to be compatible with shared socket managers such as [Einhorn](https://github.com/stripe/einhorn) by passing a file descriptor instead of port:

    CHICKEN_ENV=development einhorn -c chicken-express ./example.scm --fd srv:127.0.0.1:3000,so_reuseaddr

This allows you to add/remove workers at runtime. I am also planning on [Circus](http://circus.readthedocs.org) support, but it is not currently working due to some file descriptor issues.

## Hello World

The hello world should feel familiar to anyone that has used Express:

    #!/usr/bin/csi -script

    (load "chicken-express.scm")
    (import chicken-express)

    (load "middleware/logger.scm")
    (import middleware-logger)

    (define app (chicken-express))

    {@app.use (middleware-logger)}

    {@app.get "/" (lambda (self request response next)
                     {@response.send "Hello, world!"})}

    {@app.listen 3000}


## HACKING

This module (`chicken-express.scm`) is using [protobj](http://wiki.call-cc.org/eggref/4/protobj) for its objects, and so the API currently uses its macros as well.

Things starting with "%" (variables, methods, etc) are internal to the module. Everything else is in theory a part of the public API exposed by the module.

To understand the code, start at the `(chicken-express)` method, followed by `(! <app> listen)` further down, and go from there.
