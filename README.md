# viz_client

[![Build Status](https://travis-ci.org/lifemapper/viz_client.svg?branch=master)](https://travis-ci.org/lifemapper/viz_client)

## Build dependencies

1. [Node.js](http://nodejs.org) - I recommend installing from 
   [NodeSource](https://github.com/nodesource/distributions). 
   At https://nodejs.org/en/, says most users should use 10.16.0 
   (current ver is 12.4.0).  Installation instructions at 
   https://github.com/nodejs/help/wiki/Installation
1. [Elm](http://elm-lang.org/) - version 0.18 for Swagger Elm compatibility
   `npm install -g elm@0.18.0`
1. [Swagger Elm](https://github.com/ahultgren/swagger-elm) - 
   only supports elm version 0.18. 
   `npm -g install ahultgren/swagger-elm#v0.3.0`
1. [Elm Format](https://github.com/avh4/elm-format) - 
   `npm install -g elm-format@0.5.2`
1. Standard Linux build tools: *gnumake*, *patch*, *tar*, *bash*

## Building the debug target

The debug target includes the Elm time traveling debugger in the
generated app.

```
elm package install --yes
make clean && make debug
```

## Building the default app

```
elm package install --yes
make clean && make
```

## Development

The build can be accessed locally on the development machine by
loading the `boom/index.html` file in a browser. By default, the app
expects to find Lifemapper webservices on the same host. This can be
overridden by add the file `boom/boomFlagsOverride.js` with the
appropriate values. For example:

```
var boomFlags = {
    apiRoot: "http://yeti.lifemapper.org/api/v2/",
    minimumOccurrencePoints: 30,
    completedPollingSeconds: 5
};
```

Note: The `boomFlagsOverride.js` is excluded from the generated
tarball.


## Deployment

Unpack the generated `boom.tar.gz` in the public html directory of the
LM web server.
