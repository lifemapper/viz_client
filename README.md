# rutabaga

[![Build Status](https://travis-ci.org/lifemapper/rutabaga.svg?branch=master)](https://travis-ci.org/lifemapper/rutabaga)

## Build dependencies

1. [Node.js](http://nodejs.org) - I recommend installing from [NodeSource](https://github.com/nodesource/distributions).
1. [Elm](http://elm-lang.org/) - `npm install -g elm@0.18.0`
1. [Swagger Elm](https://github.com/ahultgren/swagger-elm) - `npm -g install ahultgren/swagger-elm#v0.3.0`
1. [Elm Format](https://github.com/avh4/elm-format) - `npm install -g elm-format@0.5.2`
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
loading the `sdm/index.html` file in a browser. By default, the app
expects to find Lifemapper webservices on the same host. This can be
overridden by add the file `sdm/sdmFlagsOverride.js` with the
appropriate values. For example:

```
var sdmFlags = {
    apiRoot: "http://yeti.lifemapper.org/api/v2/",
    minimumOccurrencePoints: 30,
    completedPollingSeconds: 5
};
```

Note: The `sdmFlagsOverride.js` is excluded from the generated
tarball.


## Deployment

Unpack the generated `sdm.tar.gz` in the public html directory of the
LM web server.
