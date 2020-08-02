# Dev Instructions
This page contains the instructions for developers to install and use this application.

## Install project
```sh
npm run install
```
This will install the project and all its dependencies.

## Build project
```sh
npm run build
```
This will create a web server version of the website with URL relative paths. This means that in the `dist/index.html` the path to the `app.js` file starts with a `/`. Remove this to run the website as a simple HTML file. An alternate route is to open the `dist` directory as a local webserver.

It should also be mentioned that the PS can be build alone using
```sh
npm run build-purescript
```

## Run project
```sh
npm start
```
This will start the web page as a server that can be accessed via `http://localhost:1234/`

## Run tests
```sh
npm run test
```
This will run all of the unit tests for the project. Note that any functionality that is dependent on external JS libraries will not work in a unit test environment as these libraries are not resolved in the PS test environment.

## Adding external JS dependencies
Adding external JS libraries as dependencies requires a different pipeline than PS dependencies. For library `x` a `require(x);` must be must be added to `app.js` and the version of the JS library must be added via the `package.json` file. The JS library `decimal.js` is an example of this.