# robust-plots
Web app to plot robust enclosures of function graphs and fractals

# install
```sh
npm run install
```
This will install the project and all its dependencies.

# build
```sh
npm run build
```
This will create a web server version of the website with URL relative paths. This means that in the `dist/index.html` the path to the `app.js` file starts with a `/`. Remove this to run the website as a simple HTML file. An alternate route is to open the `dist` directory as a local webserver.

# run tests
```sh
npm run test
```
This will run all of the unit tests for the project. Note that any functionality that is dependent on external JS libraries will not work in a unit test environment as these libraries are not resolved in the PS test environment.