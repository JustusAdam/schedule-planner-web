# schedule-planner-web

Sources for the frontend code for the schedule-planner tool.

## Install

This project builds on

- [Schedule Planner](//github.com/JustusAdam/schedule-planner) (the backend server)
- [Foundation](http://foundation.zurb.com), though it is usable without. (styling only)
- [Elm](//elm-lang.org)

Execute `bower install && bundler install && compass compile` in the directory.

Install the elm platform.

If you want to use a local instance of the [backend server](//github.com/JustusAdam/schedule-planner) set the `receiver` constant at the top of src/InputFields.elm to where the server is running. Usually "localhost:PORT" with PORT being the port you chose for the server.

Compile the project using `elm-make src/Main.elm`.

Open index.html.
