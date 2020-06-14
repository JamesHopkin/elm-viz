# elm-viz
Elm Graphviz experiments

To run web server, run node from dist folder and execute

	s = require('../server/server.js').runVizServer(8000)

To compile Elm source, run 

	elm make src/Viz.elm --output ../dist/js/elm-viz.js
