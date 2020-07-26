cd elm
elm make src/Viz.elm --optimize --output ../dist/js/elm-viz.js --report=json 2> >(node ../../../code/parse-elm-error.js)
