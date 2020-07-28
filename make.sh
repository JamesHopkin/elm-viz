cd elm
elm make src/Viz.elm --output ../dist/js/elm-viz.js --report=json 2> >(node ../../../code/parse-elm-error.js)

if [ $? -eq 0 ]; then
	echo "success! minifying ..."
	uglifyjs ../dist/js/elm-viz.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' --mangle --output ../dist/js/elm-viz.min.js
fi
