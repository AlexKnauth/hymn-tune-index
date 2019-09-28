
publish: hymn-tune-index.js hymn-tune-index.html
	cp hymn-tune-index.html hymn-tunes.html
	scp ./hymn-tune-index.{html,js,json} hymn-tunes.html ccis:~/.www

hymn-tune-index.js: hymn-tune-index-script.js hymn-tune-index.json
	cat hymn-tune-index-script.js > hymn-tune-index.js
	echo "\n\ntree = " >> hymn-tune-index.js
	cat hymn-tune-index.json >> hymn-tune-index.js
	echo ";\n" >> hymn-tune-index.js

hymn-tune-index.json: hymn-tune-index.rkt
	racket hymn-tune-index.rkt > hymn-tune-index.json

.PHONY: publish
