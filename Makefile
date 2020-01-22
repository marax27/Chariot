file-list = src/server.erl src/central.erl src/domain.erl src/firetrucks.erl  src/main.erl src/jsone/jsone.erl src/jsone/jsone_encode.erl src/jsone/jsone_decode.erl
compile:
		erlc -o beam ${file-list}