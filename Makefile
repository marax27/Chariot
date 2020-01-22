file-list = src/server.erl src/central.erl src/domain.erl src/firetrucks.erl  src/main.erl
compile:
		erlc -o beam ${file-list}