.PHONY: frontend-deploy frontend backend-deploy backend clean update gen-client clean-deploy bg bg-deploy

$(shell mkdir -p deploy)

DEPLOYDIR=/srv/http/renewal

deploy: frontend-deploy backend-deploy bg-deploy

clean-deploy:
	-rm -rf deploy/*

frontend-deploy: frontend
	rsync -r --delete deploy/ $(DEPLOYDIR)

frontend: clean-deploy deploy/index.css deploy/index.js deploy/index.html

deploy/index.css: frontend/index.css
	cp $< $@

deploy/index.html: frontend/index.html
	cp $< $@

deploy/index.js: frontend/index.js dist/client.js
	cat $^ > $@

dist/client.js: gen-client
	cabal run gen-client $@

bg-deploy: bg
	cp dist/build/renew/renew bin/

bg: lib
	cabal build exe:renew

backend-deploy: backend
	mkdir -p bin
	sudo systemctl stop books-renewal.service
	cp dist/build/books/books bin/
	sudo systemctl start books-renewal.service
	sudo systemctl status books-renewal.service

backend: lib
	cabal build exe:books

lib: update
	cabal build lib:books

gen-client: lib
	cabal build gen-client

update:
	cabal update
	cabal install --only-dependencies

clean:
	cabal clean

schema:
	psql -f sql/schema.sql
