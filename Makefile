.PHONY: frontend-deploy frontend backend-deploy backend clean update gen-client clean-deploy bg bg-deploy secrets-deploy

$(shell mkdir -p deploy)

PRODUCTION?=no

DEPLOYDIR=/srv/http/renewal

deploy: secrets-deploy frontend-deploy backend-deploy bg-deploy

secrets-deploy:
ifeq ($(PRODUCTION),yes)
	ln -sf .production-secrets .secrets
else
	ln -sf .test-secrets .secrets

clean-deploy:
	-rm -rf deploy/*

frontend-deploy: frontend
	rsync -r --delete deploy/ $(DEPLOYDIR)

frontend: clean-deploy deploy/index.css deploy/index.js deploy/index.html

deploy/index.css: frontend/index.css
	cp $< $@

deploy/index.html: frontend/index.html
	cp $< $@

PUBKEY=$(shell cat .secrets | grep BOOK_STRIPE_KEY_PUB | cut -d= -f2)

deploy/index.js: frontend/index.js dist/client.js
	cat $^ | sed "s/STRIPE_CHECKOUT_SECRET_KEY_PLREASE_REPLACE/$(PUBKEY)" > $@

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
