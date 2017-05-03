.PHONY: frontend-deploy frontend backend-deploy backend clean

DEPLOYDIR=/srv/http/renewal

deploy: frontend-deploy backend-deploy

frontend-deploy: frontend
	rsync -r --delete deploy/ $(DEPLOYDIR)

frontend:
	rm -r deploy/*
	cp frontend/* deploy

backend-deploy: backend
	mkdir -p bin
	sudo systemctl stop books-renewal.service
	cp dist/build/books/books bin/
	sudo systemctl start books-renewal.service
	sudo systemctl status books-renewal.service

backend:
	cabal update
	cabal install --only-dependencies
	cabal build

clean:
	cabal clean

schema:
	psql -f sql/schema.sql
