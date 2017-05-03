.PHONY: frontend-deploy frontend backend-deploy backend

DEPLOYDIR=/srv/http/renewal

deploy: frontend-deploy backend-deploy

frontend-deploy: frontend
	rsync --delete deploy/ $(DEPLOYDIR)

frontend:
	rm -r deploy/*
	cp frontend/* deploy

backend-deploy: backend
	mkdir -p bin
	cp dist/build/books/books bin/
	sudo systemctl restart books-renewal.service

backend:
	cabal clean
	cabal install --only-dependencies
	cabal build
