.PHONY: frontend-deploy frontend

$(shell mkdir -p deploy)

DEPLOYDIR=/srv/http/renewal

frontend-deploy: frontend
	rsync -r --delete deploy/ $(DEPLOYDIR)

frontend:
	-rm -r deploy/*
	cp frontend/index.html frontend/index.js deploy
