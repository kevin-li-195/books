.PHONY: frontend-deploy frontend

DEPLOYDIR=/srv/http/renewal

frontend-deploy: frontend
	rsync --delete deploy/ $(DEPLOYDIR)

frontend:
	rm -r deploy/*
	cp frontend/* deploy
