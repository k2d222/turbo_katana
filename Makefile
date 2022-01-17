SHELL := /bin/bash 
ARG := $(word 2, $(MAKECMDGOALS))

docker_setup:
	docker volume create katana
	docker-compose build katana --parallel

docker_up: 
	docker-compose up -d
	@echo 'Running docker Image, Port 3000 Exposed'

docker_down: 
	docker-compose down

docker_logs: 
	docker-compose logs -f $(ARG)

