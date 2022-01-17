SHELL := /bin/bash 

docker_setup:
	docker volume create katana
	docker-compose build katana --parallel

docker_up: 
	docker-compose up -d
	@echo 'Running docker, port 3000 exposed'

docker_down: 
	docker_compose down

docker_logs: 
	docker-compose logs -f $(ARG)

