SRC_DIR=$(shell pwd)
DOCKER_SRC_DIR="/home/code"

.PHONY : clean all haskell

# do nothing by default
all:

# start ghci with docker and load as a volumen the SRC_DIR
# in DOCKER_SRC_DIR.
haskell:
	docker run -it -v "${SRC_DIR}:${DOCKER_SRC_DIR}" haskell
