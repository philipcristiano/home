PROJECT = home

PKG_REVISION ?= $(shell git describe --tags)
PKG_VERSION	?= $(shell git describe --tags | tr - .)
ELEVELDB_VERSION = "1.1.0"
export ELEVELDB_VERSION

SHELL_OPTS = -eval "application:ensure_all_started(home)"


DEPS = leveltsdb cowboy jsx
dep_leveltsdb = git https://github.com/philipcristiano/leveltsdb.git 0.1.3
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.0
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.1.1

.PHONY: release

release: clean app
	./relx release

package: release
	fpm -s dir -t deb -n iot-home -v 0.1.0 _rel/home=/opt/ rel/init=/etc/init.d/iot-home

include erlang.mk
