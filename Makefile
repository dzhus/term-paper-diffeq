export ROOT := $(abspath .)

export SOURCEDIR := ${ROOT}/source
export DOCDIR := ${ROOT}/doc

export INITIAL_DATA_FILE := ${SOURCEDIR}/statement.scm

.PHONY: doc clean

doc:
	${MAKE} -C ${DOCDIR} doc

clean:
	@rm -frv `hg status --unknown --no-status`