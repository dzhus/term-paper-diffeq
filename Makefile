export ROOT := $(abspath .)

export SOURCEDIR := ${ROOT}/source
export SHARED_SOURCES := $(foreach file,shared.scm dispatcher.scm matrices.scm gauss.scm,${SOURCEDIR}/${file})
export DOCDIR := ${ROOT}/doc

.PHONY: doc clean

doc:
	${MAKE} -C ${DOCDIR} doc

clean:
	@rm -frv `hg status --unknown --no-status`