PROJECT = wol
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = jsx qdate yamerl

SHELL_OPTS = \
  -s ${PROJECT} \
  -eval "wol_test:test()"

include erlang.mk
