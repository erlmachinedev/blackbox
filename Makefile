PROJECT = blackbox
PROJECT_DESCRIPTION = "Electronic recording device placed inside a machine"
PROJECT_VERSION = 0.0.1

DEPS = erlbox

dep_erlbox = git https://github.com/erlmachine/erlbox

TEST_DEPS = meck

dep_meck = git https://github.com/eproxus/meck.git 0.9.2

include erlang.mk
