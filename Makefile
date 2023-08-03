PROJECT = blackbox
PROJECT_DESCRIPTION = "Electronic recording device placed inside a machine"
PROJECT_VERSION = 0.0.1

DEPS = erlbox gen_batch_server

dep_erlbox = git https://github.com/erlmachine/erlbox
dep_gen_batch_server = git https://github.com/rabbitmq/gen-batch-server.git v0.8.8

TEST_DEPS = meck

dep_meck = git https://github.com/eproxus/meck.git 0.9.2

include erlang.mk
