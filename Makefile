.PHONY: compile

REBAR=./rebar3

compile:
	$(REBAR) escriptize

typecheck:
	$(REBAR) do dialyzer,xref

clean:
	$(REBAR) clean

ci: compile
