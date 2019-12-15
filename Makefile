.PHONY: compile

REBAR=./rebar3

compile:
	$(REBAR) compile

typecheck:
	$(REBAR) do dialyzer,xref

clean:
	$(REBAR) clean

ci: compile
