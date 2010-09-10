
ERL=erl
APP=perv


all: dir links erl 

dir:
	-@mkdir -p ebin priv/tmp priv/log priv/files/image

links:
	-@cd priv/www && test -L img || ln -s ../files/image img

erl:
	@$(ERL) -noinput +B \
		-eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

clean:  
	@rm -fv ebin/*.beam 


