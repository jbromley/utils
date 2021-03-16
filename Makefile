NAME=utils
BINARY=$(HOME)/.local/bin/$(NAME)
SCRIPT=$(PWD)/$(NAME)
CL=cl-launch
LISP=sbcl

.PHONY: all $(NAME) clean

all: $(NAME)

$(NAME):
	@$(CL) --output $(NAME) --dump ! --lisp $(LISP) --quicklisp --system $(NAME) --dispatch-system $(NAME)/main

install: $(NAME)
	@ln -sf $(SCRIPT) $(BINARY)
	@$(SCRIPT) symlink $(NAME)

clean:
	@rm -f $(NAME) *~
