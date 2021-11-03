NARY_PATH 		:=$(shell stack path --local-install-root --allow-different-user)
NAME 			= hal

all:
		@stack install $(NAME):exe:$(NAME)-exe --local-bin-path '.' --allow-different-user
		@mv $(NAME)-exe $(NAME)

clean:
		@stack clean --allow-different-user

fclean: clean
		@rm -rf .stack-work $(NAME)

re: fclean all

tests_run:
	@stack test --allow-different-user

.PHONY: all clean fclean re
