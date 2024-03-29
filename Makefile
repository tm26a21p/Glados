##
## EPITECH PROJECT, 2023
## B-FUN-500-PAR-5-2-glados-florian.labarre
## File description:
## Makefile
##

NAME	= glados

all:
	stack --local-bin-path . install
	mv glados-exe $(NAME)

clean:
	stack clean;

fclean:	clean
	rm -f $(NAME)

re:	fclean all

.PHONY:	all clean fclean re