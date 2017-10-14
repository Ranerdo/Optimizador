!Handles all variables that must be globally accesible in order to prevent circular dependencies
module globals
    implicit none
    character(9),parameter::stockCSV='stock.csv'
    integer::                           myscreen
    integer,dimension(:,:),allocatable::textBox,textBox2

    type tablilla
        character(10)::model
        real::long
        integer::cant
    end type

    type(tablilla),dimension(:),allocatable::stock,pedido
end module globals
