! DOC
!
!  fsample.f90 - example of a Fortran extension for Tcl/Tk
!
!  Arjen Markus
!
!
!  General information:
!  This file shows how to use the Ftcl library to create
!  Fortran extensions to Tcl/Tk.
!  Although it uses Fortran 90/95, the principles hold for
!  FORTRAN 77 too.
!
! ENDDOC
!
!  $Author$
!  $Date$
!  $Source$
!  $Log$
!
! --------------------------------------------------------------------
!   Module:   FSAMPLE
!   Author:   Arjen Markus
!   Purpose:  Provide two new commands for Tcl
!   Context:  Part of the package "Fsample"
!   Summary:
!             Defines - in a module - two commands for Tcl
! --------------------------------------------------------------------
submodule (textBoxHandler) textBoxFunc
implicit none

    contains
! --------------------------------------------------------------------
!   Routine:  set_random
!   Author:   Arjen Markus
!   Purpose:  Set the argument to a random number
!   Context:  Part of the package "Fsample"
!   Summary:
!             Set the variable whose name is the first argument to
!             a random number
!   Notes:
!             Do not specify the intents for the arguments!
!             We do not know how these work out in a C environment
! --------------------------------------------------------------------
        integer function getMinNonEmptyTextBoxSizeFromStart(textBox) result(k)
            implicit none
            integer,dimension(:,:),allocatable,intent(in)::textBox

            k=getMinNonEmptyTextBoxSizeFrom(textBox,1)

        end function


        integer function getMinNonEmptyTextBoxSizeFrom(textBox,j) result(k)
                use appgraphics
                implicit none
                integer,intent(in)::j
                integer::i
                integer,dimension(:,:),allocatable,intent(in)::textBox

                do i=j,size(textBox(:,1))
                    if (.not.isTextBoxRowFullUpToColumn(textBox,i,3)) exit
                end do

                k=i-1
        end function

        integer function getMaxNonEmptyTextBox(textBox) result(k)
            use appgraphics
            implicit none
            integer::i
            integer,dimension(:,:),allocatable,intent(in)::textBox

            k=0

            do i=1,size(textBox(:,1))
                if( .not.isTextBoxRowEmpty(textBox,i) ) k=i
            end do

            if ( k == size(textBox(:,1)) ) k=-1

        end function


        logical function isTextBoxRowFull(textBox,i) result(logic)
            implicit none
            integer,dimension(:,:),allocatable,intent(in)::textBox
            integer,intent(in)::i

            logic = isTextBoxRowFullUpToColumn(textBox,i,size(textBox(1,:)))

        end function

        logical recursive function isTextBoxRowFullUpToColumn(textBox,i,j) result(logic)
            use appgraphics
            implicit none
            character(10)::dummychar
            integer,dimension(:,:),allocatable,intent(in)::textBox
            integer,intent(in)::i,j

            if (j==1) then
                logic = (gettextboxcontents(textBox(i,j),dummychar)>0)
            else
                logic = (gettextboxcontents(textBox(i,j),dummychar)>0).and.isTextBoxRowFullUpToColumn(textBox,i,j-1)
            end if

        end function

        logical function isTextBoxRowEmpty(textBox,i) result(logic)
            implicit none
            integer,dimension(:,:),allocatable,intent(in)::textBox
            integer,intent(in)::i

            logic = isTextBoxRowEmptyUpToColumn(textBox,i,size(textBox(1,:)))

        end function

        logical recursive function isTextBoxRowEmptyUpToColumn(textBox,i,j) result(logic)
            use appgraphics
            implicit none
            character(10)::dummychar
            integer,dimension(:,:),allocatable,intent(in)::textBox
            integer,intent(in)::i,j


            if (j==1) then
                logic = (gettextboxcontents(textBox(i,j),dummychar)==0)
            else if (j>1) then
                logic = (gettextboxcontents(textBox(i,j),dummychar)==0).and.isTextBoxRowEmptyUpToColumn(textBox,i,j-1)
            end if

        end function

end submodule
