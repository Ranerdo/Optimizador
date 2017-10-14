module textBoxHandler
implicit none

include 'Inc\textBoxFunc.inc'

    contains

        !!Gets the data from the nonempty textbox and returns them in the Tablillas variable
        subroutine getDataFromTextBox(textBox,Tablillas)
            use appgraphics
            use globals, only: tablilla
            implicit none
            character(10)::dummychar
            integer::i,dummy,j
            integer,dimension(:,:),allocatable,intent(in)::textBox
            type(tablilla),dimension(:),allocatable,intent(inout)::Tablillas

            j = getMinNonEmptyTextBoxSizeFromStart(textBox)
            if (j==0) write(*,*)'Wrong way!' !TODO: add this case

            do i=1,j
                dummy = gettextboxcontents(textBox(i,1),Tablillas(i)%model)

                dummy = gettextboxcontents(textBox(i,2),dummychar)
                read(dummychar,*)Tablillas(i)%long

                dummy = gettextboxcontents(textBox(i,3),dummychar)
                read(dummychar,*)Tablillas(i)%cant
            end do

        end subroutine


        subroutine textBoxEnablerHandler(textBox)
            use appgraphics
            implicit none
            character(10)::dummyChar
            integer::i,j,dummy
            integer,dimension(:,:),allocatable,intent(in)::textBox


                do i = 1,size(textBox(:,1))
                    if (.not.isTextBoxRowEmpty(textBox,i) ) then
                        if( i>1 .and.&
                            isTextBoxRowEmpty(textBox,i-1) )  then

                            do j=1,size(textBox(1,:))
                                dummy = gettextboxcontents(textBox(i,j),dummychar)
                                call settextboxcontents(textBox(i-1,j),dummychar)
                                call settextboxcontents(textBox(i,j),'')
                            end do

                        end if
                    end if

                    if (isTextBoxRowFullUpToColumn(textBox,i,3)) then
                            do j=1,size(textBox(1,:))
                                call enabletextbox(textBox(i+1,j),.true.)
                            end do
                    end if

                    if ( i>1 .and. getMaxNonEmptyTextBox(textBox)>0 .and. i > getMaxNonEmptyTextBox(textBox) &
                         .and. .not.isTextBoxRowFullUpToColumn(textBox,i-1,3) ) then
                        do j=1,size(textBox(1,:))
                            call enabletextbox(textBox(i,j),.false.)
                        end do
                    end if

                end do
        end subroutine

        subroutine clearTextBox(textBox)
            use appgraphics
            implicit none
            integer::i,j
            integer,dimension(:,:),allocatable,intent(in)::textBox

            if (.not.allocated(textBox)) return

            do i=1,size(textBox(:,1))
                do j=1,size(textBox(1,:))
                    call settextboxcontents(textbox(i,j),'')
                end do
            end do
        end subroutine


        subroutine setDataToTextBox(Tablillas,textBox)
            use appgraphics
            use globals, only: tablilla
            implicit none
            character(10)::dummychar
            integer::i
            integer,dimension(:,:),allocatable,intent(in)::textBox
            type(tablilla),dimension(:),allocatable,intent(in)::Tablillas

            do i=1,size(Tablillas(:))
                write(dummychar,'(A)')trim(Tablillas(i)%model)
                call settextboxcontents(textBox(i,1),trim(dummychar))

                write(dummychar,'(F0.2)')Tablillas(i)%long
                call settextboxcontents(textBox(i,2),trim(dummychar))

                write(dummychar,'(I0)')Tablillas(i)%cant
                call settextboxcontents(textBox(i,3),trim(dummychar))
            end do
        end subroutine
end module
