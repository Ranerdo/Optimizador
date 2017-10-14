module mdata
use globals
!use nardothCommon
implicit none

    interface operator(==)
        module procedure TablillasCompare
    end interface

    interface operator(+)
        module procedure TablillasSum
    end interface

    interface operator(-)
        module procedure TablillasSubstract
    end interface


    contains

        subroutine readDataFromCSV(rTablillas,filename)
            implicit none
            character(1)::dummychar
            character(*),intent(in)::filename
            integer::i,j
            type(tablilla),dimension(:),allocatable,intent(out)::rTablillas

            open(100,file=filename,action='read')

            i=0

            do
                read(100,*,end=150)dummychar
                i=i+1
            end do

150         if (i==0) goto 151
            if (allocated(rTablillas)) deallocate(rTablillas)
            allocate(rTablillas(i))

            rewind(100)
            do j=1,i
                read(100,*)rTablillas(j)%model,rTablillas(j)%long,rTablillas(j)%cant
            end do

151            close(100)

        end subroutine

        subroutine writeDataToCSV(wTablillas,filename,fileaccess)
            implicit none
            character(19)::fmt
            character(*),intent(in)::filename
            character(*),intent(in),optional::fileaccess
            integer::i
            type(tablilla),dimension(:),allocatable,intent(inout)::wTablillas


            if (present(fileaccess)) then
                open(100,file=filename,access=fileaccess)
            else
                open(100,file=filename)
            end if

            !TODO: Add check for start of a new line

            fmt='(A,";",F0.2,";",I0)'

            do i=1,size(wTablillas)
                write(100,fmt)trim(wTablillas(i)%model),wTablillas(i)%long,wTablillas(i)%cant
            end do

            close(100)
        end subroutine

        subroutine updateStockFromCSV()
            use textBoxHandler
            implicit none

            call readDataFromCSV(stock,stockCSV)
            if (allocated(stock)) call renderStockInTextBox()
        end subroutine

        subroutine updateCSVFromStock()
            implicit none

            call writeDataToCSV(stock,stockCSV)
            call renderStockInTextBox()
        end subroutine

        subroutine renderStockInTextBox()
            use textBoxHandler
            use globals, only: textBox,stock
            implicit none

            call clearTextBox(textBox)
            call setDataToTextBox(stock,textBox)
        end subroutine



        logical function TablillasCompare(tablilla1,tablilla2) result(logic)
            use globals, only: tablilla
            implicit none
            type(tablilla),intent(in)::tablilla1,tablilla2

            if ( (tablilla1%model == tablilla2%model).and.(tablilla1%long == tablilla2%long) ) then
                logic = .true.
            else
                logic = .false.
            end if
        end function


        type(tablilla) function TablillasSum(tablilla1,tablilla2) result(tablillaResult)
            use globals, only: tablilla
            implicit none
            type(tablilla),intent(in)::tablilla1,tablilla2

            if ( tablilla1 == tablilla2 ) then
                tablillaResult%model = tablilla1%model
                tablillaResult%long = tablilla1%long
                tablillaResult%cant = tablilla1%cant + tablilla2%cant
            else
                write(*,*)'Error en suma de tablillas'
                write(*,*)'No se puede operar con diferentes tipos de tablillas'
                stop
            end if
        end function


        type(tablilla) function TablillasSubstract(tablilla1,tablilla2) result(tablillaResult)
            use globals, only: tablilla
            implicit none
            type(tablilla),intent(in)::tablilla1,tablilla2

            if ( tablilla1 == tablilla2 ) then
                if ( tablilla1%cant >= tablilla2%cant ) then
                    tablillaResult%model = tablilla1%model
                    tablillaResult%long = tablilla1%long
                    tablillaResult%cant = tablilla1%cant - tablilla2%cant
                else
                    write(*,*)'Error en resta de tablillas'
                    write(*,*)'La cantidad del operando izquierdo no puede ser menor que la del operando derecho'
                    stop
                end if
            else
                write(*,*)'Error en resta de tablillas'
                write(*,*)'No se puede operar con diferentes tipos de tablillas'
                stop
            end if
        end function

        subroutine updateTablillasSet(Tablillas)
            use globals, only: tablilla
            implicit none
            integer::i,j,numberOfRepeated=0,temp_k,sizeT
            type(tablilla),dimension(:),allocatable,intent(inout)::Tablillas

            if( .not.allocated(Tablillas) ) return

            sizeT = size(Tablillas)
            do i = sizeT,2,-1
                block
                    type(tablilla),dimension(:),allocatable::sTablillas

                    allocate(sTablillas(i-1))
                    sTablillas = Tablillas(1:i-1)

                    temp_k = SearchForFirstTablillaInTablillaSet(Tablillas(i),sTablillas)
                    deallocate(sTablillas)
                end block

                if (temp_k==0) then
                    continue
                else
                    Tablillas(temp_k)%cant = Tablillas(temp_k)%cant + Tablillas(i)%cant
                    if ( i < sizeT ) then
                        do j=i,sizeT-1
                            Tablillas(j) = Tablillas(j+1)
                        end do
                    end if
                    Tablillas(sizeT)%cant = 0
                    numberOfRepeated=numberOfRepeated+1
                end if
            end do

            if ( numberOfRepeated > 0 ) then
                block
                    type(tablilla),dimension(sizeT-numberOfRepeated)::dummyTablillas

                    dummyTablillas = Tablillas(1:sizeT-numberOfRepeated)
                    deallocate(Tablillas)
                    allocate(Tablillas(sizeT-numberOfRepeated))
                    Tablillas = dummyTablillas
                end block
            end if

        end subroutine

        integer function SearchForFirstTablillaInTablillaSet(varTablilla,Tablillas) result(k)
            use globals, only: tablilla
            implicit none
            integer::i
            type(tablilla),intent(in)::varTablilla
            type(tablilla),dimension(:),allocatable,intent(in)::Tablillas

            k=0
            do i=1,size(Tablillas)
                if (varTablilla==Tablillas(i)) then
                    k=i
                    exit
                end if
            end do
        end function

        function searchForTablillasInTablillaSet(varTablilla,TablillasSet) result(k)
            implicit none
            integer::i
            integer,dimension(:),allocatable::j
            integer,dimension(:),allocatable::k
            type(tablilla),intent(in)::varTablilla
            type(tablilla),dimension(:),allocatable,intent(in)::TablillasSet
            type(tablilla),dimension(:),allocatable::dummyTablillas

            i=1

            do

                if ( i == 1 ) then
                    k(i) = searchForFirstTablillaInTablillaSet(varTablilla,TablillasSet)
                else if ( i == size(TablillasSet)+1 ) then
                    i = i-1
                    allocate(j(i))
                    j = k(1:i)
                    deallocate(k)!make this a subroutine
                    allocate(k(i))
                    k = j
                    deallocate(j)
                    return!FIXME:check dis block
                else

                allocate(dummyTablillas(size(TablillasSet)-k(i-1)))
                dummyTablillas = TablillasSet(k(i-1)+1:size(TablillasSet))
                k(i) = searchForFirstTablillaInTablillaSet(varTablilla,dummyTablillas)
                end if

                if ( k(i) == 0 ) then
                    i = i-1
                    allocate(j(i))
                    j = k(1:i)
                    deallocate(k)
                    allocate(k(i))
                    k = j
                    deallocate(j)
                    return
                else
                    allocate(j(i))
                    j = k
                    deallocate(k)
                    i = i+1
                    allocate(k(i))
                    k(1:i-1) = j
                    deallocate(j)
                end if

            end do

        end function


        subroutine addTablillasToStock(Tablillas)
            use globals, only: tablilla,stock
            implicit none
            integer::i
            type(tablilla),dimension(:),allocatable,intent(inout)::Tablillas

            i=size(Tablillas)

            if ( .not.allocated(stock) ) then
                allocate(stock(i))
                stock = Tablillas
            else
                block
                integer::j
                type(tablilla),dimension(:),allocatable::temp_stock
                    j=size(stock)
                    allocate(temp_stock(j))
                    temp_stock = stock

                     deallocate(stock)
                     allocate(stock(j+i))
                     stock(1:j) = temp_stock
                     stock(j+1:j+i) = Tablillas

                     deallocate(temp_stock,Tablillas)
                 end block
             end if

             call updateTablillasSet(stock)
             call updateCSVFromStock()
        end subroutine

        subroutine removeTablillasFromStock(Tablillas)
            use globals, only: tablilla,stock
            implicit none
            integer::i,j,k,s
            integer,dimension(:),allocatable::indexOfRepeatedTablillas
            type(tablilla),dimension(:),allocatable::repeatedTablillas
            type(tablilla),dimension(:),allocatable,intent(inout)::Tablillas


            i=size(Tablillas)

            do j=1,i
                if ( SearchForFirstTablillaInTablillaSet(Tablillas(j),stock) == 0 ) then
                    !TODO: call error inexistent tablillas
                    return
                end if

                indexOfRepeatedTablillas = searchForTablillasInTablillaSet(Tablillas(j),Tablillas) !!?

                s=0
                allocate(repeatedTablillas(size(indexOfRepeatedTablillas)))
                do k = 1,size(indexOfRepeatedTablillas)
                    repeatedTablillas(k) = Tablillas(indexOfRepeatedTablillas(k))
                    s = s+repeatedTablillas(k)%cant
                end do

                if ( s > stock(searchForFirstTablillaInTablillaSet(Tablillas(j),stock))%cant ) then
                    !TODO: call error for not enough tablillas to substract
                    return
                end if
            end do

            do j = 1,i
                k = searchForFirstTablillaInTablillaSet(Tablillas(j),stock)
                stock(k) = stock(k)-Tablillas(j)
            end do

            call updateCSVFromStock()

        end subroutine




end module
