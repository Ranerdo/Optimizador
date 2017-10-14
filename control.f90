!!Handles all buttons and menus
module control
    use nardothCommon
    use globals
    implicit none
    integer::                       rootMenu,archivoMenu,ayudaMenu
    integer::                       botonExit,&
                                    smallListWindow,&
                                    gotoblock,&          !Handles where textBoxEnablerHandler should be called
                                    closingWindowState=0 !Controls the adqueate execution of the process to close windows
    integer,dimension(:,:),allocatable::tempTextBox


    contains

        subroutine init_menu()
            use appgraphics
            implicit none
            rootMenu = addmenu("",MENU_FOR_WINDOW)
            archivoMenu = addmenu("Archivo",rootMenu)
            ayudaMenu = addmenu("Ayuda",rootMenu)

            botonExit = addmenuitem("Salir",archivoMenu,quitProgram)
        end subroutine

        subroutine init_body()
            use textBoxHandler
            use appgraphics
            use mdata, only:updateStockFromCSV
            implicit none
            integer::                   i
            integer,parameter::         startPosX = 60,&
                startPosX2 = 385,&
                startPosY = 60,&
                height = 20,&
                width1 = 70,&
                width2 = 50,&
                width3 = 70,&
                width4 = 150
            integer::                   buttonAdd,&
                botonRemove,&
                botonFilter,&
                botonOrder(4,2),&
                scrollBarId1,&
                scrollBarId2

            allocate(textBox(25,3),textBox2(25,4))

            !Setting up text boxes
            !LEFT SET
            call setcolor(BLACK)
            call settextstyle(SANS_SERIF_FONT,HORIZ_DIR,20)
            call outtextxy(startPosX+(width1+width2+width3-textwidth("STOCK"))/2,startPosY-40,"STOCK")
            call setmatchthemetextstyle()
            do i=0,24
                textBox(i+1,1) = createtextbox(startPosX,startPosY+i*height,width1,height)
                textBox(i+1,2) = createtextbox(startPosX+width1,startPosY+i*height,width2,height)
                call settextjustify(RIGHT_TEXT,TOP_TEXT)
                textBox(i+1,3) = createtextbox(startPosX+width2+width1,startPosY+i*height,width3,height)
                call settextjustify(LEFT_TEXT,TOP_TEXT)

            end do

            scrollBarId1 = createscrollbar(startPosX+width2+width1+width3,startPosY,17,25*height,SCROLL_VERTICAL&
                ,dummyInteger)


            !RIGHT SET
            call settextstyle(SANS_SERIF_FONT,HORIZ_DIR,20)
            call outtextxy(startPosX2+(width1+width2+width3+width4-textwidth("PEDIDO"))/2,startPosY-40,"PEDIDO")
            call setmatchthemetextstyle()
            do i=0,24
                textBox2(i+1,1) = createtextbox(startPosX2,startPosY+i*height,width1,height)
                textBox2(i+1,2) = createtextbox(startPosX2+width1,startPosY+i*height,width2,height)
                call settextjustify(RIGHT_TEXT,TOP_TEXT)
                textBox2(i+1,3) = createtextbox(startPosX2+width2+width1,startPosY+i*height,width3,height)
                call settextjustify(LEFT_TEXT,TOP_TEXT)
                textBox2(i+1,4) = createtextbox(startPosX2+width1+width2+width3,startPosY+i*height,width4,height)

            end do

            scrollBarId2 = createscrollbar(startPosX2+width2+width1+width3+width4,startPosY,17,25*height,&
                SCROLL_VERTICAL,dummyInteger)

            !Ordering Buttons
            block
                integer,parameter::             heightOrder=17
                !LEFT SET
                botonOrder(1,1) = createbutton(startPosX,startPosY-heightOrder,width1,heightOrder,"Modelo",void)
                botonOrder(2,1) = createbutton(startPosX+width1,startPosY-heightOrder,width2,heightOrder,"Medida",void)
                botonOrder(3,1) = createbutton(startPosX+width1+width2,startPosY-heightOrder,width3,heightOrder,"Cantidad",void)

                !RIGHT SET
                botonOrder(1,2) = createbutton(startPosX2,startPosY-heightOrder,width1,heightOrder,"Modelo",void)
                botonOrder(2,2) = createbutton(startPosX2+width1,startPosY-heightOrder,width2,heightOrder,"Medida",void)
                botonOrder(3,2) = createbutton(startPosX2+width1+width2,startPosY-heightOrder,width3,heightOrder,"Cantidad",void)
                botonOrder(4,2) = createbutton(startPosX2+width1+width2+width3,startPosY-heightOrder,width4,heightOrder,&
                    "Descripcion",void)
            end block

            !Disable boxes to prevent edition
            do i=1,25
                call enabletextbox(textBox(i,1),.false.)
                call enabletextbox(textBox(i,2),.false.)
                call enabletextbox(textBox(i,3),.false.)


                if (i==1) then      !Except the first one on the right
                    continue
                else
                    call enabletextbox(textBox2(i,1),.false.)
                    call enabletextbox(textBox2(i,2),.false.)
                    call enabletextbox(textBox2(i,3),.false.)
                    call enabletextbox(textBox2(i,4),.false.)
                end if
            end do

            gotoblock = 100
            call updateStockFromCSV()
!$OMP PARALLEL

!$OMP SECTIONS
!$OMP SECTION
            !Left side Buttons
            !TODO add procedures to buttons
            block
                integer,parameter::     buttonStartX=270,&
                    buttonStartY=110

                buttonAdd = createbutton(buttonStartX,buttonStartY,width1,height,"Agregar",agregarTablillas)
                botonRemove = createbutton(buttonStartX,buttonStartY+height,width1,height,"Quitar",quitarTablillas)
                botonFilter = createbutton(buttonStartX,buttonStartY+height*2+10,width1,height,"Filtrar",filtrar)

            end block
!$OMP SECTION
            do
                select case(gotoblock)
                    case(100)
                        if(getcurrentwindow()==myscreen) then
                            call textBoxEnablerHandler(textBox2)
                        end if

                    case(200)
                        if (getcurrentwindow()==smallListWindow) then
                            if(allocated(tempTextBox)) then
                                call textBoxEnablerHandler(tempTextBox)
                            end if
                        end if

                        if ( closingWindowState == 1 ) closingWindowState = 2 !Window trying to close

                        do while( closingWindowState == 2) !Wait here until closing process is complete
                        continue
                        end do

                    case default
                    continue
                 end select
            end do

!$OMP END SECTIONS

!$OMP END PARALLEL

            contains
                subroutine filtrar()
                    use mdata
                    call updateStockFromCSV()
                end subroutine

            end subroutine

            subroutine agregarTablillas()
                implicit none
                call initSmallListWindow("Agregar tablillas a stock")
            end subroutine

            subroutine quitarTablillas()
                implicit none
                call initSmallListWindow("Remover tablillas de stock")
            end subroutine

            subroutine initSmallListWindow(titlestr)
                use appgraphics
                use mdata
                use textBoxHandler
                implicit none
                character(*)::          titlestr
                integer::               buttonAdd,&
                    buttonCancel,&
                    i
                integer,parameter::     startPosX = 10,&
                    startPosY = 10,&
                    height = 20,&
                    width1 = 70,&
                    width2 = 50,&
                    width3 = 70,&
                    smallWindowSizeX=startPosX*2+width1+width2+width3,&
                    smallWindowSizeY=startPosY*5+15*height


                if (.not.allocated(tempTextBox)) allocate(tempTextBox(15,3))

                smallListWindow = initwindow(smallWindowSizeX,smallWindowSizeY,title=titlestr,closeflag=.false.)

                call setbkcolor(systemcolor(COLOR_WINDOW_BKGD))
                call setfillstyle(SOLID_FILL,creatergb(190,190,190))
                call floodfill(0,0,WHITE)

                call enablewindow(myscreen,.false.)
                call setdialoghotkeys(.true.)


                do i=1,15
                    tempTextBox(i,1) = createtextbox(startPosX,startPosY+(i-1)*height,width1,height)
                    tempTextBox(i,2) = createtextbox(startPosX+width1,startPosY+(i-1)*height,width2,height)
                    call settextjustify(RIGHT_TEXT,TOP_TEXT)
                    tempTextBox(i,3) = createtextbox(startPosX+width2+width1,startPosY+(i-1)*height,width3,height)
                    call settextjustify(LEFT_TEXT,TOP_TEXT)


                    !Can't call textBoxEnablerHandler here for some reason so I have to do it manually
                    if (i>1) then
                        call enabletextbox(tempTextBox(i,1),.false.)
                        call enabletextbox(tempTextBox(i,2),.false.)
                        call enabletextbox(tempTextBox(i,3),.false.)
                    end if

                end do


                if ( titlestr == "Agregar tablillas a stock" ) then
                    buttonAdd = createbutton(int(smallWindowSizeX/2.)-width1-10,smallWindowSizeY-20,width1,&
                    height,"Agregar",addRoutine)
                else if ( titlestr == "Remover tablillas de stock" ) then
                    buttonAdd = createbutton(int(smallWindowSizeX/2.)-width1-10,smallWindowSizeY-20,width1,&
                    height,"Remover",removeRoutine)
                end if

                buttonCancel = createbutton(int(smallWindowSizeX/2.)+10,smallWindowSizeY-20,width1,height,"Cancelar",cancelRoutine)



                gotoblock=200
                call loop()

                call n_closewindow(smallListWindow,tempTextBox)

                contains

                    subroutine addRoutine()
                        use textBoxHandler
                        use mdata
                        implicit none
                        integer::i
                        type(tablilla),dimension(:),allocatable::tablillas

                        i = getMinNonEmptyTextBoxSizeFromStart(tempTextBox)
                        allocate(tablillas(i))

                        call getDataFromTextBox(tempTextBox,tablillas)
                        call addTablillasToStock(tablillas)

                        call n_closewindow(smallListWindow,tempTextBox)

                    end subroutine

                    subroutine removeRoutine()
                        use textBoxHandler
                        use mdata
                        implicit none
                        integer::i,j
                        type(tablilla),dimension(:),allocatable::tablillas

                        i = getMinNonEmptyTextBoxSizeFromStart(tempTextBox)
                        allocate(tablillas(i))

                        call getDataFromTextBox(tempTextBox,tablillas)
                        call removeTablillasFromStock(tablillas)

                        call n_closewindow(smallListWindow,tempTextBox)

                    end subroutine


                    subroutine cancelRoutine()
                        implicit none

                        call n_closewindow(smallListWindow,tempTextBox)
                    end subroutine

            end subroutine

            !!Closes the window and pass control to myscreen, and optionally deallocates an array
            subroutine n_closewindow(window,textBox)
                use appgraphics
                implicit none
                integer,intent(in)::window
                integer,dimension(:,:),allocatable,optional,intent(inout)::textBox
                integer::allostat

                closingWindowState=1 !Attempting to close window
                do while(closingWindowState>0) !Wait here until every process if the window completed execution
                    continue
                    if ( closingWindowState == 2) exit !If every process needed to be completed finished, exit the loop
                end do

                gotoblock = 100
                call enablewindow(myscreen,.true.)
                call setcurrentwindow(myscreen)
                call closewindow(window)

                if (present(textBox)) then
                    if (allocated(textBox)) deallocate(textBox,STAT=allostat)
                end if

                closingWindowState = 0 !Window closed, go back to normal
            end subroutine


            subroutine quitProgram()
                use appgraphics
                call closewindow(myscreen)
                stop !needed to stop execution of the program
            end subroutine

end module
