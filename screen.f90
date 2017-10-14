module screen
!!Handles all visuals from the screen
use appgraphics
use control
implicit none
character(:),allocatable::       rulename
integer::                        rule


   contains

       subroutine initScreen()
            use nardothCommon
            use globals, only: myscreen
            use mdata
            integer,parameter::         windowSizeX=800,windowSizeY=600

            myscreen = initwindow(windowSizeX,windowSizeY,left=(getmaxwidth()-windowSizeX)/2,&
                top=(getmaxheight()-windowSizeY)/2,title='Optimizador de Corte',closeflag=.true.)

            !call enableresize()
            call setbkcolor(systemcolor(COLOR_WINDOW_BKGD))
            call setfillstyle(SOLID_FILL,creatergb(190,190,190))
            call floodfill(0,0,WHITE)
            call setdialoghotkeys(.true.)

            !TODO rule to measure place of buttons
            call setcolor(BLACK)
            call line(0,20,800,20)
            call line(340,0,340,600)

            call settextstyle(SANS_SERIF_FONT,HORIZ_DIR,10)

            do rule=10,790,10
                call outtextxy(rule,20,int2str(rule))
            end do

            do rule=10,590,10
                call outtextxy(340,rule,int2str(rule))
            end do
            call setmatchthemetextstyle()
            !!End of rule


            call init_menu()
            call init_body()

            call loop()

            call closewindow(myscreen)
       end subroutine

end module
