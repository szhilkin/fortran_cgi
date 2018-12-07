! Requires:
!    - the FLIBS modules cgi_protocol and fcgi_protocol
!    - the FastCGI library
! See 'readme' for setup instructions of the compiler, nignx, and FastCGI library
!
! See 'makefile' for compile and execute commands. In summary,
!   To compile dart_cgi.F90      : make
!   To execute as FastCGI process : spawn-fcgi -a 127.0.0.1 -p 9000 ./test_fcgi
!      The "-a 127.0.0.1" and "-p 9000" options to spawn-fcgi must match the
!          "fastcgi_pass   127.0.0.1:9000;" in nginx.conf
!
! Notes:
!    1. Example 2 is from FLIBS test_cgi.f90
!    2. Customize routine respond() for your own application
!

program dart_cgi

    use fcgi_protocol

    implicit real*8 (a-h, o-y)

    type(DICT_STRUCT), pointer  :: dict => null() ! Initialisation is important!
    logical                     :: stopped = .false. ! set to true in respond() to terminate program
    integer                     :: unitNo ! unit number  for a scratch file

    ! open scratch file
    open(newunit=unitNo, status='scratch')
    ! comment previous line AND uncomment next line for debugging;
    !open(newunit=unitNo, file='fcgiout', status='unknown') ! file 'fcgiout' will show %REMARKS%

    ! wait for environment variables from webserver
    do while (fcgip_accept_environment_variables() >= 0)

        ! build dictionary from GET or POST data, environment variables
        call fcgip_make_dictionary( dict, unitNo )

        ! give dictionary to the user supplied routine
        ! routine writes the response to unitNo
        ! routine sets stopped to true to terminate program
        call respond(dict, unitNo, stopped)

        ! copy file unitNo to the webserver
        call fcgip_put_file( unitNo, 'text/html' )

        ! terminate?
        if (stopped) exit

    end do !  while (fcgip_accept_environment_variables() >= 0)

    ! before termination, it is good practice to close files that are open
    close(unitNo)

    ! webserver will return an error since this process will now terminate
    unitNo = fcgip_accept_environment_variables()


contains


    subroutine respond ( dict, unitNo, stopped )

        type(DICT_STRUCT), pointer        :: dict
        integer, intent(in)               :: unitNo
        logical, intent(out)              :: stopped

        ! the following are defined in fcgi_protocol
        !character(len=3), parameter :: AFORMAT = '(a)'
        !character(len=2), parameter :: CRLF = achar(13)//achar(10)
        !character(len=1), parameter :: NUL = achar(0)

        ! the script name
        character(len=80)  :: scriptName

        ! variables for Example 2 (from test_cgi.f90 of FLIBS)
        integer                           :: steps
        real                              :: xmin
        real                              :: xmax
        character(len=20)                 :: fnName
        character(len=20)                 :: output

        real, dimension(:), allocatable   :: x
        real, dimension(:), allocatable   :: y

        integer                           :: i
        logical                           :: okInputs

        ! start of response
        ! lines starting with %REMARK% are for debugging & will not be copied to webserver
        write(unitNo, AFORMAT) &
            '%REMARK% respond() started ...', &
            '<html>', &
            '<head><title>FastCGI Application</title></head>', &
            '<body>', &
            '<h1>FastCGI Application</h1>'

        ! retrieve script name (key=DOCUMENT_URI) from dictionary
        call cgi_get( dict, "DOCUMENT_URI", scriptName )

        ! if ( trim(scriptName) /= '/' ) & ! a script was requested
            ! write(unitNo,AFORMAT) 'Script is : '//trim(scriptName)

        select case (trim(scriptName))

            case ('/calc') ! See form in Example 2 below
                ! keys are: function, minimum, maximum, steps, output

                fnName  = '?'
                xmin     = 0.0
                xmax     = 1.0
                steps    = 10
                call cgi_get( dict, "function", fnName  )
                call cgi_get( dict, "minimum",  xmin     )
                call cgi_get( dict, "maximum",  xmax     )
                call cgi_get( dict, "steps",    steps    )
                call cgi_get( dict, "output",   output   )

                write(unitNo, AFORMAT) '%REMARK% function='//trim(fnName )
                write(unitNo, '(a,f8.3)') '%REMARK% minimum=', xmin
                write(unitNo, '(a,f8.3)') '%REMARK% maximum=', xmax
                write(unitNo, '(a,i4)') '%REMARK% steps=', steps
                write(unitNo, AFORMAT) '%REMARK% output='//trim(output)

                okInputs = .true.
                if ( trim(fnName ) == '?' ) then
                    write(unitNo,AFORMAT) '<br>No function selected'
                    okInputs = .false.
                endif
                if ( abs(xmin) > 100.0 .or. abs(xmax) > 100.0 ) then
                    write(unitNo,AFORMAT) '<br>Minimum and maximum should be in the range -100 to 100'
                    okInputs = .false.
                endif
                if ( trim(fnName ) == 'J0' ) then
                    write(unitNo,AFORMAT) '<br>Sorry, the Bessel function is not yet implemented'
                    okInputs = .false.
                endif

                if (okInputs) then
                    !
                    ! Actual processing
                    !
                    allocate( x(0:steps), y(0:steps) )

                    x = (/ (xmin + i*(xmax-xmin)/steps, i=0,steps) /)
                    if ( trim(fnName ) == 'sin' ) then
                        y = sin(x)
                    endif
                    if ( trim(fnName ) == 'cos' ) then
                        y = cos(x)
                    endif

                    !
                    ! Write the HTML output or the CSV file
                    !
                    if ( trim(output) == 'html' ) then
                        write( unitNo,AFORMAT ) &
                            '<table>', &
                            '<tr><td>X</td><td>'//trim(fnName)//'(X)</td></tr>'
                        do i = 0,steps
                            write( unitNo, '(a,f12.6,a,f12.6,a)' ) &
                                '<tr><td>', x(i), '</td><td>', y(i), '</td></tr>'
                        enddo
                        write( unitNo,AFORMAT ) &
                            '</table>'
                    else
                        write( unitNo,AFORMAT ) &
                            '<pre>', '      X     ,      '//trim(fnName)//'(X)'
                        do i = 0,steps
                            write( unitNo, '(f12.6,a,f12.6)' ) x(i), ',', y(i)
                        enddo
                        write( unitNo,AFORMAT ) &
                            '</pre>'
                    endif

                end if


            case ('/shutdown') ! to terminate program
                write(unitNo,AFORMAT) '<br>Program has terminated.<br><a href="/">Verify</a>'
                stopped = .true.

        end select

        ! generate page for next action
        if (.not. stopped) then

            write(unitNo,AFORMAT) &
                "<hr>", &
                "<b>Example: GET method</b>", &
                "<form action='calc' method='get'>", &
                "<p>", &
                "Function: f(x) =", &
                "<select name='function'>", &
                "    <option value='sin' selected>sin(x)</option>", &
                "    <option value='cos'>cos(x)</option>", &
                "    <option value='J0'>J0(x)</option>", &
                "</select>", &
                "<br>", &
                "Domain:", &
                "<table>", &
                "<tr>", &
                "<td>Minimum = </td><td><input type='text' name='minimum' value='0.0'></td>", &
                "</tr>", &
                "<tr>", &
                "<td>Maximum = </td><td><input type='text' name='maximum' value='1.0'></td>", &
                "</tr>", &
                "<tr>", &
                "<td>Steps = </td><td><input type='text' name='steps' value='10'></td>", &
                "</tr>", &
                "</table>", &
                "<p>", &
                "Type of output:", &
                "<input type='radio' name='output' value='html', checked>HTML</input>", &
                "<input type='radio' name='output' value='csv'>CSV</input>", &
                "<p>", &
                "<input type='submit' value='Calculate'>", &
                "</form>"

            write(unitNo,AFORMAT) &
                "<hr>", &
                "<b>Example: Hyperlink</b><br>", &
                "&nbsp;<a href='shutdown'><b>Stop</b></a> the Fortran FastCGI program."

        end if

        ! end of response
        write(unitNo,AFORMAT) '</body>', '</html>', &
            '%REMARK% respond() completed ...'

        return

    end subroutine respond

subroutine calc

    implicit real*8 (a-h, o-y)
    COMMON /U/ A, ncab

    DIMENSION AI1(2001), AI2(2001), U(2001)
    DIMENSION BI1(2001), BI2(2001), BU(2001), A(6, 6)

    f1(t) = aim * (t / tau1)**10 * dexp(-t / tau2) / coef / (1.d0 + (t / tau1)**10)

    ! CALL system('chcp-1251')

    PRINT*, 'Amplituda toka - ? (kA)'
    read*, AIM
    print*, 'Impuls toka  10/350 -(1)    0,25/100  - (2)'
    print*, 'Bijeksponencialnyj impuls - (3)'
    read*, ntok

    if(ntok.eq.3) then
        67    print*, 'Konstanty fronta (a) i hvosta (b) impulsa (1/mks)'
        read*, at, bt
        tfr = dlog(at / bt) / (at - bt)
        umax = dexp(-bt * tfr) - dexp(-at * tfr)
        a00 = aim / umax  !!!!
        tim = 1. / bt
        dtim = tim / 100
        tim = 5. * tim
        do k = 1, 500
            tk = tfr + k * dtim
            utk = dexp(-bt * tk) - dexp(-at * tk)
            if(utk.le.umax / 2.) goto 66
        end do
        66    tim = tk
        zr1 = tfr
        print*, 'Vremja maksimuma -', zr1, ' mks'
        zr1 = tim
        print*, 'Dlitelnost impulsa -', zr1, ' mks'
        print*, 'Skorrektirovat parametry -? DA -(1), NET -(0)'
        read*, mpar
        if(mpar.eq.1) go to 67

    end if

    if(ntok.lt.2) then

        tau2 = 4.85d2
        tau1 = 1.9d1
        coef = 0.93d0
    else
        tau2 = 1.43d2
        tau1 = 0.454d0
        coef = 0.93d0
    end if

    print*, 'Dlina VL ot ob"ekta do PS-? (m)'
    read*, DL
    print*, 'Polnaja induktivnost VL -? (mkGn)'
    read*, alW
    print*, 'Udalenie tochki ot ob"ekta tochki udara molnii-(m)'
    read*, dlo

    alw1 = alw * dlo / dl
    dlp = dl - dlo
    alw2 = alw - alw1
    print*, 'Soprotivlenie zazemlenie ob"ekta -? (Om)'
    read*, RN
    print*, 'Soprotivlenie zazemlenija VL na PS -? (Om)'
    read*, RP
    print*, 'Chislo podzemnyh kabelej 1 ili 2 ?'
    read*, ncab
    PRINT*, 'Dlina podzemnogo kabelja # 1 -?  (m)'
    READ*, DL1
    print*, 'Radius kabelja # 1- ? (m)'
    read*, rs1
    print*, 'Glubina ukladki kabelja # 1- ? (m)'
    read*, h1
    PRINT*, 'Coprotivlenie v konce kabelja # 1 - ?  (Om)'
    READ*, RK1

    if(ncab.eq.2) then
        PRINT*, 'Dlina podzemnogo kabelja # 2 -?  (m)'
        READ*, DL2
        print*, 'Radius kabelja - # 2 ? (m)'
        read*, rs2
        print*, 'Glubina ukladki kabelja # 2- ? (m)'
        read*, h2
        PRINT*, 'Coprotivlenie v konce kabelja # 2 - ?  (Om)'
        READ*, RK2
    end if

    print*, 'Udelnoe soprotivlenie grunta - ? (Om m)'
    read*, ro
    PRINT*, 'Raschetnoe vremja TM, vremennoj shag DT  (mks)'
    READ*, TM, DT
    print*, 'Shag po dline kanala (m)'
    read*, dd
    PRINT*, 'Shag vyvoda na displej TTT, na pechat (mks)'
    READ*, TTT, TPP
    al1 = 0.2 * (dlog(2.d0 * dl1 / rs1) - 1.d0)
    if(h1.gt.rs1) then
        g1 = 6.283185 / ro / dlog(dl1 * dl1 / 2.d0 / rs1 / h1)
    else
        g1 = 3.141593 / ro / dlog(dl1 / rs1)
    end if

    IF(NCAB.EQ.2) THEN
        al2 = 0.2 * (dlog(2.d0 * dl2 / rs2) - 1.d0)
        if(h2.gt.rs2) then
            g2 = 6.283185 / ro / dlog(dl2 * dl2 / 2.d0 / rs2 / h2)
        else
            g2 = 3.141593 / ro / dlog(dl2 / rs2)
        end if
    END IF

    ZR1 = AIM
    write(14, *) '  '
    write(14, *) '                                  .  .2018'
    WRITE(14, *) 'Raspredelenie toka molnii mezhdu podz. kommunikaciej,'
    WRITE(14, *) '                VL i zazemlitelem ob"ekta '
    WRITE(14, *) '                              Programma dart_uzip'
    write(14, *) '         (gL-priblizhenie LR- priblizhenie VL)'
    WRITE(14, *) '  '
    WRITE(14, *) ' Amplituda toka =', ZR1, ' kA'

    if(ntok.eq.1) then
        write(14, *) ' Impuls toka molnii 10/350 mks'
    end if
    if(ntok.eq.2) THEN
        write(14, *) ' Impuls toka molnii 0,25/100 mks'
    end if
    IF(NTOK.EQ.3) THEN
        write(14, *) ' Bijeksponencialnyj impuls toka'
        zr1 = 1. / at
        zr2 = 1. / bt
        write(14, *) ' Postojannye vremeni Tf, Timp = ', zr1, zr2, ' mks'
    end if

    zr1 = dl
    write(14, *) ' Dlina VL - ', zr1, ' m'
    zr1 = alw
    write(14, *) ' Polnaja induktivnost VL - ', zr1, ' mkGn'
    zr1 = dlo
    write(14, *) ' Udalenie tochki udara ot ob"ekta - ', zr1, ' m'
    zr1 = rn
    write(14, *) ' Soprotivlenie zazemlenija ob"ekta - ', zr1, ' Om'
    zr1 = rp
    write(14, *) ' Soprotivlenie zazemlenija PS - ', zr1, ' Om'

    ZR1 = DL1
    WRITE(14, *) ' Dlina podzemnogo kabelja # 1- ', ZR1, ' m'
    zr1 = rs1
    write(14, *) ' Radius podzemnogo kabelja # 1- ', zr1, ' m'
    zr1 = h1
    write(14, *) ' Glubina podzemnogo kabelja # 1- ', zr1, ' m'
    zr1 = al1
    write(14, *) ' Pogonnaja induktivnost kabelja #1- ', zr1, ' MkGn/m'
    zr2 = g1
    write(14, *) ' Pogonnaja provodimost kabelja #1- ', zr2, ' 1/Om m'
    ZR1 = RK1
    WRITE(14, *) ' Soprotivlenie v konce kabelja # 1- ', zr1, ' Om'

    if(ncab.eq.2) Then
        ZR1 = DL2
        WRITE(14, *) ' Dlina podzemnogo kabelja # 2- ', ZR1, ' m'
        zr1 = rs2
        write(14, *) ' Radius podzemnogo kabelja # 2- ', zr1, ' m'
        zr1 = h2
        write(14, *) ' Glubina podzemnogo kabelja # 2- ', zr1, ' m'
        zr1 = al2
        write(14, *) ' Pogonnaja induktivnost kabelja #2- ', zr1, ' MkGn/m'
        zr2 = g2
        write(14, *) ' Pogonnaja provodimost kabelja #2- ', zr2, ' 1/Om m'
        ZR1 = RK2
        WRITE(14, *) ' Soprotivlenie v konce kabelja # 2- ', zr1, ' Om'
    end if

    zr1 = ro
    write(14, *) ' Udelnoe soprotivlenie grunta - ', zr1, ' Om m '
    ZR1 = TM
    ZR2 = DT
    WRITE(14, *) 'Vremja rascheta, shag rascheta', ZR1, ZR2, ' mks'
    zr1 = dd
    write(14, *) 'Prostranstvennyj shag rascheta = ', zr1, ' m'
    WRITE(14, *) '           ----------'
    write(14, *) '  '

    if(ncab.eq.2) then
        write(14, 300)
        write(14, 301)
        write(14, *) '  '
    else
        write(14, 320)
        write(14, 321)
        write(14, *) '  '
    end if

    ZR1 = AIM
    write(12, *) '  '
    write(12, *) '                                  .  .2018'
    WRITE(12, *) 'Raspredelenie toka molnii mezhdu podz. kommunikaciej,'
    WRITE(12, *) '                VL i zazemlitelem ob"ekta '
    WRITE(12, *) '                              Programma dart_uzip'
    write(12, *) '         (gL-priblizhenie LR- priblizhenie VL)'
    WRITE(12, *) '  '
    WRITE(12, *) ' Amplituda toka =', ZR1, ' kA'

    if(ntok.eq.1) then
        write(12, *) ' Impuls toka molnii 10/350 mks'
    end if
    if(ntok.eq.2) THEN
        write(12, *) ' Impuls toka molnii 0,25/100 mks'
    end if
    IF(NTOK.EQ.3) THEN
        write(12, *) ' Bijeksponencialnyj impuls toka'
        zr1 = 1. / at
        zr2 = 1. / bt
        write(12, *) ' Postojannye vremeni Tf, Timp = ', zr1, zr2, ' mks'
    end if

    zr1 = dl
    write(12, *) ' Dlina VL - ', zr1, ' m'
    zr1 = alw
    write(12, *) ' Polnaja induktivnost VL - ', zr1, ' mkGn'
    zr1 = dlo
    write(12, *) ' Udalenie tochki udara ot ob"ekta - ', zr1, ' m'
    zr1 = rn
    write(12, *) ' Soprotivlenie zazemlenija ob"ekta - ', zr1, ' Om'
    zr1 = rp
    write(12, *) ' Soprotivlenie zazemlenija PS - ', zr1, ' Om'

    ZR1 = DL1
    WRITE(12, *) ' Dlina podzemnogo kabelja # 1- ', ZR1, ' m'
    zr1 = rs1
    write(12, *) ' Radius podzemnogo kabelja # 1- ', zr1, ' m'
    zr1 = h1
    write(12, *) ' Glubina podzemnogo kabelja # 1- ', zr1, ' m'
    zr1 = al1
    write(12, *) ' Pogonnaja induktivnost kabelja #1- ', zr1, ' MkGn/m'
    zr2 = g1
    write(12, *) ' Pogonnaja provodimost kabelja #1- ', zr1, ' 1/Om m'
    ZR1 = RK1
    WRITE(12, *) ' Soprotivlenie v konce kabelja # 1- ', zr1, ' Om'

    if(ncab.eq.2) then
        ZR1 = DL2
        WRITE(12, *) ' Dlina podzemnogo kabelja # 2- ', ZR1, ' m'
        zr1 = rs2
        write(12, *) ' Radius podzemnogo kabelja # 2- ', zr1, ' m'
        zr1 = h2
        write(12, *) ' Glubina podzemnogo kabelja # 2- ', zr1, ' m'
        zr1 = al2
        write(12, *) ' Pogonnaja induktivnost kabelja #2- ', zr1, ' MkGn/m'
        zr2 = g2
        write(12, *) ' Pogonnaja provodimost kabelja #2- ', zr1, ' 1/Om m'
        ZR1 = RK2
        WRITE(12, *) ' Soprotivlenie v konce kabelja # 2- ', zr1, ' Om'
    end if

    zr1 = ro
    write(12, *) ' Udelnoe soprotivlenie grunta - ', zr1, ' Om m '
    ZR1 = TM
    ZR2 = DT
    WRITE(12, *) 'Vremja rascheta, shag rascheta', ZR1, ZR2, ' mks'
    zr1 = dd
    write(12, *) 'Prostranstvennyj shag rascheta = ', zr1, ' m'
    WRITE(12, *) '           ----------'
    write(12, *) '  '
    write(12, 310)
    write(12, 311)
    write(12, *) '  '

    TTT = TTT - 0.1 * DT
    TPP = TPP - 0.1 * DT

    N1 = (DD / 10. + DL1) / DD
    N2 = (dd / 10. + DL2) / dd
    if(n2.lt.1) n2 = 1

    NN = N1 + 1

    DO K = 1, NN
        AI1(K) = 0.
        AI2(K) = 0.
        U(K) = 0.
    end do

    if(ncab.eq.2) then
        nn2 = n2 + 1

        DO K = 1, NN2
            BI1(K) = 0.
            BI2(K) = 0.
            BU(K) = 0.
        end do
    end if

    tok1 = 0.
    tok2 = 0.
    tok3 = 0.
    tok4 = 0.
    tok5 = 0.
    tok10 = 0.
    tok20 = 0.
    tok30 = 0.
    tok40 = 0.
    tok50 = 0.

    AA1 = AL1 * DD / DT + RK1 + 1. / G1 / DD
    BB1 = 1. / DD / DD
    CC1 = AL1 * DD / DT
    DD1 = DT / G1 / AL1 / DD / DD
    FF1 = AL1 * DD / DT + RN + 1. / G1 / DD
    T = 0.
    TT = 0.
    TP = 0.
    TI = 0.
    tdet = 0.
    MT = (TM + DT / 10.) / DT


    if(ncab.eq.2) then
        AA2 = AL2 * DD / DT + RK2 + 1. / G2 / DD
        BB2 = 1. / DD / DD
        CC2 = AL2 * DD / DT
        DD2 = DT / G2 / AL2 / DD / DD
        FF2 = AL2 * DD / DT + RN + 1. / G2 / DD
    end if


    do l = 1, mt
        T = T + DT
        TT = TT + DT
        TP = TP + DT
        TI = TI + DT
        tdet = tdet + dt

        if(ntok.lt.3) then
            AI = F1(T)
        else
            ai = a00 * (dexp(-bt * t) - dexp(-at * t))
        end if

        N11 = N1 - 1
        DO K = 2, N11
            AI1(K) = (AI2(K + 1) - 2. * AI2(K) + AI2(K - 1)) * DD1 + AI2(K)
        END DO

        AI1(N1) = (AI1(N1 - 1) / G1 / DD + AI2(N1) * CC1) / AA1

        U(N1 + 1) = AI1(N1) * RK1
        N0 = N1 - 2
        DO K = N1, 2, -1
            U(K) = U(K + 1) + (AI1(K) - AI2(K)) * CC1
        END DO

        if(ncab.eq.2) then
            N21 = N2 - 1
            DO K = 2, N21
                BI1(K) = (BI2(K + 1) - 2. * BI2(K) + BI2(K - 1)) * DD2 + BI2(K)
            END DO

            BI1(N2) = (BI1(N2 - 1) / G2 / DD + BI2(N2) * CC2) / AA2

            BU(N2 + 1) = BI1(N2) * RK2
            N0 = N2 - 2
            DO K = N2, 2, -1
                BU(K) = BU(K + 1) + (BI1(K) - BI2(K)) * CC2
            END DO
        end if



        if(ncab.eq.2) then

            a(1, 1) = 1.
            a(1, 2) = 1.
            a(1, 3) = 0.
            a(1, 4) = 0.
            a(1, 5) = 0.
            a(1, 6) = ai
            a(2, 1) = 1.
            a(2, 2) = 0.
            A(2, 3) = -1.
            a(2, 4) = -1.
            a(2, 5) = -1.
            a(2, 6) = 0.
            a(3, 1) = -alw1 / dt
            a(3, 2) = alw2 / dt + rp
            a(3, 3) = -rn
            a(3, 4) = 0.
            a(3, 5) = 0.
            a(3, 6) = -alw1 / dt * tok10 + alw2 / dt * tok20
            a(4, 1) = 0.
            a(4, 2) = 0.
            a(4, 3) = -rn
            a(4, 4) = cc1
            a(4, 5) = 0.
            a(4, 6) = cc1 * tok40 - u(2)
            a(5, 1) = 0.
            a(5, 2) = 0.
            a(5, 3) = -rn
            a(5, 4) = 0.
            a(5, 5) = cc2
            a(5, 6) = cc2 * tok50 - bu(2)

        else

            a(1, 1) = 1.
            a(1, 2) = 1.
            a(1, 3) = 0.
            a(1, 4) = 0.
            a(1, 5) = ai
            a(2, 1) = 1.
            a(2, 2) = 0.
            A(2, 3) = -1.
            a(2, 4) = -1.
            a(2, 5) = 0.
            a(3, 1) = -alw1 / dt
            a(3, 2) = alw2 / dt + rp
            a(3, 3) = -rn
            a(3, 4) = 0.
            a(3, 5) = -alw1 / dt * tok10 + alw2 / dt * tok20
            a(4, 1) = 0.
            a(4, 2) = 0.
            a(4, 3) = -rn
            a(4, 4) = cc1
            a(4, 5) = cc1 * tok40 - u(2)

        end if

        CALL GAUSS

        if(ncab.eq.2) then
            tok1 = a(5, 1)
            tok2 = a(5, 2)
            tok3 = a(5, 3)
            tok4 = a(5, 4)
            tok5 = a(5, 5)

        else
            tok1 = a(4, 1)
            tok2 = a(4, 2)
            tok3 = a(4, 3)
            tok4 = a(4, 4)
        end if

        AI1(1) = TOK4
        U(1) = RN * TOK3

        if(ncab.eq.2) then
            BI1(1) = TOK5
            BU(1) = U(1)
        end if

        NN = N1 + 1
        DO K = 1, NN
            UU = AI2(K)
            AI2(K) = AI1(K)
            AI1(K) = UU
        END DO

        if(ncab.eq.2) then
            NN = N2 + 1
            DO K = 1, NN
                UU = BI2(K)
                BI2(K) = BI1(K)
                BI1(K) = UU
            END DO
        end if



        IF(TT.GE.TTT) THEN
            zr1 = t
            zr2 = AI1(N1)
            zr3 = U(N2 + 1)
            print*, 't = ', zr1, ' I = ', zr2, ' kA   U = ', zr3
            tt = 0
        end if

        300   FORMAT(1X, '   t mks   Im kA    Ikab1 kA  Ikab2 kA   Izem kA')

        301   format(1x, '--------------------------------------------------')
        302   format(1x, E8.3, 2x, E10.3, 1X, E10.3, 1x, E10.3, 1X, E10.3)

        310   FORMAT(1X, '   t mks   Im kA    Ivl1 kA  Ivl2 kA   Izem kA')

        311   format(1x, '--------------------------------------------------')
        312   format(1x, E8.3, 2x, E10.3, 1X, E10.3, 1x, E10.3, 1X, E10.3)

        320   FORMAT(1X, '   t mks   Im kA    Ikab1 kA  Izem kA')
        321   format(1x, '---------------------------------------------')
        322   format(1x, E8.3, 2x, E10.3, 1X, E10.3, 1x, E10.3)

        IF(TP.GE.TPP) THEN
            TP = 0.
            zr1 = t

            if(ncab.eq.2) then
                write(14, 302)zr1, ai, tok4, tok5, tok3
                write(12, 312)zr1, ai, tok1, tok2, tok3
            else
                write(14, 322)zr1, ai, tok4, tok3
                write(12, 312)zr1, ai, tok1, tok2, tok3
            end if
        end if

        TOK10 = TOK1
        TOK20 = TOK2
        TOK30 = TOK3
        TOK40 = TOK4
        TOK50 = TOK5

    end do

    return
END subroutine calc


SUBROUTINE GAUSS
    IMPLICIT REAL*8(A-H, O-Z)
    COMMON /U/ A, ncab

    DIMENSION A(6, 6)
    N = 5
    if(ncab.eq.1) n = 4
    NN = N+1
    N4 = N-1
    N5 = N+1
    DO 999 M =1, N4
        A1 = 0.
        DO I = M, N
            DO K = M, N
                A2 = ABS(A(I, K))
                IF(A1.LE.A2) THEN
                    A1 = A2
                    I1 = I
                    K1 = K
                END IF
            END DO
        END DO
        A2 = A(I1, K1)
        DO K = M, N5
            A1 =A(M, K)
            A(M, K)= A(I1, K)
            A(I1, K)= A1
            A(M, K)= A(M, K)/A2
        END DO
        DO I = M, N
            A1 = A(I, M)
            A(I, M)= A(I, K1)
            A(I, K1)= A1
        END DO
        A(M, 1)= K1+0.1
        M5 = M+1
        DO 999 I = M5, N
            A1= A(I, M)
            DO 999 K = M, N5
                A(I, K)= A(I, K)-A1*A(M, K)
            999   CONTINUE
    A(N, N)= A(N, N5)/A(N, N)
    DO J = 1, N4
        J1 =N-J
        X1 = A(J1, N5)
        DO L = 1, J
            L1 =N-L+1
            X1 = X1-A(N, L1)*A(J1, L1)
        END DO
        A(N, J1)= X1
        K1 = A(J1, 1)
        A2 = A(N, K1)
        A(N, K1)= X1
        A(N, J1)= A2
    END DO
    !    END DO
    RETURN
END subroutine gauss

end program dart_cgi
