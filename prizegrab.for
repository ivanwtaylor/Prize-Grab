
      Integer pick
      character*20 pickString
      Integer turns
      Integer rounds
      Integer square
      Integer backIn
      Integer dice
      Integer Xs
      Integer scratchers 
      Integer scratchWin(4) 
      Integer i 
      Integer XSquare 
      Integer diceBox
      Integer xCount(4) 
      character*20 box(4)
      character*1 boxNumber
      character*1 X1Number
      character*2 X2Number
      character*1 dice1Number
      character*2 dice2Number
      character*1 pickNumber
      character*1 newPickNumber
      character*1 scratchNumber
      character*20 boxName(4)
      Integer run 
      Integer diceWin(4) 
      Integer winner 
      Integer keepPlaying 
      Integer newPick 
      Integer answer 
      character*1 play  
      INTEGER :: count, count_rate, count_max
        
      dice = 100
      Xs = 0
      turns = 0
      scratchers = 0
      do 10 i = 1, 4
        xCount(i) = 0
        diceWin(i) = 0
        scratchWin(i) = 0
   10 continue
      run = 0
      turns = 1
      play = "y"
      CALL SYSTEM_CLOCK(count, count_rate, count_max)
      CALL srand(count) 
      print*, " WELCOME TO PRIZE GRAB"
   25 If (play .eq. "n" .or. play .eq. "N" .or.
     1   turns .gt. 40) goto 155
   35    If (mod(turns,5) .eq. 0) goto 135
            If (turns .gt. 1) then
               If (turns .lt. 10) Then
                  If (Xs .eq. 1) Then
                     write(*,1010) Xs,turns 
1010                 format("You have",i2," X and you are on turn",
     1                   i2)
                  Elseif (Xs .lt. 10) then
                     write(*,1011) Xs,turns
1011                 format("You have",i2," Xs and you are on turn",
     1                    i2) 
                  Else	
                     write(*,1012) Xs,turns
1012                 format("You have",i3," Xs and you are on turn",
     1                    i2) 
                  Endif
               Else
                  If (Xs .eq. 1) then
                     write(*,1014) Xs,turns 
1014                 format("You have",i2," X and you are on turn",
     1                    i3)
                  Elseif (Xs .lt. 10) then
                     write(*,1015) Xs,turns
1015                 format("You have",i2," Xs and you are on turn",
     1                    i3) 
                  Else	
                     write(*,1016) Xs,turns
1016                 format("You have",i3," Xs and you are on turn",
     1                    i3) 
                  Endif
               Endif
            End if
            if (turns .gt. 1) then
               print*, "Do you want to continue?",
     1              " (y for yes, n for no)"
               read(*,*) play
            endif
            If (play .eq. "n" .or. play .eq. "N") Then
               stop
            End If
            pick = 0
   45       if (pick .ge. 1 .and. pick .le. 4) goto 105
               print*, "Pick a box from 1 to 4."
               read(*,*) pick
               WRITE ( pickNumber( 1:1 ), '(I1)') pick
               pickString = "  " // pickNumber
               goto 45
  105       continue
            Do 20 i = 1, 4
               WRITE ( boxNumber( 1:1 ), '(I1)') i
               If (Rand(0) .lt. 0.5) Then
                  scratchWin(i) = 1
                  box(i)     = " 1-scratcher"
                  boxName(i) = "Box " // boxNumber  
               Else
                  scratchWin(i) = 2
                  box(i) =     " 2-scratchers"
                  boxName(i) = "Box " // boxNumber 
               End If
               diceWin(i) = 0
   20       Continue 
            If (turns .gt. 1) Then
               XSquare = Int(Rand(0) * 4) + 1
               WRITE ( boxNumber( 1:1 ), '(I1)') XSquare
               boxName(XSquare) ="Box " // boxNumber
               xCount(XSquare) = xCount(XSquare) + 1
               If (xCount(Xsquare) .lt. 10) then
                  WRITE ( X1Number( 1:1 ), '(I1)') xCount(Xsquare)
                  If (xCount(XSquare) .eq. 1) Then
                     box(XSquare) = " " // X1Number // "-X"
                  Else
                     box(XSquare) = " " // X1Number // "-Xs"
                  Endif
               Else
                  WRITE ( X2Number( 1:2 ), '(I2)') xCount(Xsquare) 
                  box(XSquare) = " " // X2Number // "-Xs"
               Endif				
               diceBox = XSquare
   55          If (diceBox .ne. XSquare) goto 115
                  diceBox = Int(Rand(0) * 4) + 1
                  goto 55
  115          Continue
               WRITE ( boxNumber( 1:1 ), '(I1)') diceBox
               diceWin(diceBox) = Int(Rand(0) * 3) + 1
               WRITE ( dice1Number( 1:1 ), '(I1)') diceWin(diceBox)
               box(diceBox) = " " // dice1Number // "-dice"
               boxName(diceBox) = "Box " // boxNumber
               If (XSquare .eq. pick) Then
                  run = 0
                  Xs = Xs + 1
                  print*, "You hit an X."
                  If (turns .lt. 10) then
                     If (Xs .eq. 1) Then
                        write(*,1030) Xs, turns
1030                    format("You have",i2," X on turn",i2)
                     Elseif (Xs .gt. 1 .and. Xs .lt. 10) then
                        write(*,1031) Xs,turns
1031                    format("You have",i2," Xs on turn",i2)
                     Else
                        write(*,1032) Xs, turns
1032                    format("You have",i3," X on turn",i2)
                     Endif
                  Else
                     If (Xs .eq. 1) then
                        write(*,1033) Xs, turns
1033                    format("You have",i2," X on turn",i3)
                     Elseif (Xs .gt. 10 .and. Xs .lt. 10) then
                        write(*,1034) Xs, turns
1034                    format("You have",i2," Xs on turn",i3)
                     Else
                        write(*,1035) Xs, turns
1035                    format("You have",i3," X on turn",i3)
                     Endif
                  Endif
                  If (Xs .eq. 1) Then
                     backIn = 2
                  ElseIf (Xs .eq. 2) Then
                     backIn = 8
                  ElseIf (Xs .eq. 3) Then
                     backIn = 16
                  ElseIf (Xs .eq. 4) Then
                     backIn = 28
                  ElseIf (Xs .ge. 5) Then
                     backIn = 28 + Int(Rand(0) * 20)
                  End If
                  If (backIn .lt. 10) then
                     write(*,1036) backIn
1036                 format("Do you want to spend",i2, 
     1                  " dice to get back in?",
     1                  " (y for yes, n for no)")
                  Else
                     write(*,1037) backIn
1037                 format("Do you want to spend",i3, 
     1                  " dice to get back in?",
     1                  " (y for yes, n for no)")
                  Endif
                  read(*,*) play
                  If (play .eq. "n" .or. play .eq. "N") Then
                     stop
                  End If
                  dice = dice - backIn
                  newPick = pick
   65             If (newPick .ne. pick .and. 
     1               newPick .ge. 1 .and. 
     1               newPick .le. 4) goto 125
                     write(*,1060) pick
1060                 format("You picked box",i2," and got an X.") 
                     print*, "Pick another box."
                     read(*,*) newpick
                     goto 65
  125             continue
                  WRITE ( pickNumber( 1:1 ), '(I1)') pick
                  WRITE ( newPickNumber( 1:1 ), '(I1)') newPick
                  pickString = pickNumber // "-" // newPickNumber
                  If (diceWin(newPick) .gt. 0) Then
                     write(*,1070) diceWin(newpick)                        
1070                 format("You won",i2, " dice.")
                     dice = dice + diceWin(newPick)
                  Else
                     If (scratchWin(newPick) .eq. 1) Then
                        write(*,1080) scratchWin(newPick)                            
1080                    format ("You won",i2," scratcher.")
                     Else
                        write(*,1090) scratchWin(newPick)                                                       
1090                    format ("You won",i2," scratchers.")
                     End If
                     scratchers = scratchers + scratchWin(newPick)
                  End If
                  pick = newPick
               Else
                  run = run + 1
                  If (diceWin(pick) .gt. 0) Then
                     dice = dice + diceWin(pick)
                     write(*,1100) diceWin(pick)
1100                 format("You won",i2, " dice.")
                  Else
                     scratchers = scratchers + scratchWin(pick)
                     If (scratchWin(pick) .eq. 1) Then
                        write(*,1080) scratchWin(pick)
                     Else
                        write(*,1090) scratchWin(pick)
                     End If
                  End If
                  write(*,1110) XSquare
1110              format("The X was in box",i2,".")
               Endif
            Else
               If (scratchWin(pick) .eq. 1) Then
                  write(*,1080) scratchWin(pick) 
                  scratchers = scratchers + scratchWin(pick)
               Else
                  write(*,1090) scratchWin(pick)
                  scratchers = scratchers + scratchWin(pick)
               End If
               run = 1
            Endif
            write(*,999) (boxName(i), i=1,4)
999         format(" Turn Dice Scratchers Winnings Your Pick     ",
     1         a13,3(" ",a13),"  Last X")
            write(*,1000) turns, dice, scratchers, diceWin(pick), 
     1         pickString, (box(i), i=1,4), run
1000        format(i4,i6,i6,i10,"        ",a3,"       ",a13,
     1         3(" ",a13),i7)
            turns = turns + 1
            goto 35
 135     Continue
         If (play .eq. "n" .or. play .eq. "N") Then
            stop
         End If
         print*, "YOU ARE AT A GIFT BOX."
         Do 30 i = 1, 4
            WRITE ( boxNumber( 1:1 ), '(I1)') i
            boxName(i) = "Box " // boxNumber 
            scratchWin(i) = Int(Rand(0) * 2) + 1
            WRITE ( scratchNumber( 1:1 ), '(I1)') scratchWin(i)
            If (scratchWin(i) .eq. 1) then
               box(i) =      " " // scratchNumber // "-scratcher"
            Else
               box(i) =      " " // scratchNumber // "-scratchers"
            Endif
            diceWin(i) = 0
  30     Continue
         diceBox = Int(Rand(0) * 4) + 1
         diceWin(diceBox) = Int(Rand(0) * 10) + turns / 2
         WRITE ( dice2Number( 1:2 ), '(I2)') diceWin(diceBox)
         WRITE ( boxNumber( 1:1 ), '(I1)') diceBox
         box(diceBox) = dice2Number // "-dice"
         boxName(diceBox) = "Box " // boxNumber 
         pick = 0
 75      If (pick .ge. 1 .and. pick .le. 4) goto 145
            print*, "Pick a gift box 1 to 4"
            read(*,*) pick
            WRITE ( pickNumber( 1:1 ), '(I1)') pick
            pickString = "  " // pickNumber
            goto 75
 145     Continue
         If (diceWin(pick) .eq. 0) Then
            If (scratchWin(pick) .eq. 1) Then
               write(*,1080) scratchWin(pick)
            Else
               write(*,1090) scratchWin(pick)
            End If
            scratchers = scratchers + scratchWin(pick)
            Do 40 i = 1, 4
               If (diceWin(i) .gt. 0 .and. diceWin(i) .lt. 10) Then
                  write(*,1120) diceWin(i),i                    
1120              format("There were",i2," dice in box",i2,".")
               Elseif (diceWin(i) .ge. 10) then
                  write(*,1121) diceWin(i),i
1121              format("There were",i3," dice in box",i2,".")                  
               End If
 40         Continue
         Else
            If (diceWin(pick) .lt. 10) then
               write(*,1070) diceWin(pick)
            Else
               write(*,1071) diceWin(pick)
1071           format("You won",i3," dice.")
            Endif
            dice = dice + diceWin(pick)
         End If
         write(*,999) (boxName(i), i=1,4)
         write(*,1000) turns, dice, scratchers, diceWin(pick), 
     1        pickString, (box(i), i = 1,4)
         turns = turns + 1
         goto 25
  155 Continue
            
      Stop
      End

