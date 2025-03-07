#include "RTS_tools.inc"



Function main
	
	SelectSite
	
	If Not FolderExists(RTS_DATA$) Then
  		MkDir RTS_DATA$
	EndIf

	If Not FolderExists(RTS_DATA$) Then
  		Print "***ERROR Can't create directory [" + RTS_DATA$ + "]"
  		Exit Function
	EndIf
	
	String dir_images$
	dir_images$ = RTS_DATA$ + "\images"

	If Not FolderExists(dir_images$) Then
  		MkDir dir_images$
	EndIf
	
	If Not FolderExists(dir_images$) Then
  		Print "***ERROR Can't create directory [" + dir_images$ + "]"
  		Exit Function
	EndIf
	
	String dir_pins$
	dir_pins$ = RTS_DATA$ + "pins"

	If Not FolderExists(dir_pins$) Then
  		MkDir dir_pins$
	EndIf
	
	If Not FolderExists(dir_pins$) Then
  		Print "***ERROR Can't create directory [" + dir_pins$ + "]"
  		Exit Function
	EndIf
	
	
	' reset arrays	JW: Use TRAY_NCOLS and TRAY_NROWS as these are maximum array sizes, even if we only fill trayNCols x trayNRows
	Integer i, j, k
	For i = 1 To NTRAYS
		For j = 1 To TRAY_NCOLS
			For k = 1 To TRAY_NROWS
				tray_X(i, j, k) = 0
				tray_Y(i, j, k) = 0
				tray_U(i, j, k) = 0
			Next k
		Next j
	Next i
	
	For i = 1 To 2
		For j = 1 To NSOCKETS
			DAT_X(i, j) = 0
			DAT_Y(i, j) = 0
			DAT_U(i, j) = 0
		Next j
	Next i
	
		
		
	
	' Initialize positions	
	'tray_X(1, 15, 6) = -0.300
	'tray_Y(1, 15, 6) = -0.300
	'tray_U(1, 15, 6) = -0.2
	
	' load positions at camera of chips coming from trays
    Integer fileNum
    String fileName$
    Double x, y, u
    Double ii, jj, kk
	fileNum = FreeFile
	fileName$ = RTS_DATA$ + "\tray_xyu.csv"
	If FileExists(fileName$) Then
		Print "Reading file ", fileName$
		ROpen fileName$ As #fileNum
		For i = 1 To NTRAYS
			For j = 1 To TRAY_NCOLS
				For k = 1 To TRAY_NROWS
					Input #fileNum, ii, jj, kk, x, y, u
					If ii = i And jj = j And kk = k Then
						'Input #fileNum, tray_X(i, j, k), tray_Y(i, j, k), tray_U(i, j, k)
						tray_X(i, j, k) = x
						tray_Y(i, j, k) = y
						tray_U(i, j, k) = u
					Else
						Print "Error reading file ", fileName$, " ijk=", i, " ", j, " ", k
						Exit Function
					EndIf
				Next k
			Next j
		Next i
	EndIf
	Close #fileNum
	
	' load positions at camera of chips coming from sockets
	fileNum = FreeFile
	fileName$ = RTS_DATA$ + "\socket_xyu.csv"
	If FileExists(fileName$) Then
		Print "Reading file ", fileName$
		ROpen fileName$ As #fileNum
		For i = 1 To 2
			For j = 1 To NSOCKETS
				Input #fileNum, ii, jj, x, y, u
				If ii = i And jj = j Then
					DAT_X(i, j) = x
					DAT_Y(i, j) = y
					DAT_U(i, j) = u
				Else
					Print "Error reading file ", fileName$
					Exit Function
				EndIf
			Next j
		Next i
	EndIf
	Close #fileNum
		
	VacuumValveClose
	PumpOn
	Wait 3

	Motor On
	Power Low

	SetSpeed
	
	' left tray
	Pallet 1, Tray_Left_P1, Tray_Left_P2, Tray_Left_P3, Tray_Left_P4, trayNCols, trayNRows

	' right tray
	Pallet 2, Tray_Right_P1, Tray_Right_P2, Tray_Right_P3, Tray_Right_P4, trayNCols, trayNRows

    'JumpToCamera
	'Call RTS_server

	'JumpToTray(2, 15, 6)
	'PickupFromTray
	'JumpToCamera

	'MoveChipFromSocketToTray(1, 1, 1, 15, 2)
	MoveChipFromTrayToSocket(1, 15, 2, 1, 1)
	
	
	'MoveChipFromTrayToSocket(2, 5, 1, 2, 2)
	
	'MoveChipFromTrayToSocket(2, 1, 1, 1, 8)
	'MoveChipFromTrayToSocket(2, 2, 1, 1, 7)
	'MoveChipFromTrayToSocket(2, 3, 1, 1, 6)
	'MoveChipFromTrayToSocket(2, 4, 1, 1, 5)
	'MoveChipFromTrayToSocket(2, 5, 1, 1, 4)
	'MoveChipFromTrayToSocket(2, 6, 1, 1, 3)
	'MoveChipFromTrayToSocket(2, 7, 1, 1, 2)
	'MoveChipFromTrayToSocket(2, 8, 1, 1, 1)

	'MoveChipFromTrayToSocket(2, 11, 6, 2, 2)
	'MoveChipFromSocketToTray(2, 2, 2, 5, 1)
	'MoveChipFromSocketToTray(1, 2, 2, 4, 1)
	'MoveChipFromTrayToSocket(1, 1, 1, 1, 1)
	'MoveChipFromSocketToTray(1, 1, 1, 1, 1)
	'MoveChipFromTrayToSocket(1, 1, 1, 2, 2)
	'MoveChipFromSocketToTray(2, 2, 1, 1, 1)
	'MoveChipFromSocketToTray(2, 2, 2, 5, 1)
	'MoveChipFromSocketToTray(1, 6, 2, 2, 1)
	'MoveChipFromSocketToTray(1, 5, 2, 3, 1)
	'MoveChipFromSocketToTray(1, 4, 2, 4, 1)
	'MoveChipFromSocketToTray(1, 3, 2, 5, 1)
	'MoveChipFromSocketToTray(1, 2, 2, 6, 1)
	'MoveChipFromSocketToTray(1, 1, 2, 7, 1)
	'MoveChipFromSocketToTray(2, 6, 2, 3, 6)
	'MoveChipFromSocketToTray(2, 5, 2, 2, 6)
	'MoveChipFromSocketToTray(2, 4, 2, 1, 6)
	'MoveChipFromSocketToTray(2, 3, 2, 15, 5)
	'MoveChipFromSocketToTray(2, 2, 2, 14, 5)
	'MoveChipFromSocketToTray(2, 1, 2, 13, 5)
	'MoveChipFromSocketToTray(2, 1, 2, 15, 6)
	'MoveChipFromTrayToTray(2, 15, 6, 2, 15, 6, 0)
	'MoveChipFromTrayToTray(2, 15, 1, 2, 15, 1, 0)
	'MoveChipFromTrayToTray(2, 15, 6, 2, 15, 6, -270)
	'MoveChipFromSocketToTray(2, 1, 2, 8, 6)
	'MoveChipFromSocketToTray(2, 2, 2, 9, 6)
	'MoveChipFromSocketToTray(2, 3, 2, 10, 6)
	'MoveChipFromSocketToTray(2, 4, 2, 11, 6)
	'MoveChipFromSocketToTray(2, 5, 2, 12, 6)
	'MoveChipFromSocketToTray(2, 6, 2, 13, 6)
	'MoveChipFromSocketToTray(2, 7, 2, 14, 6)
	'MoveChipFromSocketToTray(2, 8, 2, 15, 6)
	'MoveChipFromTrayToTray(2, 1, 2, 1, 1, 2, 0)
	'MoveChipFromTrayToTray(1, 1, 2, 2, 1, 2, 0)
    'MoveChipFromTrayToTray(1, 15, 6, 1, 15, 6, 0)
    'MoveChipFromTrayToTray(1, 15, 6, 2, 15, 6, 0)
    'MoveChipFromTrayToTray(2, 15, 6, 1, 15, 6, 0)
    'MoveChipFromTrayToTray(1, 1, 5, 1, 1, 5, 0)
	
	'MoveChipFromTrayToSocket(2, 15, 6, 2, 1)
	'MoveChipFromSocketToTray(2, 1, 2, 15, 6)
	'MoveChipFromSocketToTray(2, 1, 1, 15, 6)
	'MoveChipFromTrayToSocket(1, 15, 6, 2, 1)
	'MoveChipFromSocketToTray(2, 3, 2, 3, 1)
	'MoveChipFromTrayToSocket(2, 3, 1, 2, 3)

	'MoveChipFromTrayToSocket(2, 1, 1, 2, 1)

	'PumpOff
		
	'UF_Calib_Stinger
	'UF_Calib_Stinger_vs_U
	'DF_Calib_Mark
	'Find_Socket(7)
	'View_Socket_R(1)
	
    
    'Motor Off
    
	' save positions at camera of chips coming from trays
	fileNum = FreeFile
	fileName$ = RTS_DATA$ + "\tray_xyu.csv"
	WOpen fileName$ As #fileNum
	For i = 1 To NTRAYS
		For j = 1 To TRAY_NCOLS
			For k = 1 To TRAY_NROWS
				Print #fileNum, i, ",", j, ",", k, ",",
				Print #fileNum, tray_X(i, j, k), ",", tray_Y(i, j, k), ",", tray_U(i, j, k)
			Next k
		Next j
	Next i
	Close #fileNum
    
	' save positions at camera of chips coming from sockets
	fileNum = FreeFile
	fileName$ = RTS_DATA$ + "\socket_xyu.csv"
	WOpen fileName$ As #fileNum
	For i = 1 To 2
		For j = 1 To NSOCKETS
			Print #fileNum, i, ",", j, ",",
			Print #fileNum, DAT_X(i, j), ",", DAT_Y(i, j), ",", DAT_U(i, j)
		Next j
	Next i
	Close #fileNum
	
Fend




Function TrayTakePlaceRepeat(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, ncycles As Integer)

	SetSpeed
	
	Integer i
	For i = 1 To ncycles
		
		Print "Cycle ", i, "/", ncycles
		
		Int64 status
		status = MoveChipFromTrayToTray(pallet_nr, col_nr, row_nr, pallet_nr, col_nr, row_nr, 0)
		If status < 0 Then
			Print "***ERROR!"
			Exit For
		EndIf
	Next i

Fend








	

Function SocketTakePlaceRepeat(DAT_nr As Integer, socket_nr As Integer, ncycles As Integer)
	
	'Integer pallet_nr, row_nr, col_nr
	'pallet_nr = 1
	'row_nr = 6
	'col_nr = 15

	SetSpeed
	
	'String chip_SN$
	'chip_SN$ = "002-06335"
	
	String fname$
	fname$ = "DAT_" + FmtStr$(DAT_nr, "0") + "_socket_" + "-" + FmtStr$(socket_nr, "0") + "_BottomAna.csv"
	
	Integer fileNum
	fileNum = FreeFile
	AOpen RTS_DATA$ + "\" + fname$ As #fileNum
		
	Integer i
	For i = 1 To ncycles
	
		Print "Cycle ", i, "/", ncycles

		String d$, t$
	 	d$ = Date$
	 	t$ = Time$
        Print #fileNum, d$, " ", t$,


		' Take picture of chip in the socket
		JumpToSocket_camera(DAT_nr, socket_nr)
		' Jump Socket_R_1 :Z(-97.60) +X(58.0) -U(45)
		' DF_take_picture_socket("002-06377", 1)
		String pict_fname_socket$
		'DF_take_picture_socket(chip_SN$, socket_nr, ByRef pict_fname_socket$)
		'DF_take_picture_socket(socket_nr, ByRef pict_fname_socket$)
		Print #fileNum, ",", pict_fname_socket$,
		
		' Pickup from socket
		JumpToSocket(DAT_nr, socket_nr)
		'Go Here +U(45)
		PickupFromSocket
			
		' Take picture of the bottom of the chip
		JumpToCamera
		'Go Here +U(45)
		UF_camera_light_ON
		Wait 1
		String pict_fname_0$
		'UF_take_picture(chip_SN$, ByRef pict_fname_0$)
		'UF_take_picture(ByRef pict_fname_0$)
		'UF_camera_light_OFF
		Print #fileNum, ",", pict_fname_0$,
		
		'JumpToTray(pallet_nr, col_nr, row_nr)
		'PickupFromTray

		' Take picture of the bottom of the chip
		'JumpToCamera
		'UF_camera_light_ON
		'Wait 1
		'UF_take_picture("002-06377")
	
		'Double tray_dx, tray_dy, tray_dU
		VRun ChipBottom_Analy
	
		Boolean ret_found
		Double camera_X, camera_Y
		Double X_0, Y_0, U_0
		Double X_180, Y_180, U_180
		Double X_tool, Y_tool

		'VGet ChipBottom_Analy.Final.RobotXYU, ret_found, ret_X, ret_Y, ret_U
		VGet ChipBottom_Analy.Final.Found, ret_found
		If ret_found Then

			VGet ChipBottom_Analy.CameraCenter.CameraX, camera_X
			VGet ChipBottom_Analy.CameraCenter.CameraY, camera_Y
			
			VGet ChipBottom_Analy.Final.CameraX, X_0
			VGet ChipBottom_Analy.Final.CameraY, Y_0
			VGet ChipBottom_Analy.Final.Angle, U_0

            Print #fileNum, ",", ret_found,
            Print #fileNum, ",", camera_X, ",", camera_Y,
            Print #fileNum, ",", X_0, ",", Y_0, ",", U_0,
            'Print #fileNum, ",", camera_X - ret_X, ",", camera_Y - ret_Y

		Else
			
			Print "ERROR 1"
	        Exit For
			
		EndIf


		' Repeat measurement for 180 deg. rotation
		Go Here +U(180)
		Wait 1
		String pict_fname_180$
		'UF_take_picture(chip_SN$ + "-180", ByRef pict_fname_180$)
		'UF_take_picture(ByRef pict_fname_180$)
		Print #fileNum, ",", pict_fname_180$,

		VRun ChipBottom_Analy
	
		VGet ChipBottom_Analy.Final.Found, ret_found
		If ret_found Then

			VGet ChipBottom_Analy.CameraCenter.CameraX, camera_X
			VGet ChipBottom_Analy.CameraCenter.CameraY, camera_Y
			
			VGet ChipBottom_Analy.Final.CameraX, X_180
			VGet ChipBottom_Analy.Final.CameraY, Y_180
			VGet ChipBottom_Analy.Final.Angle, U_180

            Print #fileNum, ",", ret_found,
            Print #fileNum, ",", camera_X, ",", camera_Y,
            Print #fileNum, ",", X_180, ",", Y_180, ",", U_180,
            'Print #fileNum, ",", camera_X - ret_X, ",", camera_Y - ret_Y
		Else
			
			Print "ERROR 2"
	        Exit For
			
		EndIf

		' First correct the rotation
		Go Here -U(180)
		Wait 1

		Double d_U
		d_U = U_0 + 0.6
		If Abs(d_U) < 2.0 Then
			Go Here +U(d_U)
		Else
			Print "ERROR! Rotation angle outside of control margin"
			Exit For
		EndIf


		' Remeasure X and Y with correct rotation
		Wait 1
		'UF_take_picture(chip_SN$, ByRef pict_fname_0$)
		'UF_take_picture(ByRef pict_fname_0$)
        Print #fileNum, ",", pict_fname_0$,
		
		VRun ChipBottom_Analy
		
		VGet ChipBottom_Analy.Final.Found, ret_found
		If ret_found Then
			
			VGet ChipBottom_Analy.Final.CameraX, X_0
			VGet ChipBottom_Analy.Final.CameraY, Y_0
			VGet ChipBottom_Analy.Final.Angle, U_0

            Print #fileNum, ",", ret_found,
            Print #fileNum, ",", X_0, ",", Y_0, ",", U_0,
            'Print #fileNum, ",", camera_X - ret_X, ",", camera_Y - ret_Y

		Else
			
			Print "ERROR 3"
	        Exit For
			
		EndIf



	
		' Return to tray
		'JumpToTray(pallet_nr, col_nr, row_nr)
		'DropToTray
	
		UF_camera_light_OFF
	
		' Put chip back into socket			
		JumpToSocket(DAT_nr, socket_nr)
		Wait 1
		' correct position
		Double d_X, d_Y
		d_X = 0.5 * (X_0 + X_180) - X_0 - 0.227
		d_Y = 0.5 * (Y_0 + Y_180) - Y_0 - 0.141
		'd_U = U_0 + 0.6
		Print #fileNum, ",", d_X, ",", d_Y, ",", d_U
		Print "Correcting chip position: ",
		Print "dX = ", d_X,
		Print "dY = ", d_Y,
		Print "dU = ", d_U
		If Abs(d_X) < 1.0 And Abs(d_Y) < 1.0 And Abs(d_U) < 2.0 Then
			Go Here +X(d_X) +Y(d_Y) +U(d_U)
			InsertIntoSocket
		Else
			Print "ERROR 4"
	        Exit For
		EndIf

	Next i
	
	' Return to tray
	'JumpToTray(1, 15, 6)
	'DropToTray
	
	
	Close #fileNum

	'JumpToSocket("R", 1)
	'InsertIntoSocket

	' Take picture of chip in the socket
	'Jump Socket_R_1 :Z(-97.60) +X(58.0) -U(45)
	'DF_take_picture("002-06377", 1)

	'JumpToTray(1, 15, 6)


	' Pickup from socket
	'JumpToSocket("R", 1)
	'PickupFromSocket

	' Take picture of the bottom of the chip
	'JumpToCamera
	'UF_camera_light_ON
	'Wait 1
	'UF_take_picture("002-06377")
	'UF_camera_light_OFF



Fend


Function TakePlaceRepeat(pallet_nr As Integer, col_nr As Integer, row_nr As Integer, DAT_nr As Integer, socket_nr As Integer, ncycles As Integer)
	
	Integer i
	For i = 1 To ncycles
		
		Print "Cycle ", i, "/", ncycles
		
		Int64 status
		status = MoveChipFromTrayToSocket(pallet_nr, col_nr, row_nr, DAT_nr, socket_nr)

		If status < 0 Then
			Print "***ERROR!"
			Exit For
		EndIf

		status = MoveChipFromSocketToTray(DAT_nr, socket_nr, pallet_nr, col_nr, row_nr)

		If status < 0 Then
			Print "***ERROR!"
			Exit For
		EndIf
	
			
		
	Next i

Fend



