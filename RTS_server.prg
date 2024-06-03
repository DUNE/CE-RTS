
Function RTS_server
	Integer portNr
	portNr = 201
	String msg$
	SetNet #portNr, "127.0.0.1", 2001, CRLF, NONE, 0, TCP, 5
	Print "Starting RTS server..."
	OpenNet #portNr As Server
	Print "Waiting connection to port ", portNr
	WaitNet #portNr
 	'Print #portNr, "Data from host 1"
 	Print "Connection established"
	Print "Sending data to client"
    Print #portNr, "RTS ready"
    
    Int64 status
    Integer pallet_nr, pallet_col, pallet_row, DAT_nr, socket_nr
    
 	Do
    	Input #portNr, msg$
    	'Line Input #portNr, msg$
    	Print "Received reply: ", msg$
    	Select msg$

    		Case "MoveChipFromTrayToSocket"
    			PumpOn
    			' Receive source and destination parameters"
    			Input #portNr, pallet_nr
    			Input #portNr, pallet_col
    			Input #portNr, pallet_row
    			Input #portNr, DAT_nr
    			Input #portNr, socket_nr
    			Print "Move chip from pallet(", pallet_nr, ",", pallet_col, ",", pallet_row, ")",
    			Print " to DAT board: ", DAT_nr, " socket", socket_nr
    			'Jump Pallet(1, 15, 6) :Z(-10)
    			'DO stuff
    			status = MoveChipFromTrayToSocket(pallet_nr, pallet_col, pallet_row, DAT_nr, socket_nr)
    			Print #portNr, Str$(status)

    		Case "MoveChipFromSocketToTray"
    			PumpOn
    			' Receive source and destination parameters"
    			Input #portNr, DAT_nr
    			Input #portNr, socket_nr
    			Input #portNr, pallet_nr
    			Input #portNr, pallet_col
    			Input #portNr, pallet_row
    			Print "Move chip from DAT board: ", DAT_nr, " socket", socket_nr
    			Print " to tray(", pallet_nr, ",", pallet_col, ",", pallet_row, ")",
    			'Jump Pallet(1, 15, 6) :Z(-10)
    			'DO stuff
    			status = MoveChipFromSocketToTray(DAT_nr, socket_nr, pallet_nr, pallet_col, pallet_row)
    			Print #portNr, Str$(status)

    		Case "PumpOff"
    			'Jump Pallet(1, 12, 6) :Z(-10)
    			'Print("Not implemented")
    			PumpOff

    		Case "Exit"
		    	CloseNet #portNr
		    	PumpOff
	    		OpenNet #portNr As Server
				Print "Waiting connection to port ", portNr
				WaitNet #portNr
 				'Print #portNr, "Data from host 1"
 				Print "Connection established"
				Print "Sending data to client"
    			Print #portNr, "RTS ready"

    		Case "Shutdown"
		    	CloseNet #portNr
		    	PumpOff
		    	Motor Off
		    	Exit Do
    	Send
	Loop
Fend

