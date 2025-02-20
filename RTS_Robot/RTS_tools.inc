#define PROJ_DIR "C:\EpsonRC70\Projects\DUNE\CE-RTS"
#define RTS_DATA "C:\Users\coldelec\RTS\"
'#define HAND_U0 7.075
'#define HAND_U0 4.174
#define NTRAYS 2
#define TRAY_NCOLS 15
#define TRAY_NROWS 6
#define N_LARASIC_SOC 8
#define N_COLDADC_SOC 8
#define N_COLDATA_SOC 2
#define NSOCKETS 18
'#define DF_CAMERA_OFFSET 58
'#define DF_CAMERA_FOCUS -18.045

Global String POINTS_FILE$
Global String SITE$
Global String CHIPTYPE$
Global Double HAND_U0
Global Double DF_CAMERA_OFFSET
Global Double DF_CAMERA_FOCUS

' corrections for the trays
Global Double tray_X(NTRAYS, TRAY_NCOLS, TRAY_NROWS)
Global Double tray_Y(NTRAYS, TRAY_NCOLS, TRAY_NROWS)
Global Double tray_U(NTRAYS, TRAY_NCOLS, TRAY_NROWS)

' corrections for the sockets
Global Double DAT_X(2, NSOCKETS)
Global Double DAT_Y(2, NSOCKETS)
Global Double DAT_U(2, NSOCKETS)

' chip SN
'Global String chip_SN$

