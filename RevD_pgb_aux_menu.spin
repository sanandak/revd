'**************************************************************
'* RevD_pebble_code.spin
'* Main and top level program for operating the RevC board.
'*   built using the different test codes developed for RevC
'*
'* PGB
'* 3 October 2014 - based on RevC_pebble_code
'* 3 December 2014 - update code to include OLED stomp test stuff
'*                   this required updating and changing the way some
'*                   of the BasicPebbleFuncttions were written.
'*                   new version of that is called BasicPebbleFunctionsStomp
'*                   once it's all updated and working things can be moved
'*                   back to simpler names.  I just want to be sure to keep
'*                   a working copy.  
'*
'**************************************************************
CON ' Clock mode settings
  _CLKMODE = XTAL1 + PLL16X
  _XINFREQ = 6_250_000

  FULL_SPEED  = ((_clkmode - xtal1) >> 6) * _xinfreq               ' system freq as a constant
  ONE_MS      = FULL_SPEED / 1_000                                   ' ticks in 1ms
  ONE_US      = FULL_SPEED / 1_000_000                               ' ticks in 1us
  SLEEP_SPEED = _XINFREQ
                                 
CON ' VERSION
  VERSION = $01 ' this is a single byte    
                               
CON ' Pin map
  EXT_OSC       =  0
  GPS_PPS       =  1
  GPS_RX_FROM   =  2
  GPS_TX_TO     =  3
  SPARE         =  4 ' use spare pin as test point
  ADC_DRDYOUT   =  5
  ADC_MISO      =  6 ' connected to DOUT on MAX11040K
  ADC_MOSI      =  7 ' connected to DIN  on MAX11040K
  ADC_SCLK      =  8
  ADC_CS        =  9
  SRAM_CS       = 10
  SRAM_MOSI     = 11
  SRAM_MISO     = 12
  SRAM_CLK      = 13
  WAKEUP        = 14
  REED_SWITCH   = 15

  SLAVE_SCLK    = 16 
  SLAVE_SIMO    = 17 
  SLAVE_SOMI    = 18 
  SLAVE_CS      = 19 
  SLAVE_IRQ     = 20 

  SHARED_CLK    = 21
  SHARED_MOSI   = 22
  SHARED_MISO   = 23
  OLED_CS       = 24
  DAC_CS        = 25
  RADIO_CS      = 26
  ONE_WIRE      = 27  

  SCL           = 28
  SDA           = 29
  
  DEBUG_TX_TO   = 30
  DEBUG_RX_FROM = 31
  


  
CON ' GPS constants including bit on I2C expander
  PERIOD_LENGTH     = 16  ' number of seconds over which to collect oscillator data
  BYTES_IN_GPS_BUFFER = 1024
  GPSDO_REF         = 20_000_000  

CON ' Uart command constants
    KILL                  = "K"
    SHUTDOWN_SYSTEM       = "D"
    SOFT_REBOOT_GUMSTIX   = "S"
    HARD_REBOOT_GUMSTIX   = "H"
    PUT_SYSTEM_TO_SLEEP   = "L"
    RATE                  = "R"
    SHORT                 = "T"
    TRIG_INTERVAL         = "I"
    QUERY                 = "Q"
    GAIN                  = "G"
    SOURCE                = "O"
    STOMP_DATA            = "W"
    LINE_ONE              = "1"
    LINE_TWO              = "2"
    EUI                   = "E"

    MAX_COMMANDS      =     15

  #0, WAITING_FOR_START, WAITING_FOR_END, PROCESS_BUFFER
  
CON ' ON/OFF switch constants
  NOT_PRESSED       = 1         ' Reed switch is pulled high normally 
  PRESSED           = 0         ' Pressed is actually logic 0  

CON ' ADC constants
  
  
  WSCIR   = %0100_0000  ' Write Sampling Instant Control Register
  RSICR   = %1100_0000  ' Read Sampling Instant Control Register
  WDRCR   = %0101_0000  ' Write Data-Rate Control Register
  RDRCR   = %1101_0000  ' Read Data-Rate Control Register
  WCR     = %0110_0000  ' Write Configuration Register
  R_CR    = %1110_0000  ' Read Configuration Register
  RDR     = %1111_0000  ' Read Data Register

CON ' PGA constants
  GAIN_A = 1
  GAIN_B = 10
  GAIN_C = 10
  GAIN_D = 10

CON ' MUX constants
  INT = 0
  EXT = 1  

CON ' channel constants
  CHANNEL_A = %0001
  CHANNEL_B = %0010
  CHANNEL_C = %0100
  CHANNEL_D = %1000
  
CON ' constants for spi buffers
  BYTES_IN_SLAVE_BUFFER   = 512
  LONGS_IN_SLAVE_BUFFER   = BYTES_IN_SLAVE_BUFFER >> 2


  #0, SLAVE_BUF_EMPTY, SLAVE_BUF_EMPTYING, SLAVE_BUF_FULL
  
CON ' ADC buffer constants
  BYTES_IN_BLOCK     =  512   ' number of bytes in each block; this is pretty well fixed
  LONGS_IN_BLOCK     =  BYTES_IN_BLOCK/4
  BLOCKS_IN_BUFFER   =  5     ' number of blocks in buffer; this depends on available memory
  LONGS_IN_BUFFER    =  LONGS_IN_BLOCK * BLOCKS_IN_BUFFER
  SLOTS_IN_BLOCK     =  30        ' how many time slices are in each buffer?

'  SAMPLES_IN_BLOCK  =  31 * 4

  BYTES_IN_SRAM      =  1024*1024/8 - 1024   ' we're using a 1 MBit device; reduce a bit to be safe
  BLOCKS_IN_SRAM     =  (BYTES_IN_SRAM / BYTES_IN_BLOCK)     
                              ' each time slice has data from 4 channels; 3-bytes for each channel
                              ' so each slice contains 12 bytes = 3-bytes * 4 channels     
                              ' a block in the buffer looks like this:
                              ' [0..3]      CNTS_AT_START_OF_BUFFER
                              ' [4..7]      channelA
                              ' [8..11]     channelB
                              ' [12..15]    channelC
                              ' [16..20]    channelD
                              ' [21..507]   DATA
                              ' [508..511]  CNTS_AT_END_OF_BUFFER
'  FSAMPC  = 1
'  FSAMPF  = 964
'  FSAMPC  FSAMPF  Coarse  Fine    Output  Divisor 20000000.00
'  0       464     4       1       10000           2000
'  0       964     4       1       8000            2500
'  5       464     8       2       5000            4000
'  5       964     8       2       4000            5000
'  4       464     16      4       2500            8000
'  4       964     16      4       2000            10000
'  3       964     32      8       1000            20000
'  2       964     64      16      500             40000
'  1       964     128     32      250             80000


CON ' UART ports and pin numbers - DEBUG, GPS, MSP
  DEBUG             =      0
  GPS_PORT          =      1
  DEBUG_BAUD        = 115200
  GPS_BAUD          =   9600

  UART_SIZE         =    100
  CR                =     13
  SPACE             =     32
  TAB               =      9
  COLON             =     58
  COMMA             =     44
  CLEAR             =     16  ''CS: Clear Screen      
  HOME              =      1  ''HM: HoMe cursor       
  

CON ' gps buffer filled state
  MY_TRUE  = 1
  MY_FALSE = 0
  
CON ' main state machine states; some are also used as acqState states
  #0, OFF, TURNING_ON, ON, TURNING_OFF    ' main state machine states
  #0, BOOT, SLEEP, TRIG, CONT                   ' acqMode states

CON ' button states
  #0, WAITING_FOR_PRESS, WAITING_FOR_RELEASE, BUTTON_HOLDOFF   ' these are the button press/menu states

CON ' menu states'
  #0, MENU_NONE, MENU_PROMPT_TRIG, MENU_PROMPT_CONT, MENU_PROMPT_OFF, MENU_CONFIRM, MENU_INFORM
  #0, PEBBLE_TRIG, PEBBLE_CONT, PEBBLE_SHUT ' possible requested states'

CON ' timeout constants
  WATCH_DOG_TIMEOUT_MS =   15_000       ' time in ms allowed to elapse before watchdog reboots system
  MAG_ACC_TIMER_MS     =      250       ' number of ms between MAG/ACC readings
  SHUTDOWN_TIMEOUT_MS  =   10_000
  MENU_TIMEOUT_MS      =     4000       ' stay in a menu for this time before timing out'
  BOOT_TIMEOUT_MS      =     5000
  
  MAG_ACC_INTERVAL     =  MAG_ACC_TIMER_MS    * ONE_MS
  SHUTDOWN_TIMEOUT     =  SHUTDOWN_TIMEOUT_MS * ONE_MS
  MENU_TIMEOUT         =  MENU_TIMEOUT_MS     * ONE_MS  ' stay in a menu for this time before timing out'
  BOOT_TIMEOUT         =  BOOT_TIMEOUT_MS     * ONE_MS
  
DAT ' oled messages
  sleepMsg   byte   "Sleeping System.", 0
  awakeMsg   byte   "System Ready.   ", 0
  magnetMsg  byte   "Wake with Magnet", 0
  euiMsg     byte   "SN:             ", 0
  versionMsg byte   "Version 2.2.pgb ", 0

OBJ                                  
  UARTS     : "FullDuplexSerial4portPlus_0v3"       '1 COG for 3 serial ports
  NUM       : "Numbers"                             'Include Numbers object for writing numbers to debuginal
  UBX       : "ubloxInterface2"
  PEBBLE    : "BasicPebbleFunctions"                ' I put these into a seperate file to make editing easier
  CLOCK     : "Clock"                               ' object for changing clock speeds
  
VAR
  byte mainCogId, serialCogId, adcCogId, slaveCogId, watchDogCogId
  byte mainState, acqMode, buttonState
  byte menuState, requestedState
  byte oledOn, onDuration, led1on, led2On
  byte lockType, recordComplete
  byte edgeDetector
  byte auxIdx
  byte inUartBuf[UART_SIZE], inUartIdx, ptrIdx, uartState, inUartPtr[MAX_COMMANDS]
  byte oLedBuf[20] ' set aside some room for the oled 
  byte sramParms[64]

  word magAccSampleCnt
  word dacValue, dacNew, dacOld
  word rtcYear, rtcMonth, rtcDay, rtcDow, rtcHour, rtcMinute, rtcSecond
  word year, month, day, hour, minute, second, utcValid, gpsValid, fixStat
          
  long rtcYMD, rtcHMS
  long pressTime, releaseTime, onTime
  long menuPressTime, led2Time ' time when the menu button was pressed - use for timeout'
  long magAccTimer, displayDark, lastRTCcheck

  long change, vcxoError, k
  long watchDogTimer, watchDogStack[16]
  ' ubxBufferAddress is the HUB memory location of gps data stored by ubx object
  long ubxBufferAddress, gpsHeader, oldRecLen
  long sampleRate, gainA, gainB, gainC, gainD, sourceA, sourceB, sourceC, sourceD, interval, recordLength ' these are longs because that's how they are stored in the EEPROM
  long rtcAddr        ' hub address of 7 bytes containing the RTC time
  long euiAddr        ' hub address of 48 bytes containing the unique ID for this device
  ' keep these in order  ymd, hms, cntsAtPPS are populated in the main cog but need to be passed down to the SPIslave cog
  long ymd, hms, cntsAtPPS1, validStatus, cntsTemp, accuracy

  ' the next 2 line refers to the HUB storage where things sit before be shifted out to the gumstix
  long auxBuffer1[128]          ' put stuff here while we collect it.  Then move it to aux2 to had over to the SPI cog
  long auxDataToWrite, auxBuffer2[128]
  long gpsDataToWrite, gpsBuffer[256]
  long slaveBufCMD, slaveBuffer[LONGS_IN_SLAVE_BUFFER]       


  ' the next line refers to data read by the ADC and stored in the HUB; this is where data rest on their way to SRAM
  long fifoSem, blocksInFIFO, adcBuffer[BLOCKS_IN_BUFFER*LONGS_IN_BLOCK] ' this is a buffer that we fill from the ADC directly. Data are
                                                                         ' moved from the ADC cog to HUB then from HUB to the PASM Slave
                                                                         ' cog where they are written out to the gumstix

PUB MAIN | bPressed, previousState
' the main method here is a simple state machine
    
  mainCogId     := cogid
  serialCogId   := -1
  CLOCK.INIT(FULL_SPEED/16)       ' provide external crystal freq

  START_WATCHDOG

  PEBBLE.INIT                   ' set up the I2C pin definitions only once, hold SRAM in reset

  DIRA[WAKEUP]  := 1
  OUTA[WAKEUP]  := 0

  'pebble boot console- only available at startup/reboot
  BOOT_CONSOLE

  mainState := TURNING_ON       ' mainState is NOT stored in SRAM
  acqMode   := BOOT             ' this indicates first boot

  repeat
    PET_WATCHDOG                ' do this every time through the loop.

    bPressed := BUTTON_PRESSED  ' check for button press

    case mainState
      OFF  :                    ' mainState can only be OFF if the acqMode is SLEEP or we are sleeping between triggers
        case acqMode
          SLEEP : 
            if bPressed                      'wake up from sleep and enter continuous mode
              mainState  := TURNING_ON
              acqMode    := CONT
              onDuration := 0


          TRIG  :                            'wake up from sleep because trigger time arrived
            OUTA[WAKEUP] := 1
            OUTA[WAKEUP] := 0

            if ((rtcMinute//interval)==(interval - 1) AND (rtcSecond > 40)) OR bPressed
              mainState  := TURNING_ON
              acqMode    := TRIG
              if bPressed
                onDuration := 0
                recordComplete := FALSE
              else
                onDuration := 30 - 10 ' normally this is set to 30 seconds, but with no button press it should only be 10 seconds
              
            if (CNT - lastRTCcheck) > (clkfreq<<1)   ' avoid checking the RTC too often; how about every 2 seconds
                PEBBLE.LED2_ON                       ' blink LED to indicate TRIG mode
                READ_RTC
                lastRTCcheck := CNT
                PEBBLE.LED2_OFF


      TURNING_ON :              ' always pass through this state on the way to ON; gives an opportunity to print current acqMode
        WAKE_SYSTEM(acqMode)
        UARTS.STR(DEBUG, string(13, "Turning ON: "))
        case acqMode
          SLEEP :
            UARTS.STR(DEBUG, string("Sleep"))
            mainState := TURNING_OFF
          TRIG :
            UARTS.STR(DEBUG, string("Triggered"))
            START_ACQUISITION
            mainState := ON
            recordComplete := FALSE
          CONT :
            UARTS.STR(DEBUG, string("Continuous"))
            START_ACQUISITION
            mainState := ON
            onDuration := 0
          OTHER :
            UARTS.STR(DEBUG, string("Something is wrong; Turning on in CONT mode."))
            PAUSE_MS(5000)
            mainState := TURNING_ON
            acqMode   := CONT


      ON:                       ' this is where the work gets done; but we can also change states from here
        DO_SOMETHING_USEFUL

        case acqMode
          CONT : 
            if bPressed
              onDuration := 0

            if oledOn == FALSE AND bPressed
              UARTS.STR(DEBUG, string(13,"Turning OLED ON"))
              PEBBLE.OLED_ON                
              oledOn := TRUE
              onDuration := 0
            else 
              MENU_SYSTEM(bPressed)
              if onDuration > 30    ' is it time to turn off the OLED?
                oledOn := FALSE
                PEBBLE.OLED_OFF

            if (CNT - led2Time) > FULL_SPEED    ' toggle the LED slowly
              led2Time := CNT
              if led2On
                PEBBLE.LED2_OFF
                led2On := FALSE
              else
                PEBBLE.LED2_ON
                led2On := TRUE
        
          TRIG :     
            MENU_SYSTEM(bPressed)

            if recordComplete == FALSE ' have we completed this record?  If so, turn off  
              if (rtcMinute//interval==0) AND rtcSecond == recordLength
                UARTS.STR(DEBUG, string(13,"Record Complete"))
                recordComplete := TRUE
                
            if recordComplete AND onDuration > 30
              mainState := TURNING_OFF
              acqMode   := TRIG

            if (CNT - led2Time) > CONSTANT(FULL_SPEED>>2)    ' toggle the LED
              led2Time := CNT
              if led2On
                PEBBLE.LED2_OFF
                led2On := FALSE
              else
                PEBBLE.LED2_ON
                led2On := TRUE
        
      TURNING_OFF :
        TURN_SYSTEM_OFF
        mainState := OFF
        lastRTCcheck := CNT


PUB WAKE_SYSTEM(newAcqMode) | displayTime 

' these are the things that need to happen EVERY time we come out of sleep
' sleep is all things turned off
  serialCogId   := -1
  adcCogId      := -1
  slaveCogId    := -1
  menuState     := MENU_NONE
  DIRA[ADC_CS..ADC_DRDYOUT]   := 0        ' make sure this cog isn't holding ADC pins captive    
  DIRA[SLAVE_IRQ..SLAVE_SCLK] := 0        ' make sure this cog isn't holding SLAVE lines captive
  
  if clkfreq <> FULL_SPEED
    STOP_WATCHDOG           ' stop watchdog before changing clock speed
    CLOCK.SetMode(CLOCK#XTAL1_PLL16x)  ' set clock to 100Mhz
    START_WATCHDOG          ' we need to start the watch dog with the new clockspeed

  'PEBBLE.LED1_OFF               ' turn OFF LED1 
  PEBBLE.LED2_ON                ' turn ON  LED2  indicating we are acquiring data
  led2Time := CNT
  PEBBLE.GUMSTIX_ON             ' sak preferes to have gumstix on right away
  PEBBLE.ANALOG_ON
  PEBBLE.OLED_ON                ' turn on and init oled
  PEBBLE.GPS_ON

  oledOn := TRUE
  DISPLAY_ON_OLED(1,string(" Waking System "))

  LAUNCH_SERIAL_COG             ' handle the 2 serial ports- debug and GPS
  PAUSE_MS(1_000)
  UARTS.STR(DEBUG, string(13,13,13, "$PSMSG, Waking system: "))
  READ_RTC
  PRINT_RTC_TIME
  PET_WATCHDOG

  if newAcqMode == BOOT         ' on first boot only we need to get parameters 
    GET_PARAMETERS_FROM_SRAM    ' otherwise we can skip that because we alrady have them in memory
  else
    acqMode := newAcqMode       ' otherwise we need to save current params to SRAM                    
    STORE_PARAMETERS_TO_SRAM

  PRINT_SYSTEM_PARAMETERS
  UARTS.STR(DEBUG, string(13, "acqMode is: "))
  case acqMode
    BOOT  :  UARTS.STR(DEBUG, string("BOOT"))
    SLEEP :  UARTS.STR(DEBUG, string("SLEEP"))
    TRIG  :  UARTS.STR(DEBUG, string("TRIG"))
    CONT  :  UARTS.STR(DEBUG, string("CONT"))
    OTHER :  UARTS.STR(DEBUG, string("UNKN"))


  CONFIG_GPS
  PET_WATCHDOG
  PAUSE_MS(1_000)

  PRINT_EUI
  PET_WATCHDOG
  PAUSE_MS(1_000)

  ' set up mag and acc measurements- run these well above our sample rate and we'll get data when we can.
  PEBBLE.MAG_ACC_ON
  PEBBLE.LSM_INIT
  PET_WATCHDOG

    
' setup counter B in this cog to track the number of system counts between
' the rising GPS and when we get over to read it.
' this frees us from having a cog dedicated to just watching the gpspps

  CTRB[30..26] := %01000                                ' ctrb module to POS detector;                                                        
  CTRB[5..0]   := GPS_PPS                               ' add frqb to phsb every clock cycle that GPS_PPS high
  FRQB := 1                                             ' this will keep track of the number of system counts
  PHSB~                                                 ' from the rising edge of GPS

  GET_GPS_LOCK

  UPDATE_TIME_AND_DATE
  PRINT_TIME_AND_DATE(TRUE)
  PET_WATCHDOG
  PAUSE_MS(2000)

  UARTS.STR(DEBUG, string(13, "$PSMSG, mainCogId:     "))
  UARTS.DEC(DEBUG, mainCogId)

  UARTS.STR(DEBUG, string(13, "$PSMSG, serialCogId:   "))
  UARTS.DEC(DEBUG, serialCogId)

  UARTS.STR(DEBUG, string(13, "$PSMSG, watchDogCogId: "))
  UARTS.DEC(DEBUG, watchDogCogId)

  UARTS.STR(DEBUG, string(13, "$PSMSG, timeOutMS: "))
  UARTS.DEC(DEBUG, WATCH_DOG_TIMEOUT_MS)

  UARTS.STR(DEBUG, string(13, "$PSMSG, timeOutCnts: "))
  UARTS.DEC(DEBUG, FULL_SPEED/1000 * WATCH_DOG_TIMEOUT_MS)

  UARTS.STR(DEBUG, string(13, "$PSMSG, CLK_FREQ: "))
  UARTS.DEC(DEBUG, FULL_SPEED)

  UARTS.STR(DEBUG, string(13, "$PSMSG, SLEEP_CLK: "))
  UARTS.DEC(DEBUG, SLEEP_SPEED)

  PAUSE_MS(200)
  DISPLAY_ON_OLED(1,@awakeMsg)
  DISPLAY_ON_OLED(2,@versionMsg)
  UARTS.STR(DEBUG, string(13, "$PSMSG, "))
  UARTS.STR(DEBUG, @versionMsg)

{' P4 pin testing
  STOP_WATCHDOG
  DIRA[SPARE] := 0 ' set to input
  repeat
    PAUSE_MS(1000)
    UARTS.STR(DEBUG, string(13, "P4: "))
    if INA[SPARE] == 1
      UARTS.STR(DEBUG, string("HIGH"))
    else
      UARTS.STR(DEBUG, string("LOW"))
}
    
PUB TURN_SYSTEM_OFF | i
'' method that shuts things down beforing going into OFF/SLEEP mode
'' send shutdown message to gumstix, kill off running cogs except
'' main cog and watchdog
'  STORE_PARAMETERS_TO_SRAM
  UARTS.STR(DEBUG, string(13, "$PSMSG. Sleeping system.  Use magnet to wake or wait for trigger."))
  repeat 3   ' why is this hear?
    UARTS.STR(DEBUG, string(13, "$PSMSG. SHUTDOWN"))
  DISPLAY_ON_OLED(1,@sleepMsg)
  DISPLAY_ON_OLED(2,@magnetMsg)
  DIRA[SLAVE_IRQ] := 1         ' raise IRQ line so gumstix doesn't keep acquiring
  OUTA[SLAVE_IRQ] := 1    

  i := cnt
  repeat
    PET_WATCHDOG
    PAUSE_MS(100)
  until {INA[SPARE] == 1 OR} (CNT - i) > SHUTDOWN_TIMEOUT

  PEBBLE.SET_EXPANDER_TO_LOW_POWER
  oledOn := FALSE

  repeat i from 0 to 7
    if i <> mainCogId  AND i <> watchDogCogId
      cogstop(i)

  adcCogId    := -1
  serialCogId := -1
  slaveCogId  := -1

  lockret(fifoSem)                               ' 
  lockret(gpsSem)                               '

  PAUSE_MS(200)
  
  STOP_WATCHDOG           ' stop watchdog before changing clock speed
  CLOCK.SetMode(CLOCK#XTAL1_) ' rcslow seems too slow; xtal1 seems lower power than rcfast and is more stable
  'CLOCK.SetMode(CLOCK#XTAL1_PLL16x)  ' set clock to 100Mhz
  START_WATCHDOG          ' we need to start the watch dog with the new clockspeed
  'CLOCK.SetMode(CLOCK#RCSLOW_) ' rcslow seems too slow; xtal1 seems lower power than rcfast and is more stable

PUB MENU_SYSTEM(bPressed)
' method that displays and handles the menu system
 ' normally display time (MENU_NONE), state 0
  ' if swiped, display prompt to switch to state 1
  '    if swiped in state 1, display prompt to switch to state 2, etc
  '    in state N, switch back to state 0'
  ' if in state 1 to N, and it times out, then display a confirm message'
  ' if confirmed, then switch to that state'

  case menuState
    MENU_NONE        :
      'PEBBLE.OLED_WRITE_LINE1(string("Regular Mode    "))
      if bPressed 
        menuState      := MENU_PROMPT_TRIG
        requestedState := TRIG
        menuPressTime  := cnt
        UARTS.STR(DEBUG, string(13, "In NONE -> TRIG"))

    
    MENU_PROMPT_TRIG : ' In xxxx --> TRIG'
      if (cnt - menuPressTime) > MENU_TIMEOUT      ' if we've timed out
        menuState        := MENU_CONFIRM           ' assume this is the desired state; advance to cnofirm 
        menuPressTime    := cnt
      else
        if bPressed ' 
          menuState      := MENU_PROMPT_CONT
          requestedState := CONT
          menuPressTime  := cnt
          UARTS.STR(DEBUG, string(13, "In TRIG -> CONT"))


    MENU_PROMPT_CONT : ' In xxxx --> CONT'
      if (cnt - menuPressTime) > MENU_TIMEOUT      ' if we've timed out
        menuState        := MENU_CONFIRM           ' assume this is the desired state; advance to cnofirm 
        menuPressTime    := cnt
      else
        if bPressed ' 
          menuState      := MENU_PROMPT_OFF
          requestedState := OFF
          menuPressTime  := cnt
          UARTS.STR(DEBUG, string(13, "In CONT -> OFF"))


    MENU_PROMPT_OFF : ' In xxxx --> TRIG'
      if (cnt - menuPressTime) > MENU_TIMEOUT      ' if we've timed out
        menuState        := MENU_CONFIRM           ' assume this is the desired state; advance to cnofirm 
        menuPressTime    := cnt
      else
        if bPressed ' 
          menuState      := MENU_PROMPT_TRIG
          requestedState := TRIG
          menuPressTime  := cnt
          UARTS.STR(DEBUG, string(13, "In OFF -> TRIG"))


    MENU_CONFIRM : ' Press to confirm choice
      if (cnt - menuPressTime) > MENU_TIMEOUT      ' if we've timed out
        menuState := MENU_NONE                     ' return to beginning 
        UARTS.STR(DEBUG, string(13, "No response; timed out."))
      else
        if bPressed ' 
          UARTS.STR(DEBUG, string(13, "Switching states"))
          menuState     := MENU_INFORM
    MENU_INFORM      : ' inform the user of their choice'
      if (cnt - menuPressTime) > MENU_TIMEOUT
          menuState := MENU_NONE
          menuPressTime := cnt
          case requestedState
            OFF  :
              mainState := TURNING_OFF
              acqMode   := SLEEP
            TRIG :
              mainState := ON
              acqMode   := TRIG
            CONT :
              mainState := ON
              acqMode   := CONT
          STORE_PARAMETERS_TO_SRAM


PUB BUTTON_PRESSED  : buttonPressed 
' method that checks to see the button has been pressed.
' lights LED1 when
  buttonPressed := FALSE
  case buttonState
    WAITING_FOR_PRESS :
      if INA[REED_SWITCH] == PRESSED
        pressTime   := CNT
        buttonState := WAITING_FOR_RELEASE
  
    WAITING_FOR_RELEASE :
      if INA[REED_SWITCH] == PRESSED AND (CNT-pressTime) > clkfreq>>1 ' button currently held for more than one second
          PEBBLE.LED1_ON                  ' indicate that the button WAS pressed
          onTime        := CNT
          led1on        := TRUE
          buttonState   := BUTTON_HOLDOFF
          buttonPressed := TRUE
          onDuration    := 0
          displayDark   := CNT
      
      if INA[REED_SWITCH] == NOT_PRESSED
        if (CNT-pressTime) > clkfreq>>1      ' pressed for more than a second      
          PEBBLE.LED1_ON                  ' indicate that the button WAS pressed
          onTime        := CNT
          led1on        := TRUE
          buttonState   := BUTTON_HOLDOFF
          buttonPressed := TRUE
          onDuration    := 0
        else                              ' not pressed long enough
          buttonState := WAITING_FOR_PRESS

    BUTTON_HOLDOFF :            ' prevent method from seeing consecutive presses without some pause
      if led1on AND (CNT-onTime) > clkfreq              ' is it time to turn off the LED
        PEBBLE.LED1_OFF
        led1on := FALSE
        
      if CNT-onTime > clkfreq<<1       ' 2 second hold-off
        buttonState := WAITING_FOR_PRESS
    

PUB START_WATCHDOG
'' method that starts a new watchdog timer in unique cog
' first stop any existing watchdog cogs then start the watchdog
  watchDogCogId := cognew(WATCHDOG, @watchDogStack)

PUB STOP_WATCHDOG
' if watchdog is already running kill it; indicate this by setting id to -1
  if watchDogCogId == -1
    return              ' watchdog isn't running

  cogstop(watchDogCogId)
  watchDogCogId := -1
      
PUB WATCHDOG | resetPressed, resetState 
'' Watchdog function.  Should be started in its own cog.
'' This watchdog does 1 thing it watches a shared memory location (single LONG in HUB).  If that location
''     isn't updated evert WATCH_DOG_TIMEOUT_CNT this cog will reboot the prop.

  watchDogTimer := CNT          ' init time so we don't reboot right away
  resetPressed  := FALSE
  resetState    := WAITING_FOR_PRESS
  
  repeat
    PAUSE_MS(10)                ' spend some time sleeping but not too much

    ' Check for watchdog timeout
    if (CNT - watchDogTimer) > (clkfreq/1000 * WATCH_DOG_TIMEOUT_MS) 
      REBOOT


{    case resetState
      WAITING_FOR_PRESS :
        if INA[REED_SWITCH] == PRESSED
          resetPressed := CNT
          resetState   := WAITING_FOR_RELEASE
    
      WAITING_FOR_RELEASE :
        if INA[REED_SWITCH] == PRESSED AND (CNT-resetPressed) > clkfreq*15 ' button currently held for more than one second
          REBOOT
                
        if INA[REED_SWITCH] == NOT_PRESSED
          if (CNT-resetPressed) > clkfreq>>1      ' pressed for more than a second
            REBOOT
          else                              ' not pressed long enough
            buttonState := WAITING_FOR_PRESS
}  
PUB PET_WATCHDOG
  watchDogTimer := CNT   ' pet the watchdog                       

PUB START_ACQUISITION  
' after waking from sleep we need to start up a number of different things.
' everything that needs to be done only once/start should go here.
' all of this happens in the top-level cog

  'UARTS.STR(DEBUG, string(13, "$PSMSG, Powering Analog Section."))  
  PEBBLE.ANALOG_ON

  dacValue := $7FFF       ' set to middle of the road value
  'UARTS.STR(DEBUG, string(13, "$PSMSG, Setting DAC value: "))
  'uarts.dec(DEBUG,dacValue)
  PEBBLE.WRITE_TO_DAC(dacValue)     ' put a middle of the road value in here to start with
  PAUSE_MS(100)
  
  fifoSem   := locknew  'semaphore that guards the blocksInFIFO HUB location which is shared by 2 PASM cogs
  gpsSem    := locknew
  

  auxDataToWrite := MY_FALSE         ' remember FALSE = 0
  auxFlagPtr     := @auxDataToWrite  ' pass the HUB address to SPI_SLAVE cog
  auxBasePtr     := @auxBuffer2      ' pass the HUB address to SPI_SLAVE cog

  gpsDataToWrite := MY_FALSE         ' remember FALSE = 0
  gpsFlagPtr     := @gpsDataToWrite  ' pass the HUB address to SPI_SLAVE cog
  gpsBasePtr     := @gpsBuffer       ' pass the HUB address to SPI_SLAVE cog

  timeDatePtr    := @ymd
  validPtr       := @validStatus
  blocksInFIFO   := 0

  
  slaveCogId := cognew(@GUMSTIX_SLAVE, @fifoSem)        ' Start the SPI slave cog first and move stuff from HUB to Gumstix
  UARTS.STR(DEBUG, string(13, "$PSMSG, slaveCogId:    "))
  UARTS.DEC(DEBUG, slaveCogId)
  DIRA[SLAVE_IRQ] := 0         ' make sure we aren't holding this line high; if we are other cog won't be albe to indicate data available

  PAUSE_MS(1000)
  adcCogId := SETUP_AND_LAUNCH_ADC(sampleRate, gainA, gainB, gainC, gainD, sourceA, sourceB, sourceC, sourceD)
  UARTS.STR(DEBUG, string(13, "$PSMSG, adcCogId:      "))
  UARTS.DEC(DEBUG, adcCogId)

  'UARTS.STR(DEBUG, string(13, "SPI_MASTER: "))
  'UARTS.DEC(DEBUG, cognew(@SPI_MASTER, 0))' SPI MASTER code used for testing only; be sure to start the SPI slave cog first
  

PUB GET_AND_PRINT_PARAMETERS
  ' check to see if factury defaults have already been stored
  'CHECK_DEFAULT_PARAMS
  GET_PARAMETERS_FROM_SRAM
  PRINT_SYSTEM_PARAMETERS                                                     

PUB DO_SOMETHING_USEFUL | rxByte, response, idx
' put things here that need to be done but have some timing flexibility
' all of this happens in the top level cog

  ' here we collect bytes from uart and process them.
  ' this should follow the same setup as the ublox I think
   rxByte := uarts.rxcheck(DEBUG)       ' collect byte from DEBUG port
   case uartState
      WAITING_FOR_START :
        if rxByte == "$"                     ' beginning of a new string
          inUartIdx := 0
          ptrIdx    := 0
          bytefill(@inUartIdx, 0, UART_SIZE) ' buffer for all incoming bytes
          bytefill(@inUartPtr, 0, MAX_COMMANDS) ' contains the index of each field 
          uartState := WAITING_FOR_END

      WAITING_FOR_END   :
        if rxByte <> -1
          case rxByte
            ","    :
              inUartBuf[inUartIdx++ <# UART_SIZE] := 0          ' zero terminate strings
              inUartPtr[++ptrIdx] := inUartIdx <# MAX_COMMANDS
              uartState := WAITING_FOR_END
            "%"   :
              inUartBuf[inUartIdx++ <# UART_SIZE] := 0 ' <-- ADD THIS zero terminate string
              uartState := PROCESS_BUFFER
            OTHER : 
              inUartBuf[inUartIdx++ <# UART_SIZE] := rxByte     ' if not, keep adding to it
              uartState := WAITING_FOR_END

     PROCESS_BUFFER     :
       PROCESS_UART
       uartState := WAITING_FOR_START
    


  edgeDetector := ((edgeDetector << 1) | INA[GPS_PPS] ) & %11    
  case edgeDetector 
    %01 : ' rising edge -> grab phsb
      cntsTemp := CNT - PHSB
      onDuration++
      UPDATE_TIME_AND_DATE                   ' KEEP IN MIND WE ALWAYS UPDATE TIME/DATE ON THE PPS following OUR MESSAGE SO WE ARE BEHIND ONE SECOND
      UARTS.STR(DEBUG, string(13, "menuState:      "))
      UARTS.DEC(DEBUG, menuState)
      UARTS.STR(DEBUG, string(" onDuration: "))
      UARTS.DEC(DEBUG, onDuration)
      case menuState
        MENU_NONE        :
          PRINT_TIME_AND_DATE(FALSE)
        MENU_PROMPT_TRIG :
          DISPLAY_ON_OLED(1,string("SWITCH TO TRIG? "))
        MENU_PROMPT_CONT :                                
          DISPLAY_ON_OLED(1,string("SWITCH TO CONT? "))
        MENU_PROMPT_OFF :
          DISPLAY_ON_OLED(1,string("TURN UNIT OFF?  "))
        MENU_CONFIRM     :
          if requestedState == TRIG
            DISPLAY_ON_OLED(2,string("PRESS TO CONFIRM"))
          if requestedState == CONT
            DISPLAY_ON_OLED(2,string("PRESS TO CONFIRM"))
          if requestedState == OFF
            DISPLAY_ON_OLED(2,string("PRESS TO CONFIRM"))
        MENU_INFORM      :
          if requestedState == TRIG
            DISPLAY_ON_OLED(1,string("SWITCHING TO    "))
            DISPLAY_ON_OLED(2,string("TRIGGERED MODE  "))
          if requestedState == CONT
            DISPLAY_ON_OLED(1,string("SWITCHING TO    "))
            DISPLAY_ON_OLED(2,string("CONTINUOUS MODE "))
          if requestedState == OFF
            DISPLAY_ON_OLED(1,string("SWITCHING TO    "))
            DISPLAY_ON_OLED(2,string("SHUTDOWN        "))

      
    %10 : ' falling edge now we can reset phsb since it won't increment until PPS goes high again
      PHSB := 0

  ' if it's time for a MAG/ACC measurement, go get it
  if (CNT - magAccTimer) > MAG_ACC_INTERVAL
    if magAccSampleCnt < 15
      magAccTimer := CNT
      magAccSampleCnt++
      auxBuffer1[auxIdx++ <# 128] := PEBBLE.GET_ACC_X
      auxBuffer1[auxIdx++ <# 128] := PEBBLE.GET_ACC_Y
      auxBuffer1[auxIdx++ <# 128] := PEBBLE.GET_ACC_Z
      PEBBLE.READ_MAG             ' we need to read the mag before the others are populated.  
      auxBuffer1[auxIdx++ <# 128] := PEBBLE.GET_MAG_X
      auxBuffer1[auxIdx++ <# 128] := PEBBLE.GET_MAG_Y
      auxBuffer1[auxIdx++ <# 128] := PEBBLE.GET_MAG_Z
      auxBuffer1[auxIdx++ <# 128] := PEBBLE.READ_MAG_TEMP
      if magAccSampleCnt == 15
        outa[wakeup]  := 1
        SEND_AUX_PACKET
        UARTS.STR(DEBUG, string(13,"$PSMSG, Sending AUX. "))
        outa[wakeup]  := 0
    

  ' here we process all bytes sitting in the gps buffer -
  ' and we stay in this repeat until the uart buffer is empty OR we've reached the end of the packet
  repeat
    response := UBX.READ_AND_PROCESS_BYTE(FALSE)
  until response == -1 OR response == UBX#RXMRAW

  
  if response == UBX#RXMRAW                            ' have we collected all the data in this second?
    UARTS.STR(DEBUG, string(13,"$PSMSG, FIFO: "))
    UARTS.DEC(DEBUG, blocksInFIFO)                     ' if the gps buffer is empty put something into it
    case acqMode
      CONT  : UARTS.STR(DEBUG, string( " Mode: C"))
      TRIG  : UARTS.STR(DEBUG, string( " Mode: T, "))
              UARTS.DEC(DEBUG, recordLength)
      OTHER : UARTS.STR(DEBUG, string( " Mode: UNKNOWN"))
      
    if gpsDataToWrite == MY_FALSE                      ' have the data we put there previously been written?
      bytemove(@gpsBuffer, ubxBufferAddress, 1024)     ' copy that info into gpsBuffer
      longmove(@gpsBuffer[129], @gpsBuffer[127], 127)  ' move data over so we can insert header
      longmove(@gpsBuffer[1],   @gpsBuffer[0],   127)  ' move data over so we can insert header
      gpsBuffer[0]   := "P" << 24 | "G" << 16 | VERSION << 8 | validStatus
      gpsBuffer[128] := "P" << 24 | "G" << 16 | VERSION << 8 | validStatus

    ' buffer has new data so clear old buffer and declare buffer should be written
    UBX.CLEAR_UBX_BUFFER                                ' clear the pointer on that side so it can process next second
    gpsDataToWrite := MY_TRUE                           ' let the other cogs know there are gps data to write

PUB SEND_AUX_PACKET | idx, wakeUpMode, recTypeEnum, timeOfDayOn, timeOfDaySleep, radioOnDuration
' copy the aux data from aux1 to aux2
' signal the SPI cog that we've got aux data to send
' wipe out the buffer so it's ready for next time

  if auxDataToWrite == MY_TRUE  ' haven't emptied old buffer - return.
    return
  
  longmove(@auxBuffer2 , @auxBuffer1 , 128)
  idx := 0                      ' start using this as a BYTE-pointer

  ' long ZERO
  auxBuffer2.byte[idx++] := validStatus
  auxBuffer2.byte[idx++] := VERSION
  auxBuffer2.byte[idx++] := "A"
  auxBuffer2.byte[idx++] := "P"

  ' long ONE
  auxBuffer2.byte[idx++] := 0 'reserved
  auxBuffer2.byte[idx++] := interval
  auxBuffer2.byte[idx++] := recordLength
  case recTypeEnum
    0 : auxBuffer2.byte[idx++] := 0 ' continuous
    1 : auxBuffer2.byte[idx++] := 1 ' triggered
    2 : auxBuffer2.byte[idx++] := 2 ' timed
    3 : auxBuffer2.byte[idx++] := 3 ' threshold
    4 : auxBuffer2.byte[idx++] := 4 ' radio
    OTHER : auxBuffer2.byte[idx++] := 0
     
  ' long TWO
  auxBuffer2.byte[idx++] := radioOnDuration
  auxBuffer2.byte[idx++] := timeOfDaySleep
  auxBuffer2.byte[idx++] := timeOfDayOn
  auxBuffer2.byte[idx++] := wakeUpMode

  ' long THREE
  idx := 3                     ' now switch to using it as a LONG-pointer
  auxBuffer2[idx++] := dacValue
  auxBuffer2[idx++] := rtcYear * 10000 + rtcMonth  * 100 + rtcDay
  auxBuffer2[idx++] := rtcHour * 10000 + rtcMinute * 100 + rtcSecond
  auxBuffer2[idx++] := MAG_ACC_TIMER_MS ' reserved
  auxBuffer2[idx++] := 0 ' reserved

  auxBuffer2[idx++] := magAccSampleCnt ' number of samples in this buffer
  magAccSampleCnt   := 0

  auxIdx        := idx ' the first LONG of data should go here

  longfill(@auxBuffer1 , 0 , 128)
  auxDataToWrite := MY_TRUE
    
PUB UPDATE_TIME_AND_DATE | isLeapYear
'' get the RTC AND GPS time

  READ_RTC

  year     := UBX.YEAR
  month    := UBX.MONTH
  day      := UBX.DAY
  hour     := UBX.HOUR
  minute   := UBX.MINUTE
  second   := UBX.SECOND
  utcValid := UBX.UTC_TIME_VALID
  gpsValid := UBX.GPS_TIME_VALID
  accuracy := UBX.ACCURACY_NS
  fixStat  := UBX.GET_STATUS_FIX

  isLeapYear := 1 + ((year//4 == 0 AND year//100 <> 0 OR year//400 == 0)) ' check to see if this is a leap year
  ' leap = 0 means leap year; leap = 1 means not a leap year

' calculate the time that goes with NEXT PPS
  second += 1
  if second == 60
    second := 0
    minute++
  if minute == 60
    minute := 0
    hour++
  if hour == 24
    hour := 0
    day++
    if isLeapYear
      if day > leapTable[month-1]
        day := 0
        month++
        if month > 12
          month := 1
          year++
    else  
      if day > noLeapTable[month-1]
        day := 0
        month++
        if month > 12
          month := 1
          year++

  ' move a copy of the ymd hms and other shared data into section of memory that PASM has access to
  repeat until not lockset(gpsSem)       ' wait here until we can get the semaphore
    UARTS.STR(DEBUG,string(13,"Waiting for gpsSem."))
    PAUSE_MS(10)
  ymd         := year * 10_000 + month * 100 + day
  hms         := hour * 10_000 + minute * 100 + second 

  ' sak added 4/28/15 - for testing in non-gps situations'
  if fixStat == 0
    rtcYMD         := rtcYear * 10_000 + rtcMonth  * 100 + rtcDay
    rtcHMS         := rtcHour * 10_000 + rtcMinute * 100 + rtcSecond
    ymd := rtcYMD
    hms := rtcHMS

  cntsAtPPS1  := cntsTemp
  validStatus := fixStat & $FF   ' make sure this only occupies one byte
  lockclr(gpsSem)


  case fixStat
    0 : lockType := "U"             ' no fix
    1 : lockType := "D"             ' dead reckoning only
    2 : lockType := "2"             ' 2D-fix
    3 : lockType := "3"             ' 3D-fix
    4 : lockType := "G"             ' GPS + dead reckoning combined
    5 : lockType := "T"             ' Time only fix
    OTHER: lockType := "X"          ' unknown- this shouldn't happen 

  if (gpsValid > 2)         ' if the gps time is correct; not to be confused with UTC time
    if rtcYear <> year OR rtcMonth <> month OR rtcDay <> day OR rtcHour <> hour OR rtcMinute <> minute
      UARTS.STR(DEBUG,string(13,"$PSMSG, Updating RTC from GPS large mismatch:"))
      PEBBLE.SET_RTC_TIME(year, month, day, hour, minute, second, 0)            ' use gps time to update rtc
      READ_RTC
      PRINT_RTC_TIME
    if (||(second-rtcSecond)>2)         ' if the gps and RTC time are not within a couple seconds
      UARTS.STR(DEBUG,string(13,"$PSMSG, Updating RTC from GPS small mismatch:"))
      PEBBLE.SET_RTC_TIME(year, month, day, hour, minute, second, 0)            ' use gps time to update rtc
      READ_RTC
      PRINT_RTC_TIME


PUB PRINT_RTC_TIME
  uarts.str(debug, string(13, "$PSMSG, RTC,"))
  uarts.dec(debug, rtcyear)
  uarts.putc(debug, "-")
  uarts.dec(debug, rtcmonth)
  uarts.putc(debug, "-")
  uarts.dec(debug, rtcday)
  uarts.putc(debug, "T")
  uarts.dec(debug, rtchour)
  uarts.putc(debug, ":")
  uarts.dec(debug, rtcminute)
  uarts.putc(debug, ":")
  uarts.dec(debug, rtcsecond)

PUB PRINT_TIME_AND_DATE(display)
  PRINT_RTC_TIME
' print the RTC and GPS time and dates
  ' don't do this in ON state, gumstix owns line1 and 2 of oled
  if display
    oLedBuf[ 0] := "0" #> (rtcYear  / 10 // 10+ "0"   ) <# "9"
    oLedBuf[ 1] := "0" #> (rtcYear // 10 + "0"        ) <# "9"
    oLedBuf[ 2] := "0" #> (rtcMonth  / 10 // 10 + "0" ) <# "9"               ' divide first to guarnatee result is less than 10
    oLedBuf[ 3] := "0" #> (rtcMonth // 10+ "0"        ) <# "9"
    oLedBuf[ 4] := "0" #> (rtcDay  / 10 // 10 + "0"  ) <# "9"              ' divide first to guarnatee result is less than 10
    oLedBuf[ 5] := "0" #> (rtcDay // 10+ "0"         ) <# "9"
    oLedBuf[ 6] := " "
    oLedBuf[ 7] := "0" #> (rtcHour  / 10 // 10 + "0"  ) <# "9"
    oLedBuf[ 8] := "0" #> (rtcHour // 10 + "0"        ) <# "9"
    oLedBuf[ 9] := "0" #> (rtcMinute  / 10 // 10 + "0" ) <# "9"
    oLedBuf[10] := "0" #> (rtcMinute // 10 + "0"       ) <# "9"
    oLedBuf[11] := "0" #> (rtcSecond  / 10 // 10 + "0" ) <# "9"
    oLedBuf[12] := "0" #> (rtcSecond // 10 + "0"       ) <# "9"
    oLedBuf[13] := " "
    if UBX.NUM_SVs > 9
      oLedBuf[14] := UBX.NUM_SVs - 9 + "A"
    else
      oLedBuf[14] := UBX.NUM_SVs + "0"
    oLedBuf[15] := lockType
    oLedBuf[16] := 0              ' zero terminate the buffer
    DISPLAY_ON_OLED(1,@oLedBuf)


  uarts.str(debug, string(13, "$PSSTAT,"))
  uarts.dec(debug, year)
  uarts.putc(debug, "-")
  uarts.dec(debug, month)
  uarts.putc(debug, "-")
  uarts.dec(debug, day)
  uarts.putc(debug, "T")
  uarts.dec(debug, hour)
  uarts.putc(debug, ":")
  uarts.dec(debug, minute)
  uarts.putc(debug, ":")
  uarts.dec(debug, second)
  uarts.putc(debug, ",")
  uarts.putc(debug, lockType) ' a character derived from fixStat
  uarts.putc(debug, ",")

'  uarts.dec(debug, cntsAtPPS1)
'  uarts.putc(debug, ",")
'  uarts.dec(debug, accuracy)
'  uarts.putc(debug, ",")

  uarts.dec(debug, fixStat)   ' gpsFix taken from NAV-STATUS message
  uarts.putc(debug, ",")

  uarts.bin(debug, gpsValid, 3)   ' taken from NAV-TIMEGPS  
  uarts.putc(debug, ",")
  if ((gpsValid>>2 & %1) == %1)
    uarts.STR(DEBUG, string("GPS_VALID"))
  else
    uarts.STR(DEBUG, string("GPS_INVALID"))
  uarts.putc(debug, ",")

'  uarts.dec(debug, UBX.get_longitude)
'  uarts.putc(debug, ",")
'  uarts.dec(debug, UBX.get_latitude)
'  uarts.putc(debug, ",")
'  uarts.dec(debug, UBX.get_elevation)
'  uarts.putc(debug, ",")
  uarts.bin(debug, utcValid, 3)   ' taken from NAV-TIMEUTC  
  uarts.putc(debug, ",")
  if ((utcValid>>2 & %1) == %1)
    uarts.STR(DEBUG, string("UTC_VALID"))
  else
    uarts.STR(DEBUG, string("UTC_INVALID"))


PUB READ_RTC
  rtcAddr  := PEBBLE.READ_RTC_TIME
  rtcYear  := BYTE[rtcAddr][6]+2000
  rtcMonth := BYTE[rtcAddr][5]
  rtcDay   := BYTE[rtcAddr][4]
  rtcDow   := BYTE[rtcAddr][3]
  rtcHour  := BYTE[rtcAddr][2]
  rtcMinute:= BYTE[rtcAddr][1]
  rtcSecond:= BYTE[rtcAddr][0]

PUB SETUP_AND_LAUNCH_ADC(_sps, gA, gB, gC, gD, aMux, bMux, cMux, dMux) | sampleRateEnum, response
'' store system wide values for sps, gain, and INT/EXT 1 for EXT; 0 for INT
'' set the gain and voltage trim (0)
'' set sample rate and launch ADC cog
'' method that loads some variables and launches the ADC cog

  if adcCogId > 0   ' is the ADC cog already running
    cogstop(adcCogId)
    adcCogId := -1

                                                                           
  sampleRateEnum := lookdown(_sps:   250, 500, 1000, 2000, 2500, 4000, 5000, 8000, 10000)
  fsampc   := lookup(sampleRateEnum:   1,   2,    3,    4,    4,    5,    5,    0,     0)                      
  fsampf   := lookup(sampleRateEnum: 964, 964,  964,  964,  464,  964,  464,  964,   464)                      

  wdrcrVal := (FSAMPC << 13) | FSAMPF  ' write data rate control register value
  rdrcrVal := 0                        ' read data rate control register value
  wsicrVal := $0000_0000               ' write sampling instant control register value
  rsicrVal := 0                        ' read sampling instant contrl register value
  wcrVal   := %0_0_1_0_0_0_00          ' write configuration register
  rcrVal   := 0                        ' read configuration register

  gainA := PEBBLE.SET_GAIN(gA, CHANNEL_A)  <# $FF
  gainB := PEBBLE.SET_GAIN(gB, CHANNEL_B)  <# $FF
  gainC := PEBBLE.SET_GAIN(gC, CHANNEL_C)  <# $FF
  gainD := PEBBLE.SET_GAIN(gD, CHANNEL_D)  <# $FF

'  PEBBLE.SHORT_INPUTS(CHANNEL_A)
'  PEBBLE.SHORT_INPUTS(CHANNEL_B)
'  PEBBLE.SHORT_INPUTS(CHANNEL_C)
'  PEBBLE.SHORT_INPUTS(CHANNEL_D)

  PEBBLE.SET_MUXES(aMux, bMux, cMux, dMux)

  data1.byte[3] := "P"
  data1.byte[2] := "S"
  data1.byte[1] := VERSION
  data1.byte[0] := 0 ' this gets updated in each packet to reflect lock status
  
  data2.byte[3] := sampleRateEnum
  data2.byte[2] := (aMux << 3 | bMux << 2 | cMux << 1 | dMux ) & $0F
  data2.byte[1] := PEBBLE.GAIN_BINARY_VAL(gainA) << 4 | PEBBLE.GAIN_BINARY_VAL(gainB) 
  data2.byte[0] := PEBBLE.GAIN_BINARY_VAL(gainC) << 4 | PEBBLE.GAIN_BINARY_VAL(gainD) 

  bytefill(@adcBuffer, 0, LONGS_IN_BUFFER*4)
  adcCogId := cognew(@MAX11040K, @fifoSem)        ' Start the ADC read cog

  return adcCogId

PUB PROCESS_UART | idx, pkpA, pkpB, pkpC, cursor
' put the code necessary to process incoming UART commands here
{
    KILL                  = "K"
    SHUTDOWN_SYSTEM       = "D"
    SOFT_REBOOT_GUMSTIX   = "S"
    HARD_REBOOT_GUMSTIX   = "H"
    PUT_SYSTEM_TO_SLEEP   = "L"
    RATE                  = "R"
    SHORT                 = "T"
    TIRG_INTERVAL         = "I"
    QUERY                 = "Q"
    GAIN                  = "G"
    SOURCE                = "O"
    STOMP_DATA            = "W"
    LINE_TWO              = "2"
}
  
'  UARTS.STR(DEBUG, string(13,"Received command: "))
'  UARTS.PUTC(DEBUG, inUartBuf[0])

  
'  UARTS.STR(DEBUG, string(13,"inUartBuf: "))
'  repeat idx from 0 to 20
'    UARTS.HEX(DEBUG, inUartBuf[idx],2)
'    UARTS.PUTC(DEBUG, SPACE)
    
'  UARTS.STR(DEBUG, string(13,"inUartPtr: "))
'  repeat idx from 0 to 5
'    UARTS.HEX(DEBUG, inUartPtr[idx], 2)
'    UARTS.PUTC(DEBUG, SPACE)

'  UARTS.PUTC(DEBUG, CR)
  
  case inUartBuf[0]
    KILL :                 'K
      UARTS.STR(DEBUG, string(13,"$PSCMD, KILL"))
      PEBBLE.GUMSTIX_OFF

    SHUTDOWN_SYSTEM :      'D
      UARTS.STR(DEBUG, string(13,"$PSCMD, SHUTDOWN"))
      PAUSE_MS(12_000)
'      mainState := TURNING_OFF

    SOFT_REBOOT_GUMSTIX :  'S
      UARTS.STR(DEBUG, string(13,"$PSCMD, REBOOT"))
      PAUSE_MS(2000)

    HARD_REBOOT_GUMSTIX :  'H
      PEBBLE.GUMSTIX_OFF
      PAUSE_MS(500)
      PEBBLE.GUMSTIX_ON

    PUT_SYSTEM_TO_SLEEP :  'L
      PEBBLE.GUMSTIX_OFF
      PAUSE_MS(500)
      PEBBLE.GUMSTIX_ON

    RATE :                 'R
      UARTS.STR(DEBUG, string(" Changing sample rate to: "))
      sampleRate := NUM.FROMSTR(@inUartBuf[inUartPtr[1]],NUM#DDEC) 
      UARTS.DEC(DEBUG, sampleRate)
      UARTS.STR(DEBUG, string(13,"Stopping adcCog: "))
      UARTS.DEC(DEBUG, adcCogId)
      UARTS.STR(DEBUG, string(13,"Stopping slaveCog: "))
      UARTS.DEC(DEBUG, slaveCogId)
      cogStop(adcCogId)
      cogStop(slaveCogId)
      STORE_PARAMETERS_TO_SRAM
      START_ACQUISITION

    TRIG_INTERVAL :        'I
      UARTS.STR(DEBUG, string(" Changing triggering interval to: "))
      interval := NUM.FROMSTR(@inUartBuf[inUartPtr[1]],NUM#DDEC) 
      UARTS.DEC(DEBUG, interval)
      UARTS.STR(DEBUG, string(13,"Stopping adcCog: "))
      UARTS.DEC(DEBUG, adcCogId)
      UARTS.STR(DEBUG, string(13,"Stopping slaveCog: "))
      UARTS.DEC(DEBUG, slaveCogId)
      cogStop(adcCogId)
      cogStop(slaveCogId)
      STORE_PARAMETERS_TO_SRAM
      START_ACQUISITION

    QUERY :                ' Q
      PRINT_SYSTEM_PARAMETERS
      
    GAIN :                 'G
      UARTS.STR(DEBUG, string(" Changing gain to: "))
      gainA := NUM.FROMSTR(@inUartBuf[inUartPtr[1]],NUM#DDEC) 
      gainB := NUM.FROMSTR(@inUartBuf[inUartPtr[2]],NUM#DDEC) 
      gainC := NUM.FROMSTR(@inUartBuf[inUartPtr[3]],NUM#DDEC) 
      gainD := NUM.FROMSTR(@inUartBuf[inUartPtr[4]],NUM#DDEC) 
      UARTS.DEC(DEBUG, gainA)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, gainB)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, gainC)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, gainD)
      UARTS.STR(DEBUG, string(13,"Stopping adcCog: "))
      UARTS.DEC(DEBUG, adcCogId)
      UARTS.STR(DEBUG, string(13,"Stopping slaveCog: "))
      UARTS.DEC(DEBUG, slaveCogId)
      cogStop(adcCogId)
      cogStop(slaveCogId)
      STORE_PARAMETERS_TO_SRAM
      START_ACQUISITION

    SHORT :                'T
      UARTS.STR(DEBUG, string(" Shorting inputs on channel: "))
      gainA := NUM.FROMSTR(@inUartBuf[inUartPtr[1]],NUM#DDEC) 
      gainB := NUM.FROMSTR(@inUartBuf[inUartPtr[2]],NUM#DDEC) 
      gainC := NUM.FROMSTR(@inUartBuf[inUartPtr[3]],NUM#DDEC) 
      gainD := NUM.FROMSTR(@inUartBuf[inUartPtr[4]],NUM#DDEC) 
      
      
    SOURCE :               'O                                     
      UARTS.STR(DEBUG, string(" Changing input source to: "))
      sourceA := NUM.FROMSTR(@inUartBuf[inUartPtr[1]],NUM#DDEC) 
      sourceB := NUM.FROMSTR(@inUartBuf[inUartPtr[2]],NUM#DDEC) 
      sourceC := NUM.FROMSTR(@inUartBuf[inUartPtr[3]],NUM#DDEC) 
      sourceD := NUM.FROMSTR(@inUartBuf[inUartPtr[4]],NUM#DDEC) 
      UARTS.DEC(DEBUG, sourceA)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, sourceB)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, sourceC)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, sourceD)
      UARTS.STR(DEBUG, string(13,"Stopping adcCog: "))
      UARTS.DEC(DEBUG, adcCogId)
      UARTS.STR(DEBUG, string(13,"Stopping slaveCog: "))
      UARTS.DEC(DEBUG, slaveCogId)
      cogStop(adcCogId)
      cogStop(slaveCogId)
      STORE_PARAMETERS_TO_SRAM
      START_ACQUISITION

    STOMP_DATA :        'W
      pkpA := NUM.FROMSTR(@inUartBuf[inUartPtr[1]],NUM#DDEC)+1 
      pkpB := NUM.FROMSTR(@inUartBuf[inUartPtr[2]],NUM#DDEC)+1 
      pkpC := NUM.FROMSTR(@inUartBuf[inUartPtr[3]],NUM#DDEC)+1
      PEBBLE.OLED_COMMAND(%1100_0000)                   ' set cursor on first space of 2nd line      
      repeat idx from 0 to 15
        cursor := 0
        if pkpA > idx
          cursor |= %100
        if pkpB > idx
          cursor |= %010
        if pkpC > idx
          cursor |= %001
        PEBBLE.OLED_DATA(cursor)


    LINE_TWO :        '2
      inUartBuf[2+16] := 0                    ' make sure that we have a zero-terminated string that won't exceed OLED length
      PEBBLE.OLED_WRITE_LINE2(string("                "))  ' clear the line before writing to it.
      PEBBLE.OLED_WRITE_LINE2(@inUartBuf[inUartPtr[1]])    ' print the message  

    LINE_ONE :        '1
      inUartBuf[2+16] := 0                    ' make sure that we have a zero-terminated string that won't exceed OLED length
      PEBBLE.OLED_WRITE_LINE1(string("                "))  ' clear the line before writing to it.
      PEBBLE.OLED_WRITE_LINE1(@inUartBuf[inUartPtr[1]])    ' print the message  
                                               '  
    EUI :
      PRINT_EUI

    OTHER: 
      UARTS.STR(DEBUG, string(" Unrecognized command."))

PUB PRINT_EUI 
  bytefill(@euiAddr, 0, 8)
  euiAddr := PEBBLE.READ_EUI + 3' skip the first 3 bytes
  euiValue.byte[3] := byte[euiAddr]
  euiValue.byte[2] := byte[euiAddr+1]
  euiValue.byte[1] := byte[euiAddr+2]
  euiValue.byte[0] := 0

  euiMsg[0] := "S"
  euiMsg[1] := "N"
  euiMsg[2] := ":"
  euiMsg[3] := " "
  
  euiMsg[4] := PEBBLE.TO_HEX((BYTE[euiAddr] >> 4 & $F))
  euiMsg[5] := PEBBLE.TO_HEX((BYTE[euiAddr++] & $F))
  euiMsg[6] := ":"
  euiMsg[7] := PEBBLE.TO_HEX((BYTE[euiAddr] >> 4 & $F))
  euiMsg[8] := PEBBLE.TO_HEX((BYTE[euiAddr++] & $F))
  euiMsg[9] := ":"
  euiMsg[10] := PEBBLE.TO_HEX((BYTE[euiAddr] >> 4 & $F))
  euiMsg[11] := PEBBLE.TO_HEX((BYTE[euiAddr++] & $F))
  
  
  DISPLAY_ON_OLED(1,@euiMsg)
  UARTS.STR(DEBUG, string(13,"$PSEUI, "))
  UARTS.STR(DEBUG, @euiMsg[4])

'PUB READ_RTC_TIME
'  rtcAddr:= PEBBLE.READ_RTC_TIME

PUB GET_PARAMETERS_FROM_SRAM | response 
' collect parameters from the SRAM; this only works if we haven't lost power because SRAM
' is battery backed-up but will die eventually.  
  response := PEBBLE.READ_RTC_SRAM
  bytemove(@sramParms, response, 64)
  'repeat response from 0 to 63
  '  if response // 16 == 0
  '    UARTS.PUTC(DEBUG, CR)
  '    UARTS.DEC(DEBUG, response)
  '    UARTS.PUTC(DEBUG, TAB)
  '  UARTS.HEX(DEBUG, sramParms[response],2)
  '  UARTS.PUTC(DEBUG, " ")


  if sramParms[0] == "G" AND sramParms[1] == "P" AND strcomp(@sramParms[30], @versionMsg)' look to see if these data are valid
    UARTS.STR(DEBUG, string(13,"Parameters loaded from SRAM. "))
    UARTS.STR(DEBUG, @euiMsg[4])
    sampleRate   := sramParms[2] << 8 | sramParms[3]  ' bytes 2 and 3 are sampleRate
    gainA        := sramParms[4]                       
    gainB        := sramParms[5]                       
    gainC        := sramParms[6]                       
    gainD        := sramParms[7]                       
    sourceA      := sramParms[8]
    sourceB      := sramParms[9]
    sourceC      := sramParms[10]
    sourceD      := sramParms[11]
    interval     := sramParms[12]        '
    recordLength := sramParms[13]
    acqMode      := sramParms[14]
  else     ' here are the factory default params
    UARTS.STR(DEBUG, string(13,"SRAM parameters corrupt. "))
    UARTS.STR(DEBUG, @euiMsg[4])
    sampleRate   :=  1000  
    gainA        :=  1     
    gainB        :=  1     
    gainC        :=  1     
    gainD        :=  10    
    sourceA      :=  INT 
    sourceB      :=  INT 
    sourceC      :=  INT 
    sourceD      :=  INT 
    interval     :=  2        
    recordLength :=  10     
    acqMode      := CONT
    STORE_PARAMETERS_TO_SRAM
    UARTS.STR(DEBUG, string(13,"Wrote new parameters to SRAM."))

PUB STORE_PARAMETERS_TO_SRAM
  bytefill(@sramParms, 0, 64)

  sramParms[0]  := "G"
  sramParms[1]  := "P" ' look to see if these data are valid
  sramParms[2]  := sampleRate >> 8
  sramParms[3]  := sampleRate & $FF
  sramParms[4]  := gainA
  sramParms[5]  := gainB
  sramParms[6]  := gainC
  sramParms[7]  := gainD
  sramParms[8]  := sourceA
  sramParms[9]  := sourceB
  sramParms[10] := sourceC
  sramParms[11] := sourceD
  sramParms[12] := interval
  sramParms[13] := recordLength
  sramParms[14] := acqMode
  bytemove(@sramParms[30], @versionMsg, strsize(@versionMsg))

  'UARTS.STR(DEBUG, string(13,"Writing to SRAM:"))
  'UARTS.STR(DEBUG, @versionMsg)
  'UARTS.STR(DEBUG, string("."))
  'UARTS.DEC(DEBUG, strsize(@versionMsg))
  'UARTS.STR(DEBUG, string(" bytes."))
  

  PEBBLE.WRITE_RTC_SRAM(@sramParms)
  

PUB WRITE_DEFAULT_PARAMS_TO_SRAM
  sampleRate   := 1000
  gainA        :=    1
  gainB        :=    1
  gainC        :=    1
  gainD        :=   10
  sourceA      :=  INT
  sourceB      :=  INT
  sourceC      :=  INT
  sourceD      :=  INT
  interval     :=    2 
  recordLength :=   10
  acqMode      := CONT
  STORE_PARAMETERS_TO_SRAM
  UARTS.STR(DEBUG, string(13,"$PSMSG,Default paramaters stored to SRAM."))

PUB PRINT_SYSTEM_PARAMETERS
  
  ' PEBBLE.OLED_WRITE_LINE1(string("Getting Params.."))
  UARTS.STR(DEBUG, string(13, "$PSPARMS,"))
  UARTS.DEC(DEBUG, sampleRate)
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, gainA)
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, gainB)
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, gainC)
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, gainD)
  UARTS.PUTC(DEBUG, COMMA)
  if sourceA == INT
    UARTS.STR(DEBUG, string("INT"))
  if sourceA == EXT
    UARTS.STR(DEBUG, string("EXT"))
  UARTS.PUTC(DEBUG, COMMA)
  if sourceB == INT
    UARTS.STR(DEBUG, string("INT"))
  if sourceB == EXT
    UARTS.STR(DEBUG, string("EXT"))
  UARTS.PUTC(DEBUG, COMMA)
  if sourceC == INT
    UARTS.STR(DEBUG, string("INT"))
  if sourceC == EXT
    UARTS.STR(DEBUG, string("EXT"))
  UARTS.PUTC(DEBUG, COMMA)
  if sourceD == INT
    UARTS.STR(DEBUG, string("INT"))
  if sourceD == EXT
    UARTS.STR(DEBUG, string("EXT"))
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, interval)
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, recordLength)
  UARTS.PUTC(DEBUG, COMMA)
  case acqMode
    SLEEP : UARTS.STR(DEBUG, string("SLEEP"))
    TRIG  : UARTS.STR(DEBUG, string("TRIG"))
    CONT  : UARTS.STR(DEBUG, string("CONT"))
    OTHER : UARTS.STR(DEBUG, string("Unknow"))
  
PUB GET_GPS_LOCK | idx, response
  PEBBLE.OLED_WRITE_LINE2(string("Getting GPS Lock"))
  idx := 0

  repeat
    PET_WATCHDOG                 ' pet the watchdog
    edgeDetector := ((edgeDetector << 1) | INA[GPS_PPS] ) & %11    
    case edgeDetector 
      %01 : ' rising edge -> grab phsb
        idx++
        cntsTemp := CNT - PHSB
        UPDATE_TIME_AND_DATE
        PRINT_TIME_AND_DATE(TRUE)
        UARTS.PUTC(DEBUG, COMMA)
        UARTS.DEC(DEBUG, idx)
      %10 : ' falling edge now we can reset phsb since it won't increment until PPS goes high again
        PHSB := 0
      
    repeat
      response := UBX.READ_AND_PROCESS_BYTE(FALSE)
    until response == -1 OR response == UBX#RXMRAW
    
  until ((gpsValid>>2 & %1) == %1)  OR idx == 3'CONSTANT(3 * 60)

  if ((gpsValid>>2 & %1) == %1)
    UARTS.STR(DEBUG, string(13, "$PSMSG, GPS Locked."))
  else 
    UARTS.STR(DEBUG, string(13, "$PSMSG, Unable to get GPS lock."))
    PEBBLE.OLED_WRITE_LINE1(string(" Unable to get  "))
    PEBBLE.OLED_WRITE_LINE2(string("   GPS lock.    "))
  

PUB LAUNCH_SERIAL_COG
'' method that sets up the serial ports
  NUM.INIT
  UARTS.INIT
  UARTS.ADDPORT(DEBUG,    DEBUG_RX_FROM, DEBUG_TX_TO, -1, -1, 0, %000000, DEBUG_BAUD)    'Add DEBUG port
  UARTS.ADDPORT(GPS_PORT, GPS_RX_FROM,   GPS_TX_TO,   -1, -1, 0, %000000, GPS_BAUD)      'Add condor NMEA port
  UARTS.START
  serialCogId    := UARTS.GETCOGID                                               'Start the ports
  PAUSE_MS(300)

PUB CONFIG_GPS    
  ' we need some amount of delay between turning on the gps and speaking to it.  
  ubxBufferAddress := UBX.INIT(DEBUG, GPS_PORT)
  PEBBLE.OLED_WRITE_LINE1(string("Configuring GPS "))
  UBX.VERSION_POLL

  PEBBLE.OLED_WRITE_LINE2(string("Turning OFF NMEA"))
  PET_WATCHDOG
  UBX.TURN_OFF_NMEA

  PEBBLE.OLED_WRITE_LINE2(string("Turning ON RAW  "))
  PET_WATCHDOG
  UBX.TURN_ON_RAW

  PEBBLE.OLED_WRITE_LINE2(string("Turning ON PPS "))
  PET_WATCHDOG
  UBX.TURN_ON_PPS

PUB REBOOT_SYSTEM
  UARTS.STR(DEBUG, string(13, "$PSMSG, Rebooting system"))
  PEBBLE.OLED_ON
  PAUSE_MS(200)
  DISPLAY_ON_OLED(1,string("   REBOOTING    "))
  DISPLAY_ON_OLED(2,string("    SYSTEM      "))
  PAUSE_MS(5_000)
  REBOOT

PUB TEST_MAG_ACC | idx, measureTime
  PEBBLE.MAG_ACC_ON
  PEBBLE.LSM_INIT
  
  idx := 0
  measureTime := cnt + FULL_SPEED
  repeat
    PET_WATCHDOG
    UARTS.PUTC(DEBUG, 13)
    waitcnt(measureTime +=  CONSTANT(100_000_000)) ' attempt to get 25hz data
    'if idx // 20 == 0
    '  UARTS.STR(DEBUG, string(13, "ACCx",9,"ACCy",9,"ACCz",9,"MAGx",9,"MAGy",9,"MAGz",9,"TEMP",13))
    'idx++
    UARTS.DEC(DEBUG, PEBBLE.GET_ACC_X)
    UARTS.PUTC(DEBUG, TAB)
'    PAUSE_MS(1)
    UARTS.DEC(DEBUG, PEBBLE.GET_ACC_Y)
    UARTS.PUTC(DEBUG, TAB)
'    PAUSE_MS(1)
    UARTS.DEC(DEBUG, PEBBLE.GET_ACC_Z)
    UARTS.PUTC(DEBUG, TAB)
'    PAUSE_MS(1)

    PEBBLE.READ_MAG             ' we need to read the mag before the others are populated.  
    UARTS.DEC(DEBUG, PEBBLE.GET_MAG_X)
    UARTS.PUTC(DEBUG, TAB)
    UARTS.DEC(DEBUG, PEBBLE.GET_MAG_Y)
    UARTS.PUTC(DEBUG, TAB)
    UARTS.DEC(DEBUG, PEBBLE.GET_MAG_Z)
    UARTS.PUTC(DEBUG, TAB)
    UARTS.DEC(DEBUG, PEBBLE.READ_MAG_TEMP)

PUB CALIBRATE_SYSTEM_CLOCK | e_time, b_time, diff
  repeat 15
    waitpne(0,constant(|<GPS_PPS), 0)    ' wait for pin to go high
    e_time := cnt

    diff := e_time - b_time
    uarts.str(DEBUG,string(13,"  Number of cnts:   "))
    uarts.dec(DEBUG, diff)

    b_time := e_time
    PAUSE_MS(200)

PUB CAPTURE_VXCO_VALUES | i, t1, risingEdge, t2
' this method should run in its own cog
' setup counter A in this cog to count 20Mhz pulses
' this is used for discplining the VCXO
  CTRA := %01010 << 26 + EXT_OSC   ' counter A in POSedge mode
  FRQA := 1                             ' increment once per pulse

'  uarts.str(DEBUG,string(13,"Waiting for PPS before disclipining VCXO."))
  waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
  waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high

'  uarts.str(DEBUG,string(13, "  ---- Calibrating GPSDO  ----"))  

'  CALIBRATE_GPSDO

  dacValue := $7FFF       ' set to middle of the road value
  dacOld := dacValue
  uarts.str(DEBUG,string(13,"DAC start value: "))
  uarts.dec(DEBUG,dacValue)
  PEBBLE.WRITE_TO_DAC(dacValue)     ' put a middle of the road value in here to start with

  CALIBRATE_SYSTEM_CLOCK
   
  ' DAC values:20Mhz values range from 0:19_999_615 to 65024:20_000_244
  uarts.str(DEBUG,string(13,"Waiting for PPS before disclipining VCXO."))
  waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
  waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
  PHSA := 0

  i := 0
  repeat
    t1 := 0
    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    't1 -= cnt
    risingEdge  := PHSA    ' this capture of PHSA takes 1136 clock counts
    t1 += cnt
    if risingEdge < (GPSDO_REF+GPSDO_REF>>1)  ' could we have one or more PPS edges?
      vcxoError := (risingEdge - GPSDO_REF)
      dacNew := dacOld - K * vcxoError
      'temp := 25' READ_TC                                          ' read the temperature
    
      uarts.putc(DEBUG, 13)
      uarts.dec(DEBUG,i++)
      'uarts.putc(DEBUG, TAB)
      'SHOW_C(temp)                                             ' display in ?C
      uarts.putc(DEBUG, TAB)
      uarts.dec(DEBUG,  risingEdge)
      uarts.putc(DEBUG, TAB)
      uarts.dec(DEBUG,  vcxoError)
      uarts.putc(DEBUG, TAB)
      uarts.dec(DEBUG,  dacOld)
      uarts.putc(DEBUG, TAB)
      uarts.dec(DEBUG,  dacNew)
      uarts.putc(DEBUG, TAB)
      PEBBLE.WRITE_TO_DAC(dacNew)
'      PEBBLE.WRITE_TO_OLED(0, 1, oledClrLine)
'      PEBBLE.WRITE_TO_OLED(0, 1, string("What's up?"))
      

      
      dacOld := dacNew
     
    
    t2 := 0
    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    't2 -= cnt
    PHSA := 0    ' clearing PHSA takes 1136 clock counts
    t2 += cnt
    

PUB CALIBRATE_GPSDO | highDac, lowDac, highCount, lowCount, error
' uset CTRA briefly, to get a measurement of linear drift of the VCXO
' and to calculate a value for K our VCXO factor
  CTRA := %01010 << 26 + EXT_OSC   ' counter A in POSedge mode
  FRQA := 1                        ' increment once per pulse

  uarts.str(DEBUG,string(13,"Calibrating GPSDO."))
  uarts.str(DEBUG,string(13," DAC value   VCXO Meas."))

' get a couple readings from the GPSDO to set a linear frequncy/DAC relationship
  uarts.putc(DEBUG, 13)
  highDAC := $BFFF
'  highDAC := $FFFF
  PEBBLE.WRITE_TO_DAC(highDAC)  ' set DAC to 3/4 full-scale value
  PAUSE_MS(2000)       ' let it settle
  waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
  waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
  PHSA := 0

  repeat 20
    PET_WATCHDOG               ' do this every time through the loop.
    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    highCount := PHSA  ' an adjustment based on comparing prop counts to freq counter
    PHSA := 0
    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,highDAC)
    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,highCount)
    uarts.putc(DEBUG, 13)

  lowDAC := $3FFF
'  lowDAC := 0
  uarts.putc(DEBUG, 13)
  PEBBLE.WRITE_TO_DAC(lowDAC)  ' set DAC to 1/4 full-scale value
  PAUSE_MS(2000)       ' let it settle
  waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
  waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
  PHSA := 0

  repeat 20
    PET_WATCHDOG               ' do this every time through the loop.
    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    lowCount := PHSA  ' an adjustment based on comparing prop counts to freq counter
    PHSA := 0

    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,lowDAC)
    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,lowCount)
    uarts.putc(DEBUG, 13)

  k := (highDAC-lowDAC)/(highCount-lowCount)
  uarts.str(DEBUG,string(13,"K="))
  uarts.dec(DEBUG, k)
  uarts.putc(DEBUG, 13)

  ' now do best we can to predict a dacValue that will give smallest error
  dacValue := (GPSDO_REF-lowCount)* k + lowDAC
  UARTS.STR(DEBUG, string(13, "$PSMSG, Setting DAC value: "))
  uarts.dec(DEBUG,dacValue)
  PEBBLE.WRITE_TO_DAC(dacValue)     ' put a middle of the road value in here to start with
  uarts.putc(DEBUG, 13)

  repeat 
    PET_WATCHDOG               ' do this every time through the loop.
    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    lowCount := PHSA  ' an adjustment based on comparing prop counts to freq counter
    PHSA   := 0
    error  := (lowCount - GPSDO_REF)
    dacNew := dacOld - K * error
    PEBBLE.WRITE_TO_DAC(dacNew)
    dacOld := dacNew
     

    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,dacValue)
    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,lowCount)
    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,error)
    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,dacOld)
    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,dacNew)
    uarts.putc(DEBUG, 13)

  
PUB DISPLAY_ON_OLED(line,strPtr)
  if oledOn
    case line
      1 :PEBBLE.OLED_WRITE_LINE1(strPtr)
      2 :PEBBLE.OLED_WRITE_LINE2(strPtr)

PUB BOOT_CONSOLE | startTime, bPressed
' boot into console mode.  A button press here puts us into programming mode
' no button press and we timout and boot normally
' to exit console mode, another button press is required.
  PEBBLE.LEDS_ON                ' turn ON  LED2  indicating we are acquiring data
  DIRA[SLAVE_IRQ..SLAVE_SCLK] := 0        ' make sure this cog isn't holding SLAVE lines captive
  DIRA[SLAVE_IRQ] := 1          ' make this an output
  OUTA[SLAVE_IRQ] := 1          ' hold this high so the gumstix doesn't hunt for data
  PEBBLE.OLED_ON                ' turn on and init oled
  oledOn := TRUE
  PET_WATCHDOG

  PAUSE_MS(100)
  DISPLAY_ON_OLED(1,string("Boot Console    "))
  PEBBLE.LEDS_OFF                ' turn ON  LED2  indicating we are acquiring data
  PAUSE_MS(1000)
  DISPLAY_ON_OLED(2,string("Press to Enter  "))
  startTime := CNT
  bPressed  := FALSE
  
  repeat until bPressed OR (CNT - startTime > BOOT_TIMEOUT)
    PAUSE_MS(10)
    bPressed := BUTTON_PRESSED  ' check for button press
    PET_WATCHDOG

  if bPressed                   ' if button pressed enter programming mode
    PEBBLE.GUMSTIX_ON             ' sak preferes to have gumstix on right away
    PAUSE_MS(100)            '12345678901234567890
    DISPLAY_ON_OLED(1,string("Program Mode    "))
    PAUSE_MS(100)
    DISPLAY_ON_OLED(2,string("Press to Exit   "))
    LAUNCH_SERIAL_COG             ' handle the 2 serial ports- debug and GPS
    bPressed  := FALSE
    startTime := 0
    repeat until bPressed
      PAUSE_MS(10)
      startTime++
      if startTime == 100
        startTime := 0
        UARTS.STR(DEBUG,string(13,"$PSMSG, Program Mode."))
      bPressed := BUTTON_PRESSED  ' check for button press
      PET_WATCHDOG
    PEBBLE.GUMSTIX_OFF
    DISPLAY_ON_OLED(1,string("Booting         "))
    PAUSE_MS(100)
    DISPLAY_ON_OLED(2,string("Normally        "))
    PAUSE_MS(2000)
      
  ' if we get out of this loop we should proceed with normal boot
  PEBBLE.LEDS_OFF
  PEBBLE.OLED_OFF
  oledOn := FALSE
  
  if serialCogId > 0
    cogstop(serialCogId)
    serialCogId := -1
  DIRA[SLAVE_IRQ] := 0          ' make this an input

{PUB UPDATE_OFF_TIME
' method that sets the off time 30 seconds into the future
  offSec := rtcSecond + 30  '
  if offSec > 59
    offSec -= 60
    offMin := rtcMinute + 1
    if offMin > 59
      offMin -= 60
  else
    offMin := rtcMinute

  if clkfreq == FULL_SPEED
    UARTS.STR(DEBUG, string(13, "Offtime: "))
    UARTS.DEC(DEBUG, offMin)
    UARTS.PUTC(DEBUG,":")
    UARTS.DEC(DEBUG, offSec)
    
}    
PUB PAUSE_MS_SLEEP(ms)
  waitcnt(SLEEP_SPEED/1000 * mS + cnt)

PUB PAUSE_MS(mS)
  waitcnt(FULL_SPEED/1000 * mS + cnt)

DAT ' day in each month for leap and non-leap year
  leapTable   byte  0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30  ' leap year
  noLeapTable byte  0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30  ' NOT leap year

DAT 'SPI_SLAVE
'' this PASM code should watch the shared HUB memory ADC buffers and write each buffer full
'' as soon as it is available IF the gumstix is ready
  ' the next 2 line refers to the HUB storage where things sit before be shifted out to the gumstix
'  long auxBuffer2[128]
'  long auxDataToWrite, auxBuffer2[128]
'  long gpsDataToWrite, gpsBuffer[256]
'  long slaveBufCMD, slaveBuffer[LONGS_IN_SLAVE_BUFFER]       


GUMSTIX_SLAVE     org            0
          mov     slaveSemPtr,   par
          mov     fifoCntPtr,    par                    ' this points to the number of blocks in the FIFO?
          mov     slaveBasePtr,  par                    ' base address of the data
          add     fifoCntPtr,    #4
          add     slaveBasePtr,  #8

          
          rdlong  slaveSem,      slaveSemPtr            ' get a copy of the semaphore address
          call    #SETUP_SLAVE_PINS                     ' setup pins

:topOfBuffer
          mov     bufferCnt,     #BLOCKS_IN_BUFFER      ' keep track of how many blocks are in the entire buffer
          mov     slavePtr,      slaveBasePtr           ' make sure pointer to next sample is at the beginning of entire buffer

:waitForData    ' wait here until the ADC or GPS buffer has at least one block in it
          rdlong  stemp,         auxFlagPtr         wz  ' stemp=1 (Z=0) means data to write
   if_nz  call    #WRITE_AUX_DATA                       ' Z=1 if stemp=0
          rdlong  stemp,         gpsFlagPtr         wz  ' stemp=1 (Z=0) means data to write
   if_nz  call    #WRITE_GPS_DATA                       ' Z=1 if stemp=0
          rdlong  fifoCnt,       fifoCntPtr         wz  ' Z=1 if fifoCnt=0
   if_z   jmp     #:waitForData                         ' if Z==0 jump back up and wait some more


' at this point we've got at least one seismic block we can send. do some book keeping before getting started
          mov     sampsInBlock,  #LONGS_IN_BLOCK        ' number of times we should loop per block
          mov     sBits,         #32                    ' setup number of bits in write
          rdlong  soutData,      slavePtr               ' we know data are ready so get 1st sample
          rol     soutData,      #1                 wc  ' get highest bit of soutData

          waitpeq scsMask,       scsMask                ' wait for CS to be HIGH
          muxz    outa,          irqMask                ' lower controller FLAG to indicate buffer ready to be clocked out
          waitpne scsMask,       scsMask                ' wait for CS to be LOW
          muxnz   outa,          irqMask                ' raise controller FLAG once CS goes low


:oneBlock
' loop over buffer = 128 longs
:wLongLoop
  ' loop over this long
          muxc    outa,          somiMask               ' put bit onto SOMI
          waitpeq ssclkMask,     ssclkMask              ' wait for CLK to go high
          rol     soutData,      #1                 wc  ' get highest bit of soutData
          waitpne ssclkMask,     ssclkMask              ' wait for CLK to go low
          djnz    sBits,         #:wLongLoop        wz  ' Decrement loop count. Z=1 when sbits=0
  ' end loop over this long

          ' get ready for next sample
          'muxnz   outa,          somiMask               ' make sure MISO is low on exit
          add     slavePtr,      #4                     ' increment bufferPtr
          mov     sBits,         #32                    ' setup number of bits for next write
          rdlong  soutData,      slavePtr               ' now get the next sample to write
          rol     soutData,      #1                 wc  ' get highest bit of soutData
          djnz    sampsInBlock,  #:oneBlock
' end of loop over buffer = 128 longs
          muxnz   outa,          somiMask           wz  ' make sure MISO is low on exit

          call    #UPDATE_FIFO_CNT
          djnz    bufferCnt,     #:waitForData          ' jump back around

' we've reached the end of the entire HUB buffer; time to jump back to the top and reset pointers
          jmp     #:topOfBuffer                         ' jump up and wait for next buffer


{******************************** WRITE_LONG *******************************************************
Write 32 bits to gumstix.  
***************************************************************************************************}
{WRITE_LONG
          mov     sBits,         #32        wz   ' setup number of bits in write; set Z=0 
                                                 ' Z=0; Z flag is set (1) if Value equals zero

:woneBuffer
          rol     soutData,      #1         wc    ' get highest bit of outData
          muxc    outa,          somiMask         ' put bit onto MISO
          waitpeq ssclkMask,     ssclkMask        ' wait for CLK to go high
          waitpne ssclkMask,     ssclkMask        ' wait for CLK to go low
          djnz    sBits,         #:woneBuffer     ' Decrement loop count.

          muxz    outa,          somiMask         ' make sure MISO is low on exit

WRITE_LONG_ret    ret
}

{******************************** SETUP_SLAVE_PINS *******************************************************
Short function to setup the SPI and test pins.  Only used once and it's really
for cleaning up the code more than anything.
***************************************************************************************************}
SETUP_SLAVE_PINS

' Setup MISO pin: Preset low; set to output
          mov     stemp,         #1           wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxz    outa,          somiMask                ' Preset OUTA to low         "0"
          muxnz   dira,          somiMask               ' Set    DIRA to HIGH=OUTPUT "1"


' Setup SLAVE_IRQ pin: Preset high; set to output
          mov     stemp,         #1           wz         ' Z=0;
          muxnz   outa,          irqMask                 ' Preset OUTA to HIGH        "1"
          muxnz   dira,          irqMask                 ' Set    DIRA to HIGH=OUTPUT "1"

' Setup TESTPT pin: Preset low; set to output
'          mov     stemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
'          muxz    outa,          stestMask               ' Preset OUTA to low         "0"
'          muxnz   dira,          stestMask               ' Set    DIRA to HIGH=OUTPUT "1"

SETUP_SLAVE_PINS_ret    ret

{******************************** UPDATE_FIFO_CNT*****************************************************
Use this snippet to update the HUB long which contains the number of elements in the FIFO.
This needs a semaphore because this update involves a read, modify, write.  The same procedure
is used on the ADC cog.  
***************************************************************************************************}
UPDATE_FIFO_CNT
:waitForSlaveSem
          cmp     szero,         #0           wz         ' set Z=1 
'          xor     outa,         stestMask

          lockset slaveSem        wc
   if_c   jmp     #:waitForSlaveSem                     ' if adcSem = 0 loop 

          rdlong  stemp,         fifoCntPtr              ' copy number of blocks in buffer so we can add to it
          sub     stemp,         #1                      ' increment count
          wrlong  stemp,         fifoCntPtr              ' write the new value back to HUB
          lockclr slaveSem                              ' clear adcSem for others to use
'          xor     outa,          stestMask

UPDATE_FIFO_CNT_ret  ret

{******************************** WRITE_AUX_DATA*****************************************************
This method pulls AUX dat from HUB and spits it to the shared SPI_BUS
***************************************************************************************************}
WRITE_AUX_DATA
' we've got 512 bytes or 128 longs of gps data to send.  Only need to send one buffer

' bit of book-keeping before we start
          mov     auxPtr,        auxBasePtr             ' we always begin at the beginning of the GPS buffer

  ' single block write
          mov     sampsInBlock,  #LONGS_IN_BLOCK        ' number of times we should loop per block
          mov     sBits,         #32                wz  ' setup number of bits in write
          rdlong  soutData,      auxPtr                 ' get long containing GPS data from HUB
          rol     soutData,      #1                 wc  ' get highest bit of soutData


          waitpeq scsMask,       scsMask                ' wait for CS to be HIGH
          muxz    outa,          irqMask                ' lower controller FLAG to indicate buffer ready to be clocked out
          waitpne scsMask,       scsMask                ' wait for CS to be LOW
          muxnz   outa,          irqMask                ' raise controller FLAG once CS goes low

:auxLoop1
    ' write one long
          muxc    outa,          somiMask               ' put bit onto SOMI
          waitpeq ssclkMask,     ssclkMask              ' wait for CLK to go high
          rol     soutData,      #1                 wc  ' get highest bit of soutData
          waitpne ssclkMask,     ssclkMask              ' wait for CLK to go low
          djnz    sBits,         #:auxLoop1         wz  ' Decrement loop count. Z=1 when sbits=0
    ' end write one long

    ' get ready for next Long
          add     auxPtr,        #4                     ' increment bufferPtr
          mov     sBits,         #32                    ' setup number of bits in write
          rdlong  soutData,      auxPtr                 ' get long containing GPS data from HUB
          rol     soutData,      #1                 wc  ' get highest bit of soutData
          
          djnz    sampsInBlock,  #:auxLoop1
  ' end of single block write
          

' declare buffers written  
          wrbyte  sfalse,        auxFlagPtr             ' indicate the GPS buffer if empty

' setup things for next time
          mov     auxPtr,        auxBasePtr             ' we always begin at the beginning of the GPS buffer

WRITE_AUX_DATA_ret  ret

{******************************** WRITE_GPS_DATA*****************************************************
This method pulls GPS dat from HUB and spits it to the shared SPI_BUS
***************************************************************************************************}
WRITE_GPS_DATA
' we've got 1024 bytes or 256 longs of gps data to send.  So we'll do it in two parts.  One then the next


' bit of book-keeping before we start
          mov     gpsPtr,        gpsBasePtr             ' we always begin at the beginning of the GPS buffer
          mov     gpsBufCnt,     #2                     ' we need to send 2 buffers

' at this point we've got at two gps blocks to send
:gpsLoop2
  ' single block write
          mov     sampsInBlock,  #LONGS_IN_BLOCK        ' number of times we should loop per block
          mov     sBits,         #32                wz  ' setup number of bits in write
          rdlong  soutData,      gpsPtr                 ' get long containing GPS data from HUB
          rol     soutData,      #1                 wc  ' get highest bit of soutData


          waitpeq scsMask,       scsMask                ' wait for CS to be HIGH
          muxz    outa,          irqMask                ' lower controller FLAG to indicate buffer ready to be clocked out
          waitpne scsMask,       scsMask                ' wait for CS to be LOW
          muxnz   outa,          irqMask                ' raise controller FLAG once CS goes low

:gpsLoop1
    ' write one gps long
          muxc    outa,          somiMask               ' put bit onto SOMI
          waitpeq ssclkMask,     ssclkMask              ' wait for CLK to go high
          rol     soutData,      #1                 wc  ' get highest bit of soutData
          waitpne ssclkMask,     ssclkMask              ' wait for CLK to go low
          djnz    sBits,         #:gpsLoop1         wz  ' Decrement loop count. Z=1 when sbits=0
    ' end write one gps long

    ' get ready for next gpsLong
          add     gpsPtr,        #4                     ' increment bufferPtr
          mov     sBits,         #32                    ' setup number of bits in write
          rdlong  soutData,      gpsPtr                 ' get long containing GPS data from HUB
          rol     soutData,      #1                 wc  ' get highest bit of soutData
          
          djnz    sampsInBlock,  #:gpsLoop1
  ' end of single block write
          
          djnz    gpsBufCnt,     #:gpsLoop2
' end of all gps blocks

' declare buffers written  
          wrbyte  sfalse,        gpsFlagPtr             ' indicate the GPS buffer if empty

' setup things for next time
          mov     gpsPtr,        gpsBasePtr             ' we always begin at the beginning of the GPS buffer
          mov     gpsBufCnt,     #2                     ' we need to send 2 buffers

WRITE_GPS_DATA_ret  ret

{******************************** WRITE_GPS_LONG *******************************************************
Write 32 bits to gumstix.  
***************************************************************************************************}
{
          mov     sBits,        #8        wz   ' setup number of bits in write; set Z=0 
          mov     stemp,        gpsLong 
          shl     gpsLong,      #24
          
:loop1
          waitpne ssclkMask,    ssclkMask        ' wait for CLK to go low
          rol     gpsLong,      #1         wc    ' get highest bit of outData
          muxc    outa,         somiMask         ' put bit onto MISO
          waitpeq ssclkMask,    ssclkMask        ' wait for CLK to go high
          djnz    sBits,        #:loop1          ' Decrement loop count.

          mov     sBits,        #8        wz     ' setup number of bits in write; set Z=0 
          mov     gpsLong,      stemp
          shl     gpsLong,      #16
          
:loop2
          waitpne ssclkMask,    ssclkMask        ' wait for CLK to go low
          rol     gpsLong,      #1         wc    ' get highest bit of outData
          muxc    outa,         somiMask         ' put bit onto MISO
          waitpeq ssclkMask,    ssclkMask        ' wait for CLK to go high
          djnz    sBits,        #:loop2          ' Decrement loop count.
                                                 
          mov     sBits,        #8        wz     ' setup number of bits in write; set Z=0 
          mov     gpsLong,      stemp
          shl     gpsLong,      #8
          
:loop3
          waitpne ssclkMask,    ssclkMask        ' wait for CLK to go low
          rol     gpsLong,      #1         wc    ' get highest bit of outData
          muxc    outa,         somiMask         ' put bit onto MISO
          waitpeq ssclkMask,    ssclkMask        ' wait for CLK to go high
          djnz    sBits,        #:loop3          ' Decrement loop count.

          mov     sBits,        #8        wz     ' setup number of bits in write; set Z=0 
          mov     gpsLong,      stemp
          
:loop4
          waitpne ssclkMask,    ssclkMask        ' wait for CLK to go low
          rol     gpsLong,      #1         wc    ' get highest bit of outData
          muxc    outa,         somiMask         ' put bit onto MISO
          waitpeq ssclkMask,    ssclkMask        ' wait for CLK to go high
          djnz    sBits,        #:loop4          ' Decrement loop count.

          muxz    outa,         somiMask         ' make sure MISO is low on exit

WRITE_GPS_LONG_ret  ret
}

{**************************************************************************************************
' Pin Masks
***************************************************************************************************}

scsMask      long            |< SLAVE_CS               '
ssclkMask    long            |< SLAVE_SCLK
somiMask     long            |< SLAVE_SOMI
simoMask     long            |< SLAVE_SIMO
irqMask      long            |< SLAVE_IRQ 
'stestMask    long            |< RADIO_CS

fiveEight    long            $5858_5858
sixsix       long            $6666_6666
auxFlagPtr   long            0
auxBasePtr   long            0
auxPtr       long            0
gpsFlagPtr   long            0
gpsBasePtr   long            0
gpsPtr       long            0
gpsBufCnt    long            0
gpsInBlock   long            0
sampsInBlock long            0
szero        long            0
byte1Mask    long            $FF00_0000
byte2Mask    long            $00FF_0000
byte3Mask    long            $0000_FF00
byte4Mask    long            $0000_00FF
sfalse       long            0

byte1            res             1
byte2            res             1
byte3            res             1
byte4            res             1
gpsLong          res             1
slaveSemPtr      res             1 
slaveSem         res             1
fifoCntPtr       res             1
fifoCnt          res             1
slaveBasePtr     res             1
slavePtr         res             1
bufferCnt        res             1
stemp            res             1
sBits            res             1
sindex           res             1
soutData         res             1

                 FIT 496

DAT 'ADC_PASM
MAX11040K         org         0
          mov     adcSemPtr,     par
          mov     elementsPtr,   par                    ' where are we in the buffer?
          mov     bufferBasePtr, par                    ' where is the start of the buffer? 
          add     elementsPtr,   #4
          add     bufferBasePtr, #8

          rdlong  adcSem,        adcSemPtr            ' get a copy of the semaphore address

          call    #SETUP_PINS                           ' lower CS line

':adcLoop
'          xor     outa,          testMask
'          jmp     #:adcLoop
          
          call    #SETUP_ADC_REGS

' take care of a bit of book-keeping; (re)initializing things
:bufferLoop
          mov     blockCount,    #BLOCKS_IN_BUFFER       ' keep track of how many blocks are full; init here
          mov     bufferPtr,     bufferBasePtr           ' get copy of the start of the buffer


:waitForSpace     ' wait here until there is at least one free space in queue
          rdlong  atemp,         elementsPtr
          cmp     atemp,         #BLOCKS_IN_BUFFER wz   ' if temp==BLOCKS_IN_BUFFER set Z=1
   if_z   jmp     #:waitForSpace                        ' finished<>0 ->Z=0;   move on


' if we made it here, there must be room in the queue so let's fill a block (512 bytes)
          mov     cksum,         azero                   ' zero out cksum register

' first 2 things into the 512 byte buffer are data1 and data2
' followed by 
          mov     slotsInBlock,  #SLOTS_IN_BLOCK        ' number of time slices in each block

          and     data1,         data1Mask              ' mask of upper 3 bytes of data1
          rdlong  atemp,         validPtr               ' get current value of validStatus
          or      data1,         atemp                  ' fold these to bit fields together
          wrlong  data1,         bufferPtr
          xor     cksum,         data1                  ' add this value to cksum
          add     bufferPtr,     #4

          wrlong  data2,         bufferPtr
          xor     cksum,         data2                  ' add this value to cksum
          add     bufferPtr,     #4

:blockLoop
          waitpeq azero,         drdyMask               ' wait for DRDY to go LOW
          cmp     slotsInBlock,  #SLOTS_IN_BLOCK   wz   ' is this the first sample?
   if_z   mov     cntsAtStart,   cnt                    ' get cnts at start          

          call    #LOWER_CS                             ' lower CS line

          ' write RDR
          mov     data,         #RDR                    ' put the address into outLong
          call    #WRITE_BYTE                           ' Send it
          
          ' read Z connected to RED/BROWN phone
          call    #READ_SAMPLE  ' sample is now in data 
          'mov     data,         aaaa_aaaa'#$A
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          ' read N/S connected to ORANGE/YELLOW
          call    #READ_SAMPLE  ' sample is now in data
          'mov     data,         ffff_ffff'#$B
          call    #SWAP_AND_WRITE_LONG_TO_HUB
                                
          ' read E/W connected to BLUE/GREEN
          call    #READ_SAMPLE  ' sample is now in data
          'mov     data,         #$C
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          ' read D                                      ' we need to read this 4 sample even
          call    #READ_SAMPLE  ' sample is now in data
          'mov     data,         #$D
          mov     data,         cnt
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          ' raise CS
          call    #RAISE_CS                             ' raise CS line
          
          djnz    slotsInBlock, #:blockLoop

' now we've finished one complete block.  Get the meta-data associated with this block
          mov     cntsAtEnd,    cnt
          
' wait here until we can lock the share GPS memory
:WaitForGPS
          lockset gpsSem        wc
'          xor     outa,         testMask
   if_c   jmp     #:WaitForGPS               ' if adcSem = 0 loop 

          mov     atemp,        timeDatePtr

          rdlong  data,         atemp        ' get ymd from main cog
          add     atemp,        #4
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          rdlong  data,         atemp        ' get hms from main cog
          add     atemp,        #4
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          rdlong  data,         atemp        ' get cnts@PPS from main cog
          add     atemp,        #4
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          lockclr gpsSem                     ' clear gpsSem for others to use

          mov     data,         cntsAtStart
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          mov     data,         cntsAtEnd
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          mov     atemp,        cksum           
          shr     atemp,        #8              
          xor     cksum,        atemp
          shr     atemp,        #8
          xor     cksum,        atemp
          shr     atemp,        #8
          xor     cksum,        atemp

          and     cksum,        cksumMask
          mov     data,         euiValue
          or      data,         cksum

          call    #SWAP_AND_WRITE_LONG_TO_HUB

          
' we've filled one block so lets update shared variable blocksInFIFO
          call    #INCREMENT_ELEMENTS

' now jump back up to see if there's any free space for another block of data
          djnz    blockCount,   #:waitForSpace

' once we've filled all blocks in buffer, we need to point things back to the top of buffer
          jmp     #:bufferLoop

:stop     waitpeq sclkMask,sclkMask
          jmp     #:stop          

{******************************** INCREMENT_ELEMENTS **********************************************
Function that gets the shared semaphore, increments the shared variable:
elementsPtr
then releases the semaphore
***************************************************************************************************}
INCREMENT_ELEMENTS

          cmp     azero,         #0           wz         ' set Z=1 
'          muxz    outa,          testMask
          
:WaitForSem
          lockset adcSem        wc
   if_c   jmp     #:WaitForSem                          ' if adcSem = 0 loop 

          rdlong  atemp,        elementsPtr             ' copy number of blocks in buffer so we can add to it
          add     atemp,        #1                      ' increment count
          wrlong  atemp,        elementsPtr             ' write the new value back to HUB
          lockclr adcSem                                ' clear adcSem for others to use
'          xor     outa,          testMask

INCREMENT_ELEMENTS_ret      ret

{******************************** WRITE_LONG_TO_HUB *******************************************************
function that will swap the endianness of the long and write it to hub then increment the
hub pointer
***************************************************************************************************}
WRITE_LONG_TO_HUB
                                                ' soutData = b3 b2 b1 b0
          wrlong  data,         bufferPtr    ' copy it back to hub in new location
          add     bufferPtr,    #4

WRITE_LONG_TO_HUB_ret    ret
{******************************** SWAP_AND_WRITE_LONG_TO_HUB *******************************************************
function that will swap the endianness of the long and write it to hub then increment the
hub pointer
***************************************************************************************************}
SWAP_AND_WRITE_LONG_TO_HUB
                                                ' soutData = b3 b2 b1 b0
          mov     swapTemp,     endianMask      '       t1 = 00 FF 00 FF
          ror     data,         #8              ' soutData = b0 b3 b2 b1
          and     swapTemp,     data            '       t1 = 00 b3 00 b1
          xor     data,         swapTemp        ' soutData = b0 00 b2 00
          rol     swapTemp,     #16             '       t1 = 00 b1 00 b3
          or      data,         swapTemp        ' soutData = b0 b1 b2 b3 

          wrlong  data,         bufferPtr       ' copy it back to hub in new location
          xor     cksum,        data            ' add this value to cksum
          add     bufferPtr,    #4

SWAP_AND_WRITE_LONG_TO_HUB_ret    ret
{******************************** SETUP_PINS *******************************************************
Short function to setup the SPI and test pins.  Only used once and it's really
for cleaning up the code more than anything.
***************************************************************************************************}
SETUP_PINS
' Setup CS pin: Preset high; set to output
          mov     atemp,         #1          wz         ' Z=0; 
          muxnz   outa,          csMask                 ' Preset OUTA to high        "1"
          muxnz   dira,          csMask                 ' Set    DIRA to HIGH=OUTPUT "1"

' Setup SCLK pin: Preset high; set to output
          mov     atemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxnz   outa,          sclkMask               ' Preset OUTA to high        "1"
          muxnz   dira,          sclkMask               ' Set    DIRA to HIGH=OUTPUT "1"

' Setup MOSI pin: Preset low; set to output
          mov     atemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxz    outa,          mosiMask               ' Preset OUTA to low         "0"
          muxnz   dira,          mosiMask               ' Set    DIRA to HIGH=OUTPUT "1"

'' Setup TESTPT pin: Preset low; set to output
'          mov     atemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
'          muxz    outa,          testMask               ' Preset OUTA to low         "0"
'          muxnz   dira,          testMask               ' Set    DIRA to HIGH=OUTPUT "1"

SETUP_PINS_ret    ret


{******************************** SETUP_ADC_REGS *******************************************************
Short function to setup the ADC register values- sample rate, etc
CS is HIGH on entry
CS is HIGH on exit
***************************************************************************************************}
SETUP_ADC_REGS
' reset ADC
          call    #LOWER_CS                             ' lower CS line
          mov     data,         #WCR                    ' put the address into outLong
          call    #WRITE_BYTE                           ' Send it
          
          mov     data,         #%0_1_0_0_0_0_00        ' reset to be written to control register     
          call    #WRITE_BYTE                           ' Send it
          call    #RAISE_CS                             ' raise CS line
          
' set ADC to 24-bit data
          call    #LOWER_CS                             ' lower CS line
          mov     data,         #WCR                    ' put the address into outLong
          call    #WRITE_BYTE                           ' Send it
          
          mov     data,         #%0_0_1_0_0_0_00        ' set 24-bit enable     
          call    #WRITE_BYTE                           ' Send it
          call    #RAISE_CS                             ' raise CS line
  
' setup sample rate
          call    #LOWER_CS                             ' lower CS line
          mov     data,         #WDRCR                  ' put the address into outLong
          call    #WRITE_BYTE                           ' Send it
          
          mov     data,         wdrcrVal                ' send data rate     
          call    #WRITE_WORD                           ' Send it
          call    #RAISE_CS                             ' raise CS line

' read sample rate
{ removed the following because we've gotten past this and there's no
  easy way to put this value into HUB at the moment
          call    #LOWER_CS                             ' lower CS line
          mov     data,         #RDRCR                  ' put the address into outLong
          call    #WRITE_BYTE                           ' Send it
          
          call    #READ_WORD                            ' Send it
          call    #RAISE_CS                             ' raise CS line
          wrlong  data,         responsePtr
}
SETUP_ADC_REGS_ret    ret

{******************************** WRITE_BYTE *******************************************************
This command will write the LOWEST 8 BITS of data MSB to the DATA line.  ADC latches data on falling
edge.  This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
WRITE_BYTE
          mov     bits1,        #8         wz   ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero
          rol     data,         #24             ' rotate data left so byte of interest is in upper 8 bits

:wbyteLoop
          muxnz   outa,         sclkMask        ' RAise clock Pin
          rol     data,         #1         wc   ' shift data left 1; C=bit shifted out
          muxc    outa,         mosiMask        ' Set OUTA[mosiMask]=C
          xor     outa,         sclkMask        ' lower the clock line
          djnz    bits1,        #:wbyteLoop     ' Decrement loop count.


WRITE_BYTE_ret    ret


{******************************** WRITE_WORD *******************************************************
This command will write the LOWEST 16 BITS of data MSB to the DATA line.  ADC latches data on falling
edge.  This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
WRITE_WORD
          mov     bits1,        #16        wz   ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero
          rol     data,         #16             ' rotate data left so byte of interest is in upper 8 bits

:wwordLoop
          muxnz   outa,         sclkMask        ' Rause clock Pin
          rol     data,         #1         wc   ' shift data left 1; C=bit shifted out
          muxc    outa,         mosiMask        ' Set OUTA[mosiMask]=C
          xor     outa,         sclkMask        ' lower the clock line
          djnz    bits1,        #:wwordLoop     ' Decrement loop count.

WRITE_WORD_ret    ret


{******************************** READ_WORD *******************************************************
This command will read 16 BITS of data on DATA line.  ADC latches data on falling
edge.  This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
READ_WORD
          mov     bits1,        #16        wz   ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero
          mov     data,         azero           ' clear out input register     

:rwordLoop
          muxnz   outa,         sclkMask        ' Rause clock Pin
          test    misoMask,     ina        wc   ' C=INA[misoMask]
          rcl     data,         #1              ' shift C left into data
          xor     outa,         sclkMask        ' lower the clock line
          djnz    bits1,        #:rwordLoop     ' Decrement loop count.

READ_WORD_ret    ret

{******************************** READ_SAMPLE*******************************************************
This command will read 24 BITS of data on DATA line.  ADC latches data on falling
edge.  This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
READ_SAMPLE
          mov     bits1,        #24        wz   ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero
          mov     data,         azero           ' clear out input register     

:rSampleLoop
          muxnz   outa,         sclkMask        ' Rause clock Pin
          test    misoMask,     ina        wc   ' C=INA[misoMask]
          rcl     data,         #1              ' shift C left into data
          xor     outa,         sclkMask        ' lower the clock line
          djnz    bits1,        #:rSampleLoop   ' Decrement loop count.

          ' no longer need to shift left then right.  Our samples are 24 bits and we are using the
          ' upper 8 bits for channel number and gain
          shl     data,         #8              ' shift left so bit 23 moves into bit 31
          sar     data,         #8              ' shift back right while preserving the sign
READ_SAMPLE_ret    ret

{******************************** WRITE_DATA_TO_HUB *******************************************************
This command writes the recently collected data and advances the pointers.
This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
WRITE_DATA_TO_HUB
          wrlong  data,         bufferPtr               ' write A0 sample to buffer
          mov     data,         cnt                     ' grab current cnt
          add     bufferPtr,    #4                      ' increment bufferPtr
          sub     data,         startCnts               ' data1=data1-startCnts
          wrlong  data,         bufferPtr               ' write current
          add     bufferPtr,    #4                      ' increment bufferPtr

WRITE_DATA_TO_HUB_ret    ret

{******************************** WRITE_CNT_TO_HUB *******************************************************
This command writes the recently collected data and advances the pointers.
This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
WRITE_CNT_TO_HUB
          wrlong  data,         bufferPtr               ' write A0 sample to buffer
          mov     data,         cnt                     ' grab current cnt
          add     bufferPtr,    #4                      ' increment bufferPtr
          sub     data,         startCnts               ' data1=data1-startCnts
          wrlong  data,         bufferPtr               ' write current
          add     bufferPtr,    #4                      ' increment bufferPtr

WRITE_CNT_TO_HUB_ret    ret

{******************************** LOWER_CS *********************************************************
This command simply lowers the CS line.  Nothing else.
***************************************************************************************************}
LOWER_CS
          cmp     azero,        azero       wz      ' set Z=1
          muxnz   outa,         csMask              ' set OUTA[csMask]=0
LOWER_CS_ret      ret

{******************************** RAISE_CS *********************************************************
This command simply raises the the SCLK and CS line.  Nothing else.
***************************************************************************************************}
RAISE_CS
          cmp     azero,        azero       wz  ' set Z=1
          muxz    outa,         sclkMask        ' set OUTA[clkMask]=1
          muxz    outa,         csMask          ' set OUTA[csMask]=1
RAISE_CS_ret      ret



{******************************** WAIT_FOR_PPS*****************************************************
A couple of lines used to wait for a rising PPS.
***************************************************************************************************}
WAIT_FOR_PPS
          waitpne   ppsMask, ppsMask         ' wait for ppsMask to go LOW; fall through if ppsMask is low
          waitpeq   ppsMask, ppsMask         ' wait for ppsMask to go HIGH

WAIT_FOR_PPS_ret  ret
{**************************************************************************************************
' Pin Masks
***************************************************************************************************}

csMask      long            |< ADC_CS               ' ADC CS line
drdyMask    long            |< ADC_DRDYOUT           
sclkMask    long            |< ADC_SCLK
misoMask    long            |< ADC_MISO
mosiMask    long            |< ADC_MOSI
ppsMask     long            |< GPS_PPS
'testMask    long            |< ADC_TEST

' Variables being passed in
samples          long            0
wdrcrVal         long            0
rdrcrVal         long            0
wsicrVal         long            0
rsicrVal         long            0
wcrVal           long            0
rcrVal           long            0
fsampc           long            0
fsampf           long            0
three            long            3
two              long            2
one              long            1
azero            long            0
codeA            long            0
codeB            long            0
codeC            long            0
codeD            long            0
blockCount       long            0
ymdAtEnd         long            0    ' ymd at end
hmsAtEnd         long            0    ' hms at end
cntsAtPPS2       long            0    ' cnts @ PPS
cntsAtStart      long            0    ' cnts @ start
cntsAtEnd        long            0    ' cnts @ end
cksum            long            0    ' 
timeDatePtr      long            0
validPtr         long            0
gpsSem           long            0

data1            long            0    
data2            long            0    
euiValue         long            0
endianMask       long            $00FF_00FF         ' used for byte swap
data1Mask        long            $FFFF_FF00         ' mask used on data1 to update clock status
cksumMask        long            $0000_00FF         ' use this to mask off lowest 8 bits of cksum
ffff_ffff        long            $FFFF_FFFF
aaaa_aaaa        long            $aaaa_aaaa

swapTemp         res             1
atemp            res             1
adcSemPtr        res             1
adcSem           res             1
elementsPtr      res             1
bufferPtr        res             1
bufferBasePtr    res             1
slotsInBlock     res             1
startCnts        res             1
curCnts          res             1
data             res             1
dataA            res             1
dataB            res             1
dataC            res             1
dataD            res             1
bits1            res             1
a0               res             1
a1               res             1
a2               res             1
a3               res             1
                 FIT 496

DAT 'SPI_MASTER                 
SPI_MASTER        org            0

          call    #SETUP_MASTER_PINS                           ' lower CS line
':toggle
'          xor     outa,          mtestMask
'          jmp     #:toggle


' take care of a bit of book-keeping; (re)initializing things
' set pointer to beginning of outBuffer, reset 
:mblockLoop
          mov     mindex,         mBytesInBlock        ' keep track of how many blocks are full; init here

{Here's what the master is doing...
'          waitpeq scsMask,       scsMask                ' wait for CS to be HIGH
'          muxz    outa,          irqMask                ' lower controller FLAG to indicate buffer ready to be clocked out
'          waitpne scsMask,       scsMask                ' wait for CS to be LOW

'          muxnz   outa,          irqMask                ' raise controller FLAG once CS goes low
}

          call    #M_RAISE_CS                           ' raise CS line to start
          waitpne mirqMask,       mirqMask              ' wait for IRQ to go LOW
          call    #M_LOWER_CS                           ' lower CS line

          nop
          nop
          nop

:masterLoop
          ' get next long to shift out
          call    #M_WRITE_BYTE

          djnz    mindex,        #:masterLoop

          call    #M_RAISE_CS                             ' raise CS line


          jmp     #:mblockLoop          

{******************************** WRITE_WORD *******************************************************
This command will write the LOWEST 16 BITS of data MSB to the DATA line.  ADC latches data on falling
edge.  This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
M_WRITE_BYTE
          mov     moutData,     #$99'mindex
          shl     moutData,     #24             ' shift things over to MSBit is in bit 31
          mov     mBits,        #8        wz    ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero
          nop
          nop
          nop
:wByteLoop
          rol     moutData,     #1         wc   ' shift data left 1; C=bit shifted out
          muxc    outa,         mmosiMask       ' Set OUTA[mosiMask]=C
          muxnz   outa,         msclkMask       ' Rause clock Pin
          nop
          nop
          xor     outa,         msclkMask       ' lower the clock line

          djnz    mBits,        #:wByteLoop     ' Decrement loop count.
          muxz    outa,         mmosiMask       ' make sure MOSI is low on exit
          muxz    outa,         msclkMask       ' make sure MOSI is low on exit

M_WRITE_BYTE_ret    ret


{******************************** SETUP_PINS *******************************************************
Short function to setup the SPI and test pins.  Only used once and it's really
for cleaning up the code more than anything.
***************************************************************************************************}
SETUP_MASTER_PINS
' Setup CS pin: Preset high; set to output
          mov     mtemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxnz   outa,          mcsMask                 ' Preset OUTA to high        "1"
          muxnz   dira,          mcsMask                 ' Set    DIRA to HIGH=OUTPUT "1"

' Setup SCLK pin: Preset high; set to output
          mov     mtemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxz    outa,          msclkMask               ' Preset OUTA to low         "0"
          muxnz   dira,          msclkMask               ' Set    DIRA to HIGH=OUTPUT "1"

' Setup MOSI pin: Preset low; set to output
          mov     mtemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxz    outa,          mmosiMask               ' Preset OUTA to low         "0"
          muxnz   dira,          mmosiMask               ' Set    DIRA to HIGH=OUTPUT "1"

' Setup TESTPT pin: Preset low; set to output
'          mov     mtemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
'          muxz    outa,          mtestMask               ' Preset OUTA to low         "0"
'          muxnz   dira,          mtestMask               ' Set    DIRA to HIGH=OUTPUT "1"

SETUP_MASTER_PINS_ret    ret

{******************************** LOWER_CS *********************************************************
This command simply lowers the CS line.  Nothing else.
***************************************************************************************************}
M_LOWER_CS
          cmp     mzero,        mzero        wz      ' set Z=1
          muxnz   outa,         mcsMask              ' set OUTA[csMask]=0
M_LOWER_CS_ret      ret

{******************************** RAISE_CS *********************************************************
This command simply raises the the SCLK and CS line.  Nothing else.
***************************************************************************************************}
M_RAISE_CS
          cmp     mzero,        mzero        wz  ' set Z=1
          muxz    outa,         mcsMask          ' set OUTA[csMask]=1
M_RAISE_CS_ret      ret

                                     
{**************************************************************************************************
' Pin Masks
***************************************************************************************************}

mcsMask      long            |< SLAVE_CS               '
msclkMask    long            |< SLAVE_SCLK
mmisoMask    long            |< SLAVE_SOMI
mmosiMask    long            |< SLAVE_SIMO
mirqMask     long            |< SLAVE_IRQ
mzero        long            0
mBytesInBlock     long       BYTES_IN_BLOCK

mtemp             res             1
mBits             res             1
mindex            res             1
masterPtr         res             1
masterBasePtr     res             1
moutData          res             1
minData           res             1
                 FIT 496    
                 