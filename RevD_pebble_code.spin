'**************************************************************
'* RevD_pebble_code.spin
'* Main and top level program for operating the RevD board.
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
'* 26 Jan 2014    - version from WAIS.  Added LINE_TWO write
'*
'**************************************************************
CON ' Clock mode settings
  _CLKMODE = XTAL1 + PLL16X
  _XINFREQ = 6_250_000

  CLK_FREQ = ((_clkmode - xtal1) >> 6) * _xinfreq               ' system freq as a constant
  ONE_MS   = CLK_FREQ / 1_000                                   ' ticks in 1ms
  ONE_US   = CLK_FREQ / 1_000_000                               ' ticks in 1us
 
                                 
CON ' VERSION
  VERSION = $01 ' this is a single byte    
                               
CON ' Button time constants
  REBOOT_TIME   = 100_000_000*20
  
CON ' GPS constants including bit on I2C expander
  EXTERNAL_OSC      = 0
  PERIOD_LENGTH     = 16  ' number of seconds over which to collect oscillator data
  GPS_PPS           = 1
  GPS_RX_FROM       = 2
  GPS_TX_TO         = 3

  BYTES_IN_GPS_BUFFER = 1024

CON ' Uart command constants
    KILL                  = "K"
    SHUTDOWN_SYSTEM       = "D"
    SOFT_REBOOT_GUMSTIX   = "S"
    HARD_REBOOT_GUMSTIX   = "H"
    PUT_SYSTEM_TO_SLEEP   = "L"
    RATE                  = "R"
    QUERY                 = "Q"
    GAIN                  = "G"
    SOURCE                = "O"
    STOMP_DATA            = "W"
    LINE_TWO              = "2"

  #0, WAITING_FOR_START, WAITING_FOR_END, PROCESS_BUFFER
  
CON ' ON/OFF switch constants
  REED_SWITCH       = 15
  BUTTON_PRESS_TIME = CLK_FREQ
  BUTTON_HOLD_TIME  = CLK_FREQ*5
  NOT_PRESSED       = 1         ' Reed switch is pulled high normally 
  PRESSED           = 0         ' Pressed is actually logic 0  

  WAKEUP            = 14
CON ' Acq enumerations
 #0, TRIGGERED, CONTINUOUS, STAY_ASLEEP

CON ' ADC constants
  
  ADC_DRDYOUT =  5
  ADC_MISO    =  6 ' connected to DOUT on MAX11040K
  ADC_MOSI    =  7 ' connected to DIN  on MAX11040K
  ADC_SCLK    =  8
  ADC_CS      =  9

  
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

  INTERNAL = 0
  EXTERNAL = 1  

CON ' channel constants
  CHANNEL_A = %0001
  CHANNEL_B = %0010
  CHANNEL_C = %0100
  CHANNEL_D = %1000
  
CON ' constants for spi buffers
  BYTES_IN_SLAVE_BUFFER   = 512
  LONGS_IN_SLAVE_BUFFER   = BYTES_IN_SLAVE_BUFFER >> 2


  SLAVE_CLK           = 16 
  SLAVE_SIMO          = 17 
  SLAVE_SOMI          = 18 
  SLAVE_CS            = 19 
  SLAVE_IRQ           = 20 
  

  SLAVE_TEST          = 4 ' use spare pin as test point
'  MASTER_TEST         = 14
'  ADC_TEST
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
  SPACE             =     32
  TAB               =      9
  COLON             =     58
  COMMA             =     44
  MAX_COMMANDS      =     10
  
  DEBUG_RX_FROM     = 31
  DEBUG_TX_TO       = 30
  CL = 16  ''CS: Clear Screen
  CR = 13  ''CR: Carriage Return      

CON ' main state machine states
  #0, SLEEP, WAIT_FOR_WAKE_UP, CHANGE_ACQ_SETTINGS, ACQUISITION_MODE, SHUTDOWN

  MY_TRUE  = 1
  MY_FALSE = 0

CON ' EEPROM constants
  EEPROM_BASE      = $8000             ' set to 32,768 = 32k
  
  GPB_ADDRESS      = EEPROM_BASE +  0
  SPS_ADDRESS      = EEPROM_BASE +  4
  GAIN_A_ADDRESS   = EEPROM_BASE +  8
  GAIN_B_ADDRESS   = EEPROM_BASE + 12
  GAIN_C_ADDRESS   = EEPROM_BASE + 16
  GAIN_D_ADDRESS   = EEPROM_BASE + 20
  SOURCE_A_ADDRESS = EEPROM_BASE + 24
  SOURCE_B_ADDRESS = EEPROM_BASE + 28
  SOURCE_C_ADDRESS = EEPROM_BASE + 32
  SOURCE_D_ADDRESS = EEPROM_BASE + 36

    
DAT ' oled messages
  blankMsg   byte   "                ", 0
  oled1Msg   byte   "                ", 0
  oled2Msg   byte   "                ", 0
  offMsg     byte   "Shutting Down...", 0
  bootMsg    byte   "Booting System. ", 0
  bootedMsg  byte   "System Booted   ", 0
  sleepMsg   byte   "Sleeping System.", 0
  wakeMsg    byte   "Waking System.  ", 0
  magnetMsg  byte   "Wake with Magnet", 0
  euiMsg     byte   "SN:             ", 0
  versionMsg byte   "Version 1.4.7   ", 0

OBJ                                  
  UARTS     : "FullDuplexSerial4portPlus_0v3"       '1 COG for 3 serial ports
  NUM       : "Numbers"                             'Include Numbers object for writing numbers to debuginal
  UBX       : "ubloxInterface2"
  PEBBLE    : "BasicPebbleFunctionsStomp"                ' I put these into a seperate file to make editing easier

VAR
  byte mainCogId, serialCogId, adcCogId, slaveCogId
  byte clockSetYet, acqOn, lockType, oldDay
  byte mainState, edgeDetector
  byte inUartBuf[UART_SIZE], inUartIdx, ptrIdx, uartState, inUartPtr[MAX_COMMANDS]
  byte oLedBuf[20] ' set aside some room for the oled 

  word dacValue
  word rtcYear, rtcMonth, rtcDay, rtcDow, rtcHour, rtcMinute, rtcSecond
  word year, month, day, hour, minute, second, utcValid, gpsValid, fixStat
  ' ubxBufferAddress is the HUB memory location of gps data stored by ubx object
  long ubxBufferAddress, gpsHeader, oldRecLen
  long sampleRate, gainA, gainB, gainC, gainD, sourceA, sourceB, sourceC, sourceD, interval, recordLength ' these are longs because that's how they are stored in the EEPROM
  long rtcAddr        ' hub address of 7 bytes containing the RTC time
  long euiAddr        ' hub address of 48 bytes containing the unique ID for this device
  ' keep these in order  ymd, hms, cntsAtPPS are populated in the main cog but need to be passed down to the SPIslave cog
  long ymd, hms, cntsAtPPS1, validStatus, cntsTemp, accuracy
  ' the next 2 line refers to the HUB storage where things sit before be shifted out to the gumstix
  long gpsDataToWrite, gpsBuffer[256]
  long slaveBufCMD, slaveBuffer[LONGS_IN_SLAVE_BUFFER]       

  ' the next line refers to data read by the ADC and stored in the HUB; this is where data rest on their way to SRAM
  long fifoSem, blocksInFIFO, adcBuffer[BLOCKS_IN_BUFFER*LONGS_IN_BLOCK] ' this is a buffer that we fill from the ADC directly. Data are
                                                                         ' moved from the ADC cog to HUB then from HUB to the PASM Slave
                                                                         ' cog where they are written out to the gumstix

PUB MAIN | idx, response, displayTime, pressType, flag, oldPhsa
' the main method here is a simple state machine
' Wednesday morning in Meadville

  adcCogId    := -1
  slaveCogId  := -1
  serialCogId := -1
  oldDay      := -1
  mainCogId   := cogid
  acqon       := FALSE

  PEBBLE.INIT
  PEBBLE.GUMSTIX_ON             ' turn on gumstix so we can have a chance to program it. It will stay awake until we get GPS lock. 
  LAUNCH_SERIAL_COG             ' handle the 2 serial ports- debug and GPS
  DIRA[WAKEUP] := 1             ' this is a testing pin so use it.


  'PEBBLE.GUMSTIX_ON
  'repeat
  '  waitcnt(0)
    
  PEBBLE.LED1_ON
  PEBBLE.OLED_ON
  PAUSE_MS(100)
  PEBBLE.OLED_WRITE_LINE1(string(" Booting System "))
  
  displayTime := cnt+clkFreq<<1
  UARTS.STR(DEBUG, string(13, "$PSMSG, Booting geoPebble: "))
  waitcnt(displayTime += clkfreq<<1)
  TURN_ON_GPS
  waitcnt(displayTime += clkfreq<<1)
  GET_AND_PRINT_PARAMETERS
  waitcnt(displayTime += clkfreq<<1)
  PRINT_EUI
  waitcnt(displayTime += clkfreq<<1)
  PEBBLE.OLED_WRITE_LINE1(@versionMsg)
  waitcnt(displayTime += clkfreq<<1)

  clockSetYet := FALSE          ' when we first boot we have no idea what the lock status is/was
  PEBBLE.OLED_WRITE_LINE1(string("Getting GPS Lock"))
  waitcnt(displayTime += clkfreq<<1)
  
' setup counter B in this cog to track the number of system counts between
' the rising GPS and when we get over to read it.
' this frees us from having a cog dedicated to just watching the gpspps

  CTRB[30..26] := %01000                                'ctrb module to POS detector;                                                        
  CTRB[5..0]   := GPS_PPS                               'add frqb to phsb every clock cycle that GPS_PPS high
  FRQB:= 1                                              'this will keep track of the number of system counts
  PHSB~                                                 'from the rising edge of GPS

  GET_GPS_LOCK
  PAUSE_MS(2000)
  UPDATE_TIME_AND_DATE
  PRINT_TIME_AND_DATE
  PAUSE_MS(2000)
' setup counter A in this cog to track times when the "button" is "pressed"
' this is used to sleep and wake the system
  CTRA := %10101 << 26 + REED_SWITCH    ' counter A in LOGIC !A mode
  FRQA := 1                             ' increment phsa once for every clock cycle that REED_SWITCH is PRESSED
  PHSA := 0
  FLAG := FALSE
  
  mainState := SLEEP
  uartState := WAITING_FOR_START

' Here is the main system state machine.  Do a couple checks everytime around the loop
' then take care of business

' use the following three lines to boot into continuous mode.
'  COME_OUT_OF_SLEEP(3)
'  PHSA := 0               ' clear before moving to new state
'  mainState := ACQUISITION_MODE

  repeat
    if PHSA > REBOOT_TIME  ' has button been pressed for more than 20 (when running at 100Mhz) seconds?
      REBOOT_SYSTEM
                              
    case mainState
      SLEEP             :
        UARTS.STR(DEBUG, string(13, "$PSMSG. Sleeping system.  Use magnet to wake or wait for trigger.", 13, 13))
        repeat 3
          UARTS.STR(DEBUG, string(13, "$PSMSG. SHUTDOWN"))
        PEBBLE.OLED_WRITE_LINE1(@sleepMsg)
        PEBBLE.OLED_WRITE_LINE2(@magnetMsg)
        PAUSE_MS(5000)          ' display messages for a moment before going to sleep
        PEBBLE.SLEEP_3(cogId)
        mainState := WAIT_FOR_WAKE_UP
        PHSA := 0               ' clear before moving to new state
        FLAG := FALSE           ' anytime we sleep clear flag
        recordLength := 10       ' always set record lenght to 10 before sleeping
        
                                        
      WAIT_FOR_WAKE_UP :
        'OUTA[WAKEUP] := 1
        PAUSE_MS(100)           ' let's sleep for a bit to save power
        pressType := PEBBLE.SWITCHED_ON(interval)
        if pressType > 0
          COME_OUT_OF_SLEEP(pressType)
          PHSA := 0               ' clear before moving to new state
          mainState := ACQUISITION_MODE
        'OUTA[WAKEUP] := 0

      ACQUISITION_MODE  :
        OUTA[WAKEUP] := 1

        DO_SOMETHING_USEFUL

        ' now check to see if we should sleep or not
        if ( (PHSA > (clkfreq *5)) )        ' was it pressed for more than 5 seconds?  
          UARTS.STR(DEBUG, string(13, "Sleep case: button pressed: "))
          UARTS.DEC(DEBUG, PHSA)
          recordLength := 10
          mainState := SLEEP
          PHSA := 0               ' clear before moving to new state
            

        if  rtcSecond == recordLength  AND recordLength <> 0
          UARTS.STR(DEBUG, string(13, "Sleep case: end of record "))
          recordLength := 10
          PHSA := 0               ' clear before moving to new state
          mainState := SLEEP

        OUTA[WAKEUP] := 0

{      SHUTDOWN          :
        UARTS.STR(DEBUG, string(13, "$PSMSG, Sleeping System at:"))
        UPDATE_TIME_AND_DATE
        PRINT_TIME_AND_DATE
        PEBBLE.OLED_WRITE_LINE1(0, 1, string("Sleeping System "))
        PAUSE_MS(1000)
        PHSA := 0               ' clear before moving to new state
        FLAG := FALSE
        mainState := SLEEP
}


PUB COME_OUT_OF_SLEEP(pressType)
  PEBBLE.WAKE_UP
  LAUNCH_SERIAL_COG                     ' handle the 2 serial ports- debug, and GPS
  PAUSE_MS(200)
  'TURN_ON_GPS                           ' turn on gps as early as we can  so it has a chance to lock
  READ_RTC
  UARTS.STR(DEBUG, string(13, "$PSMSG, System awake and ready for action: "))
  UARTS.DEC(DEBUG, PHSA)
  PRINT_RTC_TIME
  case pressType
    1 : UARTS.STR(DEBUG, string(13, "Triggered mode ")) 
        recordLength := 10
        sampleRate :=  4000  'PEBBLE.READ_EEPROM_LONG(SPS_ADDRESS) 
    2 : UARTS.STR(DEBUG, string(13, "Short Button press ")) 
        recordLength := 10
        sampleRate :=  4000  'PEBBLE.READ_EEPROM_LONG(SPS_ADDRESS) 
    3 : UARTS.STR(DEBUG, string(13, "Continuous mode "))
        recordLength := 0
        sampleRate :=  1000  'PEBBLE.READ_EEPROM_LONG(SPS_ADDRESS) 
  UARTS.DEC(DEBUG, recordLength)
  PAUSE_MS(200)
  PEBBLE.OLED_WRITE_LINE1(@wakeMsg)
  PEBBLE.OLED_WRITE_LINE2(@versionMsg)
  PAUSE_MS(200)
  UPDATE_TIME_AND_DATE
  PRINT_TIME_AND_DATE
  START_ACQUISITION


PUB START_ACQUISITION  
' after waking from sleep we need to start up a number of different things.
' everything that needs to be done only once/start should go here.
' all of this happens in the top-level cog

  PEBBLE.OLED_WRITE_LINE1(string("System Awake    "))
  UARTS.STR(DEBUG, string(13, "$PSMSG, Powering Analog Section."))  
  PEBBLE.ANALOG_ON
  UARTS.STR(DEBUG, string(13, "$PSMSG, Powering GumStix."))
  PEBBLE.GUMSTIX_OFF
  PAUSE_MS(100)
  PEBBLE.GUMSTIX_ON

  dacValue := $7FFF       ' set to middle of the road value
  UARTS.STR(DEBUG, string(13, "$PSMSG, Setting DAC value: "))
  uarts.dec(DEBUG,dacValue)
  PEBBLE.WRITE_TO_DAC(dacValue)     ' put a middle of the road value in here to start with
  PAUSE_MS(100)
  
  fifoSem   := locknew  'semaphore that guards the blocksInFIFO HUB location which is shared by 2 PASM cogs
  gpsSem    := locknew
  

  gpsDataToWrite := MY_FALSE            ' remember FALSE = 0
  gpsFlagPtr     := @gpsDataToWrite  ' pass the HUB address to SPI_SLAVE cog
  gpsBasePtr     := @gpsBuffer       ' pass the HUB address to SPI_SLAVE cog
  timeDatePtr    := @ymd
  validPtr       := @validStatus
  blocksInFIFO   := 0

  
  PRINT_SYSTEM_PARAMETERS
  
  slaveCogId := cognew(@GUMSTIX_SLAVE, @fifoSem)        ' Start the SPI slave cog first and move stuff from HUB to Gumstix
  UARTS.STR(DEBUG, string(13, "$PSMSG, GUMSITX_SLAVE: "))
  UARTS.DEC(DEBUG, slaveCogId)

  PAUSE_MS(1000)
  adcCogId := SETUP_AND_LAUNCH_ADC(sampleRate, gainA, gainB, gainC, gainD, sourceA, sourceB, sourceC, sourceD)
  UARTS.STR(DEBUG, string(13, "$PSMSG, ADC_COG_ID: "))
  UARTS.DEC(DEBUG, adcCogId)

'  UARTS.STR(DEBUG, string(13, "SPI_MASTER: "))
'  UARTS.DEC(DEBUG, cognew(@SPI_MASTER, 0))' SPI MASTER code used for testing only; be sure to start the SPI slave cog first
  

PUB GET_AND_PRINT_PARAMETERS
  ' check to see if factury defaults have already been stored
  'CHECK_DEFAULT_PARAMS
  GET_PARAMETERS_FROM_EEPROM
  PRINT_SYSTEM_PARAMETERS                                                     

PUB DO_SOMETHING_USEFUL | rxByte, response, idx
' put things here that need to be done but have some timing flexibility
' all of this happens in the top level cog

  ' here we collect bytes from uart and process them.
  ' this should follow the same setup as the ublox I think
   rxByte := uarts.rxcheck(DEBUG)       ' collect byte from DEBUG port
   case uartState
      WAITING_FOR_START      :   if rxByte == "$"                     ' beginning of a new string
                                   inUartIdx := 0
                                   ptrIdx    := 0
                                   bytefill(@inUartIdx, 0, UART_SIZE) ' buffer for all incoming bytes
                                   bytefill(@inUartPtr, 0, MAX_COMMANDS) ' contains the index of each field 
                                   uartState := WAITING_FOR_END

      WAITING_FOR_END        :   if rxByte <> -1
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

     PROCESS_BUFFER          :   'OUTA[WAKEUP] := 1
                                 PROCESS_UART
                                 'OUTA[WAKEUP] := 0
                                 uartState := WAITING_FOR_START
    
     
  edgeDetector := ((edgeDetector << 1) | INA[GPS_PPS] ) & %11    
  case edgeDetector 
    %01 : ' rising edge -> grab phsb
      cntsTemp := CNT - PHSB
      UPDATE_TIME_AND_DATE                   ' KEEP IN MIND WE ALWAYS UPDATE TIME/DATE ON THE PPS following OUR MESSAGE SO WE ARE BEHIND ONE SECOND
      PRINT_TIME_AND_DATE
    %10 : ' falling edge now we can reset phsb since it won't increment until PPS goes high again
      PHSB := 0

  ' here we process all bytes sitting in the gps buffer -
  ' and we stay in this repeat until the uart buffer is empty OR we've reached the end of the packet
  repeat
    response := UBX.READ_AND_PROCESS_BYTE(FALSE)
  until response == -1 OR response == UBX#RXMRAW

  
  if response == UBX#RXMRAW                            ' have we collected all the data in this second?
    UARTS.STR(DEBUG, string(13,"$PSMSG, FIFO: "))
    UARTS.DEC(DEBUG, blocksInFIFO)                     ' if the gps buffer is empty put something into it
    if gpsDataToWrite == MY_FALSE                      ' have the data we put there previously been written?
      'longfill(@gpsBuffer, 0, 256)
      bytemove(@gpsBuffer, ubxBufferAddress, 1024)     ' copy that info into gpsBuffer
      longmove(@gpsBuffer[129], @gpsBuffer[127], 127)  ' move data over so we can insert header
      longmove(@gpsBuffer[1],   @gpsBuffer[0],   127)  ' move data over so we can insert header
      gpsBuffer[0]   := "P"  | "G" << 8 | VERSION << 16 | validStatus << 24
      gpsBuffer[128] := "P"  | "G" << 8 | VERSION << 16 | validStatus << 24
{      idx := 0
      repeat 1024
        if idx//32 == 0
          UARTS.PUTC(DEBUG, 13)
          UARTS.DEC(DEBUG, idx)
          UARTS.PUTC(DEBUG, TAB)
        'if idx == 512
        '  UARTS.PUTC(DEBUG, 13)
        'UARTS.HEX(DEBUG, BYTE[@gpsBuffer][idx++],2)
        UARTS.HEX(DEBUG, BYTE[ubxBufferAddress][idx++],2)
        UARTS.PUTC(DEBUG, SPACE)
}        

    ' buffer has new data so clear old buffer and declare buffer should be written
    UBX.CLEAR_UBX_BUFFER                                ' clear the pointer on that side so it can process next second
    gpsDataToWrite := MY_TRUE                           ' let the other cogs know there are gps data to write

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
    if clockSetYet == FALSE     ' if clock has not been set, set it
      UARTS.STR(DEBUG,string(13,"$PSMSG, Updating RTC for the first time.  From:"))
      PRINT_RTC_TIME           ' print the old time
      PEBBLE.SET_RTC_TIME(year, month, day, hour, minute, second, 0)            ' use gps time to update rtc
      clockSetYet := TRUE
      READ_RTC
      UARTS.STR(DEBUG,string(13,"$PSMSG, Updating RTC for the first time.  TO:"))
      PRINT_RTC_TIME
    if {(year<>rtcYear) OR (month<>rtcMonth) OR (day<>rtcDay) OR} (hour<>rtcHour) {OR (minute<>rtcMinute) OR (||(second-rtcSecond)>2)}         ' if the gps and RTC time are not within a couple seconds
      UARTS.STR(DEBUG,string(13,"$PSMSG, Correcting RTC time. From:"))
      PRINT_RTC_TIME
      PEBBLE.SET_RTC_TIME(year, month, day, hour, minute, second, 0)            ' use gps time to update rtc
      READ_RTC
      UARTS.STR(DEBUG,string(13,"$PSMSG, Correcting RTC time. TO:"))
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

PUB PRINT_TIME_AND_DATE
  PRINT_RTC_TIME
' print the RTC and GPS time and dates
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
  PEBBLE.OLED_WRITE_LINE1(@oLedBuf)


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

  uarts.dec(debug, cntsAtPPS1)
  uarts.putc(debug, ",")
  uarts.dec(debug, accuracy)
  uarts.putc(debug, ",")

  uarts.dec(debug, fixStat)   ' gpsFix taken from NAV-STATUS message
  uarts.putc(debug, ",")

  uarts.bin(debug, gpsValid, 3)   ' taken from NAV-TIMEGPS  
  uarts.putc(debug, ",")
  if ((gpsValid>>2 & %1) == %1)
    uarts.STR(DEBUG, string("GPS_VALID"))
  else
    uarts.STR(DEBUG, string("GPS_INVALID"))
  uarts.putc(debug, ",")

  uarts.dec(debug, UBX.get_longitude)
  uarts.putc(debug, ",")
  uarts.dec(debug, UBX.get_latitude)
  uarts.putc(debug, ",")
  uarts.dec(debug, UBX.get_elevation)
  uarts.putc(debug, ",")
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
'' store system wide values for sps, gain, and internal/external 1 for external; 0 for internal
'' set the gain and voltage trim (0)
'' set sample rate and launch ADC cog
'' method that loads some variables and launches the ADC cog

  uarts.str(debug, string(13, "Settings: "))
  uarts.dec(debug,_sps)
  uarts.putc(debug, Comma)
  uarts.dec(debug,gA)
'  uarts.dec(debug,PEBBLE.GAIN_DECODER(gainA))
  uarts.putc(debug, Comma)
  uarts.dec(debug,gB)
'  uarts.dec(debug,PEBBLE.GAIN_DECODER(gainB))
  uarts.putc(debug, Comma)
  uarts.dec(debug,gC)
'  uarts.dec(debug,PEBBLE.GAIN_DECODER(gainC))
  uarts.putc(debug, Comma)
  uarts.dec(debug,gD)
'  uarts.dec(debug,PEBBLE.GAIN_DECODER(gainD))
  uarts.putc(debug, Comma)
  uarts.dec(debug,aMux)
  uarts.putc(debug, Comma)
  uarts.dec(debug,bMux)
  uarts.putc(debug, Comma)
  uarts.dec(debug,cMux)
  uarts.putc(debug, Comma)
  uarts.dec(debug,dMux)

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

  uarts.str(debug, string(13, "Settings: "))
  uarts.dec(debug,sampleRateEnum)
  uarts.putc(debug, Comma)
  uarts.dec(debug,gainA)
  uarts.putc(debug, Comma)
  uarts.bin(debug,PEBBLE.GAIN_BINARY_VAL(gainA),4)
  uarts.putc(debug, Comma)
  uarts.dec(debug,gainB)
  uarts.putc(debug, Comma)
  uarts.bin(debug,PEBBLE.GAIN_BINARY_VAL(gainB),4)
  uarts.putc(debug, Comma)
  uarts.dec(debug,gainC)
  uarts.putc(debug, Comma)
  uarts.bin(debug,PEBBLE.GAIN_BINARY_VAL(gainC),4)
  uarts.putc(debug, Comma)
  uarts.dec(debug,gainD)
  uarts.putc(debug, Comma)
  uarts.bin(debug,PEBBLE.GAIN_BINARY_VAL(gainD),4)
   

  data1.byte[3] := "P"
  data1.byte[2] := "S"
  data1.byte[1] := VERSION
  data1.byte[0] := 0 ' change this to clock status
  
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
      mainState := SHUTDOWN

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
      'STORE_PARAMETERS_TO_EEPROM
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
      STORE_PARAMETERS_TO_EEPROM
      START_ACQUISITION
      
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
      STORE_PARAMETERS_TO_EEPROM
      START_ACQUISITION

    STOMP_DATA :        'W
      'UARTS.STR(DEBUG, string(" Write to OLED: "))
      'UARTS.STR(DEBUG, @inUartBuf[2])
      'UARTS.PUTC(DEBUG, CR)
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
        PEBBLE.OLED_WRITE_LINE2(@inUartBuf[inUartPtr[1]])' maybe we don't need the @ symbol?  
                                               '  
      'UARTS.STR(DEBUG, string(" Write to OLED: "))
      'UARTS.STR(DEBUG, @inUartBuf[2])
      'UARTS.PUTC(DEBUG, CR)
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
  
  
  PEBBLE.OLED_WRITE_LINE1(@euiMsg)
  UARTS.STR(DEBUG, string(13,"$PSEUI,"))
  UARTS.STR(DEBUG, @euiMsg[4])

PUB READ_RTC_TIME
  rtcAddr:= PEBBLE.READ_RTC_TIME

{PUB PRINT_RTC_TIME
  UARTS.STR(DEBUG, string(13,"$PSMSG,RTC Date and time: "))
  UARTS.DEC(DEBUG, BYTE[rtcAddr][6]+2000)
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][5])
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][4])
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][3])
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][2])
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][1])
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][0])
  UARTS.PUTC(DEBUG, SPACE)
}

PUB GET_PARAMETERS_FROM_EEPROM
' can't currently store parameters in EEPROM so we set them to be this everytime.  
  sampleRate :=  4000  'PEBBLE.READ_EEPROM_LONG(SPS_ADDRESS) 
  gainA      :=  1     'PEBBLE.READ_EEPROM_LONG(GAIN_A_ADDRESS)
  gainB      :=  1     'PEBBLE.READ_EEPROM_LONG(GAIN_B_ADDRESS)
  gainC      :=  1     'PEBBLE.READ_EEPROM_LONG(GAIN_C_ADDRESS)
  gainD      :=  10    'PEBBLE.READ_EEPROM_LONG(GAIN_D_ADDRESS)
  sourceA    :=  INTERNAL 'PEBBLE.READ_EEPROM_LONG(SOURCE_A_ADDRESS)
  sourceB    :=  INTERNAL 'PEBBLE.READ_EEPROM_LONG(SOURCE_B_ADDRESS)
  sourceC    :=  INTERNAL 'PEBBLE.READ_EEPROM_LONG(SOURCE_C_ADDRESS)
  sourceD    :=  INTERNAL 'PEBBLE.READ_EEPROM_LONG(SOURCE_D_ADDRESS)
  interval   :=  2        '
  recordLength := 10     ' number of seconds to record

PUB STORE_PARAMETERS_TO_EEPROM
  PEBBLE.WRITE_EEPROM_LONG(SPS_ADDRESS, sampleRate)
  PEBBLE.WRITE_EEPROM_LONG(GAIN_A_ADDRESS, gainA)
  PEBBLE.WRITE_EEPROM_LONG(GAIN_B_ADDRESS, gainB)
  PEBBLE.WRITE_EEPROM_LONG(GAIN_C_ADDRESS, gainC)
  PEBBLE.WRITE_EEPROM_LONG(GAIN_D_ADDRESS, gainD)
  PEBBLE.WRITE_EEPROM_LONG(SOURCE_A_ADDRESS, sourceA)
  PEBBLE.WRITE_EEPROM_LONG(SOURCE_B_ADDRESS, sourceB)
  PEBBLE.WRITE_EEPROM_LONG(SOURCE_C_ADDRESS, sourceC)
  PEBBLE.WRITE_EEPROM_LONG(SOURCE_D_ADDRESS, sourceD)
'  PEBBLE.WRITE_EEPROM_LONG(INTERVAL_ADDRESS, interval)  FIXME
'  PEBBLE.WRITE_EEPROM_LONG(RECORD_ADDRESS, recordLength)  FIXME
  
PUB CHECK_DEFAULT_PARAMS | eepromValue

  eepromValue := PEBBLE.READ_EEPROM_LONG(GPB_ADDRESS)
  if eepromValue == $DEAD_BEEF
    UARTS.STR(DEBUG, string(13,"$PSMSG,Factory parameters already set."))
  else
    WRITE_DEFAULT_PARAMS_TO_EEPROM

PUB WRITE_DEFAULT_PARAMS_TO_EEPROM
  sampleRate := 1000
  gainA      :=    1
  gainB      :=    1
  gainC      :=    1
  gainD      :=   10
  sourceA    := INTERNAL
  sourceB    := INTERNAL
  sourceC    := INTERNAL
  sourceD    := INTERNAL
  interval   := 2
  recordLength := 10
  STORE_PARAMETERS_TO_EEPROM
  PEBBLE.WRITE_EEPROM_LONG(GPB_ADDRESS, $DEAD_BEEF)
  UARTS.STR(DEBUG, string(13,"$PSMSG,Default paramaters stored to EEPROM."))
  PEBBLE.OLED_WRITE_LINE1(string("WROTE TO EEPROM "))
  PAUSE_MS(1500)

PUB PRINT_SYSTEM_PARAMETERS
  
  PEBBLE.OLED_WRITE_LINE1(string("Getting Params.."))
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
  if sourceA == INTERNAL
    UARTS.STR(DEBUG, string("INTERNAL"))
  if sourceA == EXTERNAL
    UARTS.STR(DEBUG, string("EXTERNAL"))
  UARTS.PUTC(DEBUG, COMMA)
  if sourceB == INTERNAL
    UARTS.STR(DEBUG, string("INTERNAL"))
  if sourceB == EXTERNAL
    UARTS.STR(DEBUG, string("EXTERNAL"))
  UARTS.PUTC(DEBUG, COMMA)
  if sourceC == INTERNAL
    UARTS.STR(DEBUG, string("INTERNAL"))
  if sourceC == EXTERNAL
    UARTS.STR(DEBUG, string("EXTERNAL"))
  UARTS.PUTC(DEBUG, COMMA)
  if sourceD == INTERNAL
    UARTS.STR(DEBUG, string("INTERNAL"))
  if sourceD == EXTERNAL
    UARTS.STR(DEBUG, string("EXTERNAL"))
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, interval)
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, recordLength)

  
PUB STOP_ADC_COG
'' method that kills off the ADC cog
  lockret(fifoSem)                               ' 
  cogstop(adcCogId)
  adcCogId := -1

PUB GET_GPS_LOCK | idx, response
  idx := 0

  repeat 
    edgeDetector := ((edgeDetector << 1) | INA[GPS_PPS] ) & %11    
    case edgeDetector 
      %01 : ' rising edge -> grab phsb
        idx++
        cntsTemp := CNT - PHSB
        UPDATE_TIME_AND_DATE
        PRINT_TIME_AND_DATE
        UARTS.PUTC(DEBUG, COMMA)
        UARTS.DEC(DEBUG, idx)
      %10 : ' falling edge now we can reset phsb since it won't increment until PPS goes high again
        PHSB := 0
      
    repeat
      response := UBX.READ_AND_PROCESS_BYTE(FALSE)
    until response == -1 OR response == UBX#RXMRAW
    
  until clockSetYet OR idx == CONSTANT(3 * 60)

  if clockSetYet == FALSE  
    UARTS.STR(DEBUG, string(13, "$PSMSG, Unable to get GPS lock or set RTC."))
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

PUB TURN_ON_GPS
  UARTS.STR(DEBUG, string(13, "$PSMSG,GPS ON"))
  PEBBLE.GPS_ON
  ubxBufferAddress := UBX.INIT(DEBUG, GPS_PORT)          

  ' we need some amount of delay between turning on the gps and speaking to it.  
  PAUSE_MS(100)
  UBX.VERSION_POLL
  UBX.TURN_OFF_NMEA
  UBX.TURN_ON_RAW
  UBX.TURN_ON_PPS

PUB REBOOT_SYSTEM
  UARTS.STR(DEBUG, string(13, "$PSMSG, Rebooting system"))
  PEBBLE.OLED_ON
  PAUSE_MS(200)
  PEBBLE.OLED_WRITE_LINE1(string("   REBOOTING    "))
  PEBBLE.OLED_WRITE_LINE1(string("    SYSTEM      "))
  PAUSE_MS(5_000)
  REBOOT

PUB TEST_MAG_ACC | idx, measureTime
  PEBBLE.MAG_ACC_ON
  PEBBLE.LSM_INIT
  
  idx := 0
  measureTime := cnt + clkfreq
  repeat
    UARTS.PUTC(DEBUG, 13)
    waitcnt(measureTime +=  CONSTANT(100_000_000/25))
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

  
PUB PAUSE_MS(mS)
  waitcnt(clkfreq/1000 * mS + cnt)

PUB PAUSE_US(uS)
  waitcnt(clkfreq/1_000_000 * uS + cnt)

PUB SPARE_PARTS
{        case PEBBLE.SWITCH_OFF(recordLength, second)
          0 :
            UARTS.STR(DEBUG, string(13, "Sleeping case: button pressed: "))
            UARTS.DEC(DEBUG, PHSA)
            recordLength := 10
            mainState := SLEEP
            PHSA := 0               ' clear before moving to new state
          1 :                                            
            UARTS.STR(DEBUG, string(13, "Sleeping case: continuous mode "))
            recordLength := 0
            mainState := ACQUISITION_MODE
          2 :
            UARTS.STR(DEBUG, string(13, "Sleeping case: end of record "))
            recordLength := 10
            PHSA := 0               ' clear before moving to new state
            mainState := SLEEP
}

  { 'use this block of code to test the speed of the OLED.
    ' looks like the spin code takes about 0.36ms for one byte
    ' and 6.63ms for 16 characters of the OLED.   }
    
{  idx := 0
  displayTime:=cnt+clkfreq
  repeat
    waitcnt(displayTime+=CONSTANT(100*ONE_MS))
    response:= strsize(NUM.TOSTR(idx,NUM#DEC))
    bytemove(@gpsMsg[16-response], NUM.TOSTR(idx,NUM#DEC), response)
    PEBBLE.OLED_WRITE_LINE1(0, 1, @gpsMsg)
    idx++
}    

{
PUB TEST_RTC | setDow, setYear, setMon, setDay, setHour, setMin, setSec

  setDow  :=    4
  setYear := 2014
  setMon  :=   11
  setDay  :=    6
  setHour :=   21
  setMin  :=   46
  setSec  :=   30

  PEBBLE.SET_RTC_TIME(setYear, setMon, setDay, setHour, setMin, setSec, setDow)
  UARTS.STR(DEBUG, string(13,"Setting RTC time: "))
  UARTS.DEC(DEBUG, setYear)
  UARTS.PUTC(DEBUG, "-")
  UARTS.DEC(DEBUG, setMon)
  UARTS.PUTC(DEBUG, "-")
  UARTS.DEC(DEBUG, setDay)
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, setHour)
  UARTS.PUTC(DEBUG, COLON)
  UARTS.DEC(DEBUG, setMin)
  UARTS.PUTC(DEBUG, COLON)
  UARTS.DEC(DEBUG, setSec)
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, setDow)

  repeat
    UARTS.STR(DEBUG, string(13, "Going to sleep.",13))
    PAUSE_MS(500)
    PEBBLE.SLEEP(mainCogId)
    PEBBLE.WAIT_FOR_TIME(2)
    LAUNCH_SERIAL_COG                     ' handle the 2 serial ports- debug, and GPS
    PAUSE_MS(200)
    UARTS.STR(DEBUG, string(13, "System booted and ready for action.",13))
    PEBBLE.OLED_WRITE_LINE1(0, 1, @bootMsg)
    repeat 
      rtcAddr := PEBBLE.READ_RTC_TIME
      UARTS.STR(DEBUG, string(13,"Date and time: "))
      UARTS.DEC(DEBUG, BYTE[rtcAddr][6]+2000)
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][5])
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][4])
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][3])
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][2])
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][1])
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][0])
      UARTS.PUTC(DEBUG, SPACE)
      PAUSE_MS(500)
    until BYTE[rtcAddr][0] == 15    
  

}

{      WAIT_FOR_WAKE_UP  :
        PEBBLE.WAIT_FOR_WAKE_UP               ' this calls forces system to block until button pressed
        LAUNCH_SERIAL_COG                     ' handle the 2 serial ports- debug, and GPS
        PAUSE_MS(200)
        UARTS.STR(DEBUG, string(13, "System booted and ready for action.",13))
        PEBBLE.OLED_WRITE_LINE1(0, 1, @bootMsg)
        START_ACQUISITION
        mainState := ACQUISITION_MODE
        PHSA      := 0
}


DAT ' day in each month for leap and non-leap year
  leapTable   byte  0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30  ' leap year
  noLeapTable byte  0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30  ' NOT leap year

DAT 'GUMSTIX_SLAVE
'' this PASM code should watch the shared HUB memory ADC buffers and write each buffer full
'' as soon as it is available IF the gumstix is ready

GUMSTIX_SLAVE         org            0
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
          rdlong  stemp,         gpsFlagPtr         wz   ' stemp=1 means data to write
   if_nz  call    #WRITE_GPS_DATA                        ' Z=1 if stemp=0
          rdlong  fifoCnt,       fifoCntPtr         wz   ' Z=1 if fifoCnt=0
   if_z   jmp     #:waitForData                          ' if Z==0 jump back up and wait some more


' at this point we've got at least one block we can send
          mov     sampsInBlock,  #LONGS_IN_BLOCK wz    ' number of times we should loop per block; set Z=0
          
          waitpeq scsMask,       scsMask                ' wait for CS to be HIGH
          muxz    outa,          irqMask                ' lower controller FLAG to indicate buffer ready to be clocked out
          waitpne scsMask,       scsMask                ' wait for CS to be LOW

          muxnz   outa,          irqMask                ' raise controller FLAG once CS goes low

:slaveLoop
          'now get ready to shift out all 128-longs
        
          rdlong  soutData,      slavePtr              ' get long from HUB
          'mov     soutData,      sixsix                ' put 0x5858_5858 into outbyte
          call    #WRITE_LONG
          add     slavePtr,      #4                    ' increment bufferPtr
          
          djnz    sampsInBlock,  #:slaveLoop
          call    #UPDATE_FIFO_CNT
          
          djnz    bufferCnt,     #:waitForData   wz     ' jump back around; when count=0 set Z=1
          

          jmp     #:topOfBuffer                        ' jump up and wait for next buffer



{******************************** WRITE_LONG *******************************************************
Write 32 bits to gumstix.  
***************************************************************************************************}
WRITE_LONG
          mov     sBits,        #32        wz   ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero

:wslaveLoop
          waitpne ssclkMask,    ssclkMask        ' wait for CLK to go low
          rol     soutData,     #1         wc    ' get highest bit of outData
          muxc    outa,         somiMask         ' put bit onto MISO
          waitpeq ssclkMask,    ssclkMask        ' wait for CLK to go high
          djnz    sBits,        #:wslaveLoop     ' Decrement loop count.

          muxz    outa,         somiMask         ' make sure MISO is low on exit

WRITE_LONG_ret    ret


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
          cmp     szero,          #0           wz         ' set Z=1 
'          xor     outa,          stestMask

          lockset slaveSem        wc
   if_c   jmp     #:waitForSlaveSem                     ' if adcSem = 0 loop 

          rdlong  stemp,         fifoCntPtr              ' copy number of blocks in buffer so we can add to it
          sub     stemp,         #1                      ' increment count
          wrlong  stemp,         fifoCntPtr              ' write the new value back to HUB
          lockclr slaveSem                              ' clear adcSem for others to use
'          xor     outa,          stestMask

UPDATE_FIFO_CNT_ret  ret

{******************************** WRITE_GPS_DATA*****************************************************
This method pulls GPS dat from HUB and spits it to the shared SPI_BUS
***************************************************************************************************}
WRITE_GPS_DATA
' we've got 1024 bytes or 256 longs of gps data to send.  So we'll do it in two parts.  One then the next
          mov     gpsPtr,        gpsBasePtr             ' we always begin at the beginning of the GPS buffer
          mov     gpsBufCnt,     #2                     ' we need to send 2 buffers

:gpsLoop2
          mov     gpsInBlock,    #LONGS_IN_BLOCK wz     ' number of times we should loop per block; set Z=0
          
          waitpeq scsMask,       scsMask                ' wait for CS to be HIGH
          muxz    outa,          irqMask                ' lower controller FLAG to indicate buffer ready to be clocked out
          waitpne scsMask,       scsMask                ' wait for CS to be LOW

          muxnz   outa,          irqMask                ' raise controller FLAG once CS goes low

:gpsLoop1
          'now shift out all 128-longs in current block

          rdlong  gpsLong,       gpsPtr                 ' get long from HUB
          'mov     gpsLong,       sixsix
          call    #WRITE_GPS_LONG
          add     gpsPtr,        #4                     ' increment bufferPtr
          
          djnz    gpsInBlock,    #:gpsLoop1
          
          djnz    gpsBufCnt,     #:gpsLoop2

          wrbyte  sfalse,            gpsFlagPtr             ' indicate the GPS buffer if empty

WRITE_GPS_DATA_ret  ret

{******************************** WRITE_GPS_LONG *******************************************************
Write 32 bits to gumstix.  
***************************************************************************************************}
WRITE_GPS_LONG
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


{**************************************************************************************************
' Pin Masks
***************************************************************************************************}

scsMask      long            |< SLAVE_CS               '
ssclkMask    long            |< SLAVE_CLK
somiMask     long            |< SLAVE_SOMI
simoMask     long            |< SLAVE_SIMO
irqMask      long            |< SLAVE_IRQ 
'stestMask    long            |< SLAVE_TEST

fiveEight    long            $5858_5858
sixsix       long            $6666_6666
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
          'mov     data,         #$A
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          ' read N/S connected to ORANGE/YELLOW
          call    #READ_SAMPLE  ' sample is now in data
          'mov     data,         #$B
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



          waitpeq mzero,         mirqMask                 ' wait for IRQ to go LOW
          call    #M_LOWER_CS                               ' lower CS line

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
'          test    mmisoMask,    ina        wc   ' C=INA[misoMask]
'          rcl     minData,      #1              ' shift C left into data
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
msclkMask    long            |< SLAVE_CLK
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
                 